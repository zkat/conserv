(in-package #:conserv.http)

(defvar *request*)
(setf (documentation '*request* 'variable)
      "During execution of `request-event-driver` events, this variable is bound to the associated
`request` object. This variable is unbound outside of the scope of `request-event-driver` events,
unless otherwise noted.")

(defprotocol request-event-driver (a)
  ((data ((driver a) data)
    :default-form nil
    :documentation "Event called when `*request*` has received data from the user-agent. `data` will
                    be automatically encoded according to `(request-external-format-in *request*)`.")
   (continue ((driver a))
    :documentation "Event called when the user-agent sent the `Expect: 100-continue` header. By
                    default, this event calls `write-continue` on `*request*`.")
   (upgrade ((driver a) data)
    :default-form (close *socket* :abort t)
    :documentation "Event called when `*request*` has received either an `Upgrade:` header, or a
                    `CONNECT` method in an HTTP request. When this happens, `*request*`'s socket is
                    deregistered with the HTTP server. The socket is available through the
                    `conserv.tcp:*socket*` variable. `data` is the first chunk of non-header data
                    received by the socket, and is either `nil`, or an array of `(unsigned-byte 8)`,
                    regardless of the `external-format` assigned to `*request*`. By default, this
                    event immediately shuts down the socket.")
   (close ((driver a))
    :default-form nil
    :documentation "Event called after `*request*` has been closed."))
  (:prefix on-request-))

(defprotocol request (a)
  ((driver ((request a))
    :documentation "Driver instance for dispatching REQUEST's events.")
   (socket ((request a))
    :documentation "TCP socket associated with this request.")
   (http-server ((request a))
    :documentation "HTTP server associated with this request.")
   (keep-alive-p ((request a))
    :accessorp t
    :documentation "Controls whether the underlying connection should be closed after this request
                    is completed. For HTTP/1.1 clients, this defaults to true. For other clients,
                    the default is nil. This value should not be set directly by users, instead, set
                    the outgoing Connection: close header.")
   (closed-p ((request a))
    :documentation "Is REQUEST closed?"
    :accessorp t)
   (state ((request a))
          :accessorp t
          :documentation "Internal -- keeps track of the internal state of the request, :headers, :body,
                    or :closed")
   ;; Incoming data
   (request-parser ((request a))
    :documentation "Internal -- Request parser object.")
   (method ((request a))
    :accessorp t
    :documentation "Request method as a string (e.g. `\"GET\"`, `\"POST\"`, `\"CONNECT\"`)")
   (url ((request a))
    :accessorp t
    :documentation "Request URL as a string (e.g. `\"/\"`, `\"/blog/post?number=1\"`, `\"http://example.com/something\"`)")
   (headers-in ((request a))
    :accessorp t
    :documentation "An alist of (name . value) pairs representing the incoming headers sent by the
                    client. Header names are :keywords. All values are strings.")
                    #+nil(trailers ((request a)))
   (http-version ((request a))
    :accessorp t
    :documentation "HTTP version as a string. (e.g. `\"1.1\"`, `\"1.0\"`)")
   (external-format-in ((request a))
    :accessorp t
    :documentation "External format used to encode incoming data. If `nil`, no encoding will be
                    done, and `on-request-data` will receive arrays of `(unsigned-byte 8)` as its
                    `data`.")
   ;; Outgoing data
   (response-status ((request a))
    :accessorp t
    :documentation "HTTP status code for this request's response. Can be either a simple integer,
                    such as `404`, or a cons of `(code . message-string)`, such as `(404 . \"Not
                    Found\")`. If no message is present, only the code is sent to
                    clients. Returns `(200 . \"OK\")` by default.")
   (headers-out ((request a))
    :accessorp t
    :documentation "An alist containing the outgoing headers. Header names can either be strings or
                    `:keywords`. Values can be any printable lisp value. These headers should be set
                    using the `set-headers` function.

                    Certain headers trigger special behavior:

                    * `(:connection . \"close\")` -- Disables keep-alive (if active), and requests
                      that the underlying socket connection be closed after the current request
                      completes.
                    * `(:connection . \"keep-alive\")` -- Enables keep-alive (if not already active).
                    * `(:content-length . integer)` -- Disables chunked encoding of outgoing data.
                    * `(:transfer-encoding . \"chunked\")` -- The default. Enables chunked encoding of outgoing data.")
   (header-bytes ((request a))
    :accessorp t
    :documentation "Internal -- ASCII-encoded bytes representing the outgoing headers.")
   (headers-written-p ((request a))
    :accessorp t
    :documentation "Returns true when the outgoing headers have been written to the client.")
   (chunked-p ((request a))
    :accessorp t
    :documentation "Internal -- whether the request's body output should be chunk-encoded. Do not
                    set this directly. By default, all responses use chunked encoding unless the
                    Content-length header is provided.")
   (external-format-out ((request a))
    :accessorp t
    :documentation "The external format used to encode outgoing strings. If `nil`, attempting to
                    write a string or character to the request will signal an error --
                    only arrays of `(unsigned-byte 8)` will be allowed.")
)
  (:prefix request-))

;;;
;;; Host/port info
;;;
(defun request-remote-name (request)
  "Name of the remote host `request` is coming from."
  (socket-remote-name (request-socket request)))
(defun request-remote-port (request)
  "Remote port that `request` is coming from."
  (socket-remote-port (request-socket request)))

;;;
;;; Headers
;;;
(defparameter +http-1.1+ (babel:string-to-octets "HTTP/1.1 " :encoding :ascii))
(defparameter +crlf-octets+ #.(make-array 2
                                          :element-type '(unsigned-byte 8)
                                          :initial-contents #(13 10)))

(defun format-header (stream name value)
  (format stream "~:(~A~): ~A~A" name value +crlf-ascii+))

(defun calculate-header-string (header-alist)
  (with-output-to-string (s)
    (loop for (name . value) in header-alist
       with transfer-encoding-p
       with content-length-p
       do (format-header s name value)
         (when (eq :content-length name)
           (setf content-length-p t))
         (when (eq :transfer-encoding name)
           (setf transfer-encoding-p t))
       finally (progn
                 (unless (or transfer-encoding-p
                             content-length-p)
                   (format-header s :transfer-encoding "chunked"))
                 (write-sequence +crlf-ascii+ s)))))

;; NOTE - the reason this isn't just (setf request-headers-out) is because some implementations expand
;;        setf functions into something like (let ((val ,value)) (funcall #'(setf foo) val)), which
;;        will always pass 'val as the literal argument to the compiler macro. :(
(defun set-headers (request &rest headers &aux (alist (plist-alist headers)))
  "Sets `request`'s outgoing headers. `headers` is a plist of `header-name` `header-value` pairs. On
output, header values will be converted to strings with `*print-escape*` and `*print-readably*` set
to `nil` (like `princ` or `format`'s `~A` directive). Has no effect if `write-headers` has already
been called."
  (setf (request-header-bytes request) (babel:string-to-octets
                                      (calculate-header-string alist)
                                      :encoding :ascii)
        (request-headers-out request) alist))

#+nil
(defun add-headers (request &rest headers &aux (alist (plist-alist headers)))
  ;; TODO - in order for this to work, we can't append the final crlf in
  ;;        calculate-header-string. The logic behind automatically inserting :transfer-encoding
  ;;        needs to be changed, too.
  (setf (request-header-bytes request) (concatenate '(vector (unsigned-byte 8))
                                                (request-header-bytes request)
                                                (babel:string-to-octets
                                                 (calculate-header-string alist)
                                                 :encoding :ascii)))
  (appendf (request-headers-out request) alist))

(defun write-headers (request)
  "Begins http response output, including writing the response line with the response status and all
the headers that have been set for `request`. This function should only be called once per `request`. It
is implicitly called as soon as any attempt is made to write to `request` through other methods."
  (cond ((request-headers-written-p request)
         (warn "Headers already written."))
        (t
         (let ((socket (request-socket request)))
           (write-sequence +http-1.1+ socket)
           (write-sequence (babel:string-to-octets (let ((status (request-response-status request)))
                                                     (etypecase status
                                                       (integer (princ-to-string status))
                                                       (cons (format nil "~A ~A"
                                                                     (car status)
                                                                     (cdr status)))))
                                                   :encoding :ascii)
                           socket)
           (write-sequence +crlf-octets+ socket)
           (write-sequence (request-header-bytes request) socket)
           (setf (request-headers-written-p request) t)))))

(defun ensure-headers-written (request)
  (unless (request-headers-written-p request)
    (write-headers request)))

;;; Optimization
(defun process-literal-headers (headers env)
  (loop
     for (name value) on headers by #'cddr
     if (and (or (symbolp name) (stringp  name))
             (constantp value env))
     collect (format-header nil name value) into compiled
     and collect name into literal and collect value into literal
     else
     collect name into dynamic and collect value into dynamic
     finally (return (values (when compiled
                               (apply #'babel:concatenate-strings-to-octets :ascii compiled))
                             literal
                             dynamic))))

(define-compiler-macro set-headers (request &rest headers &environment env)
  (multiple-value-bind (compiled literal dynamic)
      (process-literal-headers headers env)
    (let ((dynamics-var (gensym "DYNAMIC-HEADERS"))
          (literals-var (gensym "LITERAL-HEADERS"))
          (request-var (gensym "REQUEST")))
      `(let ((,dynamics-var (plist-alist (list ,@dynamic)))
             (,literals-var (plist-alist (list ,@literal)))
             (,request-var ,request))
         (setf (request-header-bytes ,request-var)
               ,(cond ((and literal dynamic)
                       `(concatenate '(vector (unsigned-byte 8))
                                     ,compiled
                                     (babel:string-to-octets
                                      (calculate-header-string ,dynamics-var)
                                      :encoding :ascii)))
                      (compiled
                       (concatenate '(vector (unsigned-byte 8))
                                    compiled
                                    (babel:string-to-octets
                                     (calculate-header-string nil)
                                     :encoding :ascii)))
                      (dynamic `(babel:string-to-octets
                                 (calculate-header-string ,dynamics-var)
                                 :encoding :ascii)))
               ;; NOTE - nconc is *probably* okay here. literals and dynamics have already been
               ;;        used above...
               (request-headers-out ,request-var)
               (nconc ,literals-var ,dynamics-var))))))

(defclass request (trivial-gray-stream-mixin
                   fundamental-binary-output-stream
                   fundamental-character-output-stream)
  ((driver :reader request-driver :initarg :driver)
   (socket :initarg :socket :reader request-socket)
   (http-server :reader request-http-server :initarg :http-server)
   (keep-alive-p :accessor request-keep-alive-p :initform t)
   (closedp :accessor request-closed-p :initform nil)
   (state :accessor request-state :initform :headers)
   (request-parser :initform (make-request-parser) :reader request-request-parser)
   (method :initform nil :accessor request-method)
   (url :accessor request-url)
   (headers-in :accessor request-headers-in)
   (http-version :accessor request-http-version)
   (external-format-in :initarg :external-format-in :accessor request-external-format-in)
   (response-status :initform '(200 . "OK") :accessor request-response-status)
   (headers-out :accessor request-headers-out :initform '((:transfer-encoding . "chunked")))
   (header-bytes :accessor request-header-bytes
                 :initform #.(babel:concatenate-strings-to-octets
                              :ascii "Transfer-Encoding: chunked"
                              +crlf-ascii+))
   (headers-written-p :accessor request-headers-written-p :initform nil)
   (chunkedp :accessor request-chunked-p :initform t)
   (external-format-out :accessor request-external-format-out :initarg :external-format-out)))

(defmethod (setf request-headers-out) :after (new-headers (request request))
  (when (member :content-length new-headers :key #'car :test #'string-equal)
    (setf (request-chunked-p request) nil)))

;;; Request Gray Streams
(defmethod stream-write-sequence ((request request) sequence start end &key)
  (ensure-headers-written request)
  (let ((socket (request-socket request))
        (chunkedp (request-chunked-p request))
        (output (etypecase sequence
                  (string
                   (babel:string-to-octets sequence :encoding (request-external-format-out request)))
                  (sequence sequence))))
    (when chunkedp
      (write-sequence (babel:string-to-octets
                       (format nil "~x~a" (- (or end (length output)) (or start 0)) +crlf-ascii+)
                       :encoding :ascii)
                      socket))
    (write-sequence output socket :start start :end end)
    (when chunkedp
      (write-sequence +crlf-octets+ socket))))
(defmethod stream-line-column ((request request))
  (stream-line-column (request-socket request)))
(defmethod stream-write-char ((request request) char)
  (write-sequence (string char) request))
(defmethod stream-write-byte ((request request) byte)
  (write-sequence (make-array 1 :element-type '(unsigned-byte 8) :initial-element byte) request))
(defmethod stream-write-string ((request request) string &optional start end)
  (stream-write-sequence request string start end))
