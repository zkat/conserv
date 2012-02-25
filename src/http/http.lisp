(in-package #:conserv.http)

;;;
;;; Protocols
;;;
(defvar *http-server*)
(defvar *request*)
(defvar *reply*)
(defprotocol http-server-event-driver (a)
  ((listen ((driver a))
    :default-form nil)
   (request ((driver a))
    :default-form nil)
   #+nil(connection ((driver a) server socket))
   (close ((driver a))
    :default-form nil)
   (error ((driver a) error)
    :default-form nil))
  (:prefix on-http-))

(defprotocol http-server (a)
  ((driver ((server a))
    :accessorp t)
   (connections ((server a)))
   (external-format-in ((server a))
    :accessorp t)
   (external-format-out ((server a))
    :accessorp t))
  (:prefix http-server-))

(defprotocol request-event-driver (a)
  ((data ((driver a) data)
    :default-form nil)
   (continue ((driver a))
    :default-form nil)
   (upgrade ((driver a))
    :default-form (close *request* :abort t))
   (close ((driver a))
    :default-form nil))
  (:prefix on-request-))

(defprotocol request (a)
  ((method ((request a)) :accessorp t)
   (url ((request a)) :accessorp t)
   (headers ((request a)) :accessorp t)
   #+nil(trailers ((request a)))
   (http-version ((request a)) :accessorp t)
   (external-format ((request a)) :accessorp t)
   (request-parser ((request a)))
   (socket ((request a))))
  (:prefix request-))

(defprotocol reply-event-driver (a)
  ((close ((driver a))
    :default-form nil))
  (:prefix on-reply-))

(defprotocol reply (a)
  ((status ((reply a)) :accessorp t)
   (headers ((reply a)) :accessorp t)
   (header-bytes ((reply a)) :accessorp t)
   (write-headers ((reply a)))
   (headers-written-p ((reply a)) :accessorp t)
   (keep-alive-p ((reply a)) :accessorp t)
   (chunked-p ((reply a)) :accessorp t)
   (http-server ((reply a)))
   (external-format ((request a)) :accessorp t)
   (socket ((reply a)))
   #+nil(trailers ((reply a)) :accessorp t))
  (:prefix reply-))

(defclass request ()
  ((method :initform nil :accessor request-method)
   (url :accessor request-url)
   (headers :accessor request-headers)
   (http-version :accessor request-http-version)
   (external-format :initarg :external-format :accessor request-external-format)
   (request-parser :initform (make-request-parser) :reader request-request-parser)
   (socket :initarg :socket :reader request-socket)))

(defmethod close ((req request) &key abort)
  (close (request-socket req) :abort abort))

;; TODO - the reply implementation needs to go below all the header parsing later anyway.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun format-header (stream name value)
    (format stream "~:(~A~): ~A~A" name value +crlf-ascii+)))

(defclass reply (trivial-gray-stream-mixin
                 fundamental-binary-output-stream
                 fundamental-character-output-stream)
  ((headers :accessor reply-headers :initform '((:transfer-encoding . "chunked")))
   (header-bytes :accessor reply-header-bytes
                 :initform #.(babel:concatenate-strings-to-octets
                              :ascii
                              (format-header nil :transfer-encoding "chunked")
                              +crlf-ascii+))
   (status :accessor reply-status :initform 200)
   (socket :reader reply-socket :initarg :socket)
   (headers-written-p :accessor reply-headers-written-p :initform nil)
   (http-server :reader reply-http-server :initarg :http-server)
   (keep-alive-p :accessor reply-keep-alive-p :initform t)
   (chunkedp :accessor reply-chunked-p :initform t)
   (external-format :accessor reply-external-format :initarg :external-format)))

(defparameter +0crlfcrlf+ #.(make-array 5
                                        :element-type '(unsigned-byte 8)
                                        :initial-contents #(48 13 10 13 10)))

(defmethod close ((reply reply) &key abort)
  (unless (or (reply-headers-written-p reply) abort)
    (write-headers reply))
  (when (reply-chunked-p reply)
    (write-sequence +0crlfcrlf+ (reply-socket reply)))
  (if (reply-keep-alive-p reply)
      (new-request (reply-http-server reply)
                   (reply-socket reply))
      (close (reply-socket reply) :abort abort)))

;;;
;;; Headers
;;;
(defmethod (setf reply-headers) :after (new-headers (reply reply))
  (when (member :content-length new-headers :key #'car :test #'string-equal)
    (setf (reply-chunked-p reply) nil)))

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

;; NOTE - the reason this isn't just (setf reply-headers) is because some implementations expand
;;        setf functions into something like (let ((val ,value)) (funcall #'(setf foo) val)), which
;;        will always pass 'val as the literal argument to the compiler macro. :(
(defun set-headers (reply &rest headers &aux (alist (plist-alist headers)))
  (setf (reply-header-bytes reply) (babel:string-to-octets
                                      (calculate-header-string alist)
                                      :encoding :ascii)
        (reply-headers reply) alist))

#+nil
(defun add-headers (reply &rest headers &aux (alist (plist-alist headers)))
  ;; TODO - in order for this to work, we can't append the final crlf in
  ;;        calculate-header-string. The logic behind automatically inserting :transfer-encoding
  ;;        needs to be changed, too.
  (setf (reply-header-bytes reply) (concatenate '(vector (unsigned-byte 8))
                                                (reply-header-bytes reply)
                                                (babel:string-to-octets
                                                 (calculate-header-string alist)
                                                 :encoding :ascii)))
  (appendf (reply-headers reply) alist))

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

(define-compiler-macro set-headers (reply &rest headers &environment env)
  (multiple-value-bind (compiled literal dynamic)
      (process-literal-headers headers env)
    (let ((dynamics-var (gensym "DYNAMIC-HEADERS"))
          (literals-var (gensym "LITERAL-HEADERS"))
          (reply-var (gensym "REPLY")))
      `(let ((,dynamics-var (plist-alist (list ,@dynamic)))
             (,literals-var (plist-alist (list ,@literal)))
             (,reply-var ,reply))
         (setf (reply-header-bytes ,reply-var) ,(cond ((and literal dynamic)
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
               (reply-headers ,reply-var) (nconc ,literals-var ,dynamics-var))))))

;;; Reply Gray Streams
(defun ensure-headers-written (reply)
  (unless (reply-headers-written-p reply)
    (write-headers reply)))

(defparameter +crlf-octets+ #.(make-array 2
                                          :element-type '(unsigned-byte 8)
                                          :initial-contents #(13 10)))

(defmethod stream-write-sequence ((reply reply) sequence start end &key)
  (ensure-headers-written reply)
  (let ((socket (reply-socket reply))
        (chunkedp (reply-chunked-p reply))
        (output (etypecase sequence
                  (string
                   (babel:string-to-octets sequence :encoding (reply-external-format reply)))
                  (sequence sequence))))
    (when chunkedp
      (write-sequence (babel:string-to-octets
                       (format nil "~x~a" (- (or end (length output)) (or start 0)) +crlf-ascii+)
                       :encoding :ascii)
                      socket))
    (write-sequence output socket :start start :end end)
    (when chunkedp
      (write-sequence +crlf-octets+ socket))))
(defmethod stream-line-column ((reply reply))
  (stream-line-column (reply-socket reply)))
(defmethod stream-write-char ((reply reply) char)
  (write-sequence (string char) reply))
(defmethod stream-write-byte ((reply reply) byte)
  (write-sequence (make-array 1 :element-type '(unsigned-byte 8) :initial-element byte) reply))
(defmethod stream-write-string ((reply reply) string &optional start end)
  (stream-write-sequence reply string start end))

(defparameter +http-1.1+ (babel:string-to-octets "HTTP/1.1 " :encoding :ascii))

(defmethod write-headers ((reply reply))
  (cond ((reply-headers-written-p reply)
         (warn "Headers already written."))
        (t
         (let ((socket (reply-socket reply)))
           (write-sequence +http-1.1+ socket)
           (write-sequence (babel:string-to-octets (princ-to-string (reply-status reply))
                                                   :encoding :ascii)
                           socket)
           (write-sequence +crlf-octets+ socket)
           (write-sequence (reply-header-bytes reply)
                           (reply-socket reply))
           (setf (reply-headers-written-p reply) t)))))

;;; Server
(defclass http-server-driver ()
  ((driver :initarg :driver :accessor http-server-driver)
   (external-format-in :initarg :external-format-in :accessor http-server-external-format-in)
   (external-format-out :initarg :external-format-out :accessor http-server-external-format-out)
   (connections :initform (make-hash-table) :accessor http-server-connections)))

(defun new-request (driver socket)
  (setf (gethash socket (http-server-connections driver))
        (cons (make-instance 'request
                             :socket socket
                             :external-format (http-server-external-format-in driver))
              (make-instance 'reply
                             :socket socket
                             :external-format (http-server-external-format-out driver)
                             :http-server driver))))

(defmethod on-server-connection ((driver http-server-driver) socket)
  (new-request driver socket))

(defmethod on-socket-data ((driver http-server-driver) data
                           &aux
                           (user-driver (http-server-driver driver))
                           (socket *socket*))
  (destructuring-bind (req . rep)
      (gethash socket (http-server-connections driver))
    (let ((*request* req)
          (*reply* rep)
          (*http-server* driver))
      (if (request-method req)
          (on-request-data user-driver (if-let (format (request-external-format req))
                                         (babel:octets-to-string data :encoding format)
                                         data))
          (parse-headers user-driver (socket-server socket) data)))))

(defparameter +100-continue+ #.(babel:string-to-octets (format nil "HTTP/1.1 100 Continue~a~a" +crlf-ascii+ +crlf-ascii+)))
(defun parse-headers (driver server data)
  (multiple-value-bind (donep parser rest)
      (feed-parser (request-request-parser *request*) (babel:octets-to-string data :encoding :ascii))
    (when donep
      (setf (request-method *request*) (request-parser-method parser)
            (request-http-version *request*) (request-parser-http-version parser)
            (request-url *request*) (request-parser-url parser)
            (request-headers *request*) (request-parser-headers parser))
      (when (string-equal "1.0" (request-http-version *request*))
        (setf (reply-keep-alive-p *reply*) nil)))
    (when rest
      (if donep
          (progn
            (when (member '(:expect . "100-continue") (request-headers *request*)
                          :test #'equalp)
              ;; TODO - perhaps we shouldn't write this when the client's HTTP version is <1.1?
              (write-sequence +100-continue+ (reply-socket *reply*)))
            (when (member '(:connection . "keep-alive") (request-headers *request*)
                          :test #'equalp)
              (setf (reply-keep-alive-p *reply*) t))
            (when (member '(:connection . "close") (request-headers *request*)
                          :test #'equalp)
              (setf (reply-keep-alive-p *reply*) nil))
            (when (member :upgrade (request-headers *request*)
                          :test #'eq
                          :key #'car)
              (on-request-upgrade driver))
            (on-http-request driver)
            (on-request-data driver (let ((rest-octets (babel:string-to-octets rest :encoding :ascii)))
                                      (if-let (format (request-external-format *request*))
                                        (babel:octets-to-string rest-octets :encoding format)
                                        rest-octets))))
          (parse-headers driver server rest)))))

(defmethod on-socket-close ((driver http-server-driver))
  (remhash *socket* (http-server-connections driver)))

(defun http-listen (driver &key
                    (host iolib:+ipv4-unspecified+)
                    (port 8080)
                    (external-format-in *default-external-format*)
                    (external-format-out *default-external-format*))
  (server-listen (make-instance 'http-server-driver
                                :driver driver
                                :external-format-in external-format-in
                                :external-format-out external-format-out)
                 :host host
                 :port port
                 :external-format-in nil
                 :external-format-out nil))
