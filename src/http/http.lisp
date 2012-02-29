(in-package #:conserv.http)

(defvar *http-server*)
(setf (documentation '*http-server* 'variable)
      "During execution of `http-server-event-driver` events, this variable is bound to the
       associated `http-server` object. This variable is unbound outside of the scope of
       `http-server-event-driver` events.")
(defprotocol http-server-event-driver (a)
  ((listen ((driver a))
    :default-form nil
    :documentation "Event called when `*http-server*` has just started listening for connections.")
   (request ((driver a))
    :default-form nil
    :documentation "Event called when an incoming HTTP request has been made. `*request*` is
                    available at this point.")
   (connection ((driver a))
    :default-form nil
    :documentation "Event called when a user-agent first connects to the
                    server. `conserv.tcp:*socket*` contains the incoming socket
                    connection. `*request*` is NOT bound at this point.")
   (close ((driver a))
    :default-form nil
    :documentation "Event called after the `*http-server*` and all its active connections have been
                    closed.")
   (error ((driver a) error)
    :default-form nil
    :documentation "Event called when an error has happened during processing. `error` is the actual error condition signaled."))
  (:prefix on-http-))

(defprotocol http-server (a)
  ((driver ((server a))
    :accessorp t
    :documentation "User driver for http server event dispatch.")
   (server ((server a))
    :accessorp t
    :documentation "Conserv TCP server instance that this http server is running on top of.")
   (connections ((server a))
    :documentation "Internal -- hash table holding the current socket connections for this server,
                    and their associated requests.")
   (external-format-in ((server a))
    :accessorp t
    :documentation "Default external-format-in for requests.")
   (external-format-out ((server a))
    :accessorp t
    :documentation "Default external-format-out for requests.")
   (closed-p ((server a))
    :accessorp t
    :documentation "Internal -- whether the server has been closed."))
  (:prefix http-server-))

(defmethod close ((request request) &key abort)
  "Ends the HTTP request associated with this `request`. If the request headers have not been written
and `abort` is `nil`, the request headers are written to the user agent before closing the request. If
`abort` is true, the associated request and its socket are immediately closed and nothing else is
written to the user agent, and queued output will not be flushed before closing"
  (unless (request-closed-p request)
    (let ((socket (request-socket request)))
      (unless (or (request-headers-written-p request) abort)
        (write-headers request))
      (when (request-chunked-p request)
        (write-sequence #.(make-array 5 :element-type '(unsigned-byte 8)
                                      :initial-contents #(48 13 10 13 10))
                        socket))
      (when (member '(:connection . "close") (request-headers-out request)
                    :test 'equalp)
        (setf (request-keep-alive-p request) nil))
      (setf (request-closed-p request) t
            (request-state request) :closed)
      (let ((*request* request))
        (on-request-close (request-driver request)))
      (if (request-keep-alive-p request)
          (new-request (request-http-server request) socket)
          (close socket :abort abort)))))

(defclass http-server ()
  ((driver :initarg :driver :accessor http-server-driver)
   (server :accessor http-server-server)
   (external-format-in :initarg :external-format-in :accessor http-server-external-format-in)
   (external-format-out :initarg :external-format-out :accessor http-server-external-format-out)
   (connections :initform (make-hash-table) :accessor http-server-connections)
   (closedp :initform nil :accessor http-server-closed-p)))

(defun new-request (server socket)
  (let* ((driver (http-server-driver server))
         (*http-server* server)
         (*request* (make-instance 'request
                                   :socket socket
                                   :driver driver
                                   :http-server server
                                   :external-format-in (http-server-external-format-in server)
                                   :external-format-out (http-server-external-format-out server))))
    (setf (gethash socket (http-server-connections server))
          *request*)))

(defmethod on-server-connection ((server http-server) socket)
  (let ((*http-server* server)
        (*socket* socket))
    (on-http-connection (http-server-driver server)))
  (new-request server socket))

(defmethod on-socket-data ((server http-server) data
                           &aux
                           (driver (http-server-driver server))
                           (socket *socket*))
  (let ((*request* (gethash socket (http-server-connections server)))
        (*http-server* server))
    (case (request-state *request*)
      (:headers
       (parse-headers server driver data))
      (:body
       (on-request-data driver (if-let (format (request-external-format-in *request*))
                                 (babel:octets-to-string data :encoding format)
                                 data))))))

(defparameter +100-continue+ #.(babel:string-to-octets (format nil "HTTP/1.1 100 Continue~a~a" +crlf-ascii+ +crlf-ascii+)))
(defun parse-headers (server driver data)
  (multiple-value-bind (donep parser rest)
      (feed-parser (request-request-parser *request*) (babel:octets-to-string data :encoding :ascii))
    (when donep
      (setf (request-method *request*) (request-parser-method parser)
            (request-http-version *request*) (request-parser-http-version parser)
            (request-url *request*) (request-parser-url parser)
            (request-headers-in *request*) (request-parser-headers parser))
      ;; Connection: keep-alive is required in HTTP/1.0, but the default in HTTP/1.1
      ;; TODO - Probably shouldn't even support this for 0.9. This code also treats 0.9 as 1.1...
      (when (string-equal "1.0" (request-http-version *request*))
        (setf (request-keep-alive-p *request*) nil)))
    (when rest
      (if donep
          (cond ((or
                  (member :upgrade (request-headers-in *request*)
                          :test #'string-equal
                          :key #'car)
                  (string-equal "CONNECT" (request-method *request*)))
                 (upgrade-request *request* *socket* server driver rest))
                (t
                 (when (member '(:expect . "100-continue") (request-headers-in *request*)
                               :test #'equalp)
                   #+nil(on-request-continue driver)
                   (write-continue *request*))
                 (when (member '(:connection . "keep-alive") (request-headers-in *request*)
                               :test #'equalp)
                   (setf (request-keep-alive-p *request*) t))
                 (when (member '(:connection . "close") (request-headers-in *request*)
                               :test #'equalp)
                   (setf (request-keep-alive-p *request*) nil))
                 (setf (request-state *request*) :body)
                 (on-http-request driver)
                 (on-request-data driver
                                  (let ((rest-octets (babel:string-to-octets rest :encoding :ascii)))
                                    (if-let (format (request-external-format-in *request*))
                                      (babel:octets-to-string rest-octets :encoding format)
                                      rest-octets)))))
          (parse-headers server driver rest)))))

(defmethod on-request-continue ((driver t))
  (write-continue *request*))

(defun write-continue (request)
  (write-sequence +100-continue+ (request-socket request)))

(defun upgrade-request (request socket server driver rest)
  ;; HACK - This is terrible. Because CLOSE will close the underlying socket if request-keep-alive-p
  ;;        is NIL, we force it to true before the close, so all the cleanup that needs to be done
  ;;        gets done, but we still have the open socket..
  (setf (request-keep-alive-p request) t)
  (close request)
  (unregister-http-socket server socket)
  (setf (socket-driver socket) nil)
  (on-request-upgrade driver (babel:string-to-octets rest :encoding :ascii)))

(defun unregister-http-socket (server socket)
  (remhash socket (http-server-connections server)))

(defmethod on-socket-close ((server http-server))
  (unregister-http-socket server *socket*))
(defmethod on-server-listen ((server http-server))
  (let ((*http-server* server))
    (on-http-listen (http-server-driver server))))
(defmethod on-server-close ((server http-server))
  (unless (http-server-closed-p server)
    (close server :abort t)))
#+nil(defmethod on-socket-error ((server http-server) error)
  (on-http-error (http-server-driver server) error))

(defmethod close ((server http-server) &key abort)
  "Implementation of `cl:close` for `http-server`. Shuts down all active connections and then shuts
down the http server itself, making the port available for listening again. If `abort` is true, all
sockets are immediately shut down without waiting for any pre-shutdown cleanup to complete."
  (loop for request in (hash-table-values (http-server-connections server))
     do (setf (request-keep-alive-p request) nil)
       (close request :abort abort))
  (clrhash (http-server-connections server))
  (setf (http-server-closed-p server) t)
  (close (http-server-server server) :abort abort)
  (let ((*http-server* server))
    (on-http-close (http-server-driver server))))

(defun http-listen (driver &key
                    (host iolib:+ipv4-unspecified+)
                    (port 8080)
                    (external-format-in *default-external-format*)
                    (external-format-out *default-external-format*))
  "Starts an HTTP server listening on `host` and `port`. If successful, returns an `http-server`
object.

  * `driver` -- An instance of a driver class. Used to dispatch `http-server-event-driver` events.
  * `host` -- Either a string representing a local IP address to bind to, or a pathname to use as a unix socket.
  * `port` -- Port to listen on. Should not be provided if `host` is a unix socket.
  * `external-format-in` -- Default `external-format-in` for requests.
  * `external-format-out` -- Default `external-format-out` for requests."

  (let ((http-server (make-instance 'http-server
                                    :driver driver
                                    :external-format-in external-format-in
                                    :external-format-out external-format-out)))
    (setf (http-server-server http-server)
          (server-listen http-server
                         :host host
                         :port port
                         :external-format-in nil
                         :external-format-out nil))
    http-server))
