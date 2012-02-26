(in-package #:conserv.http)

(defvar *http-server*)
(defprotocol http-server-event-driver (a)
  ((listen ((driver a))
    :default-form nil
    :documentation "Event called when *HTTP-SERVER* has just started listening for connections.")
   (request ((driver a))
    :default-form nil
    :documentation "Event called when an incoming HTTP request has been made. *REQUEST* and *REPLY*
                    are both available, at this point.")
   (connection ((driver a))
    :default-form nil
    :documentation "Event called when a user-agent first connects to the server. *SOCKET* contains
                    the incoming socket connection. *REQUEST* and *REPLY* are NOT bound at this
                    point.")
   (close ((driver a))
    :default-form nil
    :documentation "Event called when *HTTP-SERVER* has closed.")
   (error ((driver a) error)
    :default-form nil
    :documentation "Event called when an error has happened during processing."))
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
                    and their associated requests/replies.")
   (external-format-in ((server a))
    :accessorp t
    :documentation "Default external-format for requests.")
   (external-format-out ((server a))
    :accessorp t
    :documentation "Default external-format for replies.")
   (closed-p ((server a))
    :accessorp t
    :documentation "Internal -- whether the server has been closed."))
  (:prefix http-server-))

(defmethod close ((req request) &key abort &aux (socket (request-socket req)))
  (unless (eq :closed (request-state req))
    (when abort
      (setf (request-keep-alive-p req) nil))
    (setf (request-state req) :closed)
    (on-request-close (request-driver req))
    (if (request-keep-alive-p *request*)
        (new-request (request-http-server req) socket)
        (close socket :abort abort))))

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
                                   :external-format (http-server-external-format-in server)))
         (*reply* (make-instance 'reply
                               :socket socket
                               :driver driver
                               :request *request*
                               :external-format (http-server-external-format-out server)
                               :http-server server)))
    (setf (gethash socket (http-server-connections server))
          (cons *request* *reply*))))

(defmethod on-server-connection ((server http-server) socket)
  (let ((*http-server* server)
        (*socket* socket))
    (on-http-connection (http-server-driver server)))
  (new-request server socket))

(defmethod on-socket-data ((server http-server) data
                           &aux
                           (driver (http-server-driver server))
                           (socket *socket*))
  (destructuring-bind (req . rep)
      (gethash socket (http-server-connections server))
    (let ((*request* req)
          (*reply* rep)
          (*http-server* server))
      (case (request-state req)
        (:headers
         (parse-headers server driver data))
        (:body
         (on-request-data driver (if-let (format (request-external-format req))
                                   (babel:octets-to-string data :encoding format)
                                   data)))))))

(defparameter +100-continue+ #.(babel:string-to-octets (format nil "HTTP/1.1 100 Continue~a~a" +crlf-ascii+ +crlf-ascii+)))
(defun parse-headers (server driver data)
  (multiple-value-bind (donep parser rest)
      (feed-parser (request-request-parser *request*) (babel:octets-to-string data :encoding :ascii))
    (when donep
      (setf (request-method *request*) (request-parser-method parser)
            (request-http-version *request*) (request-parser-http-version parser)
            (request-url *request*) (request-parser-url parser)
            (request-headers *request*) (request-parser-headers parser))
      ;; Connection: keep-alive is required in HTTP/1.0, but the default in HTTP/1.1
      ;; TODO - Probably shouldn't even support this for 0.9. This code also treats 0.9 as 1.1...
      (when (string-equal "1.0" (request-http-version *request*))
        (setf (request-keep-alive-p *request*) nil)))
    (when rest
      (if donep
          (cond ((or
                  (member :upgrade (request-headers *request*)
                          :test #'string-equal
                          :key #'car)
                  (string-equal "CONNECT" (request-method *request*)))
                 (upgrade-request *request* *socket* server driver rest))
                (t
                 (when (member '(:expect . "100-continue") (request-headers *request*)
                               :test #'equalp)
                   #+nil(on-request-continue driver)
                   (write-continue *reply*))
                 (when (member '(:connection . "keep-alive") (request-headers *request*)
                               :test #'equalp)
                   (setf (request-keep-alive-p *request*) t))
                 (when (member '(:connection . "close") (request-headers *request*)
                               :test #'equalp)
                   (setf (request-keep-alive-p *request*) nil))
                 (setf (request-state *request*) :body)
                 (on-http-request driver)
                 (on-request-data driver
                                  (let ((rest-octets (babel:string-to-octets rest :encoding :ascii)))
                                    (if-let (format (request-external-format *request*))
                                      (babel:octets-to-string rest-octets :encoding format)
                                      rest-octets)))))
          (parse-headers server driver rest)))))

(defmethod on-request-continue ((driver t))
  (write-continue *reply*))

(defun write-continue (reply)
  (write-sequence +100-continue+ (reply-socket reply)))

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
  (loop for (request . reply) in (hash-table-values (http-server-connections server))
     do (setf (request-keep-alive-p request) nil)
       (close reply :abort abort))
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
