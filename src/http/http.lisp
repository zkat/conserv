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
   (connections ((server a))))
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
   (write-headers ((reply a)))
   (headers-written-p ((reply a)) :accessorp t)
   (keep-alive-p ((reply a)) :accessorp t)
   (http-server ((reply a)))
   (socket ((reply a))))
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

(defclass reply (trivial-gray-stream-mixin
                 fundamental-binary-output-stream
                 fundamental-character-output-stream)
  ((headers :accessor reply-headers :initform nil)
   (status :accessor reply-status :initform 200)
   (socket :reader reply-socket :initarg :socket)
   (headers-written-p :accessor reply-headers-written-p :initform nil)
   (http-server :reader reply-http-server :initarg :http-server)
   (keep-alive-p :accessor reply-keep-alive-p :initform t)))

(defmethod close ((reply reply) &key abort)
  (unless (or (reply-headers-written-p reply) abort)
    (write-headers reply))
  (if (reply-keep-alive-p reply)
      ;; TODO - reuse request/reply objects by reinitializing?
      ;;        (use shared-initialize with t for slots)
      (new-request (reply-http-server reply)
                   (reply-socket reply))
      (close (reply-socket reply) :abort abort)))

(defun reply-headers* ()
  (reply-headers *reply*))
(defun (setf reply-headers*) (new-headers)
  (setf (reply-headers *reply*) new-headers))

(defun ensure-headers-written (reply)
  (unless (reply-headers-written-p reply)
    (write-headers reply)))

(defmethod stream-write-sequence ((reply reply) sequence start end &key)
  (ensure-headers-written reply)
  (stream-write-sequence (reply-socket reply) sequence start end))
(defmethod stream-line-column ((reply reply))
  (ensure-headers-written reply)
  (stream-line-column (reply-socket reply)))
(defmethod stream-write-char ((reply reply) char)
  (ensure-headers-written reply)
  (stream-write-char (reply-socket reply) char))
(defmethod stream-write-byte ((reply reply) byte)
  (ensure-headers-written reply)
  (stream-write-byte (reply-socket reply) byte))
(defmethod stream-write-string ((reply reply) string &optional start end)
  (ensure-headers-written reply)
  (stream-write-string (reply-socket reply) string start end))

(defmethod write-headers ((reply reply))
  (cond ((reply-headers-written-p reply)
         (warn "Headers already written."))
        (t
         (let ((socket (reply-socket reply)))
           (format socket "HTTP/1.1 ~A" (reply-status reply))
           (write-sequence +crlf+ socket)
           (loop for (name . value) in (reply-headers reply)
              do (format socket "~:(~A~): ~A" name value)
                (write-sequence +crlf+ socket))
           (write-sequence +crlf+ socket)
           (setf (reply-headers-written-p reply) t)))))

;;; Server
(defclass http-server-driver ()
  ((driver :initarg :driver :accessor http-server-driver)
   (connections :initform (make-hash-table) :accessor http-server-connections)))

(defun new-request (driver socket)
  (setf (gethash socket (http-server-connections driver))
        (cons (make-instance 'request
                             :socket socket
                             :external-format :utf-8 #+nil(server-external-format-in *server*))
              (make-instance 'reply
                             :socket socket
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
          (on-request-data user-driver data)
          (parse-headers user-driver (socket-server socket) data)))))

(defparameter +100-continue+ #.(babel:string-to-octets (format nil "HTTP/1.1 100 Continue~a~a" +crlf+ +crlf+)))
(defun parse-headers (driver server data)
  (multiple-value-bind (donep parser rest)
      (feed-parser (request-request-parser *request*) data)
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
            (on-request-data driver data))
          (parse-headers driver server rest)))))

(defmethod on-socket-close ((driver http-server-driver))
  (remhash *socket* (http-server-connections driver)))

(defun http-listen (driver &key
                    (host iolib:+ipv4-unspecified+)
                    (port 8080)
                    (external-format-in *default-external-format*)
                    (external-format-out *default-external-format*)
                    binaryp)
  (server-listen (make-instance 'http-server-driver :driver driver)
                 :host host
                 :port port
                 :external-format-in external-format-in
                 :external-format-out external-format-out
                 :binaryp binaryp))
