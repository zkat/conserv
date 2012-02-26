(cl:defpackage #:conserv-hello-http
  (:use :cl :conserv :conserv.http))
(cl:in-package #:conserv-hello-http)

(defclass hello-http () ())

(defmethod on-http-request ((driver hello-http))
  (set-headers *reply* :content-type "text/html")
  (format *reply* "<h1>Hello, world!</h1>")
  (close *reply*))

(defun start ()
  (with-event-loop ()
    (http-listen (make-instance 'hello-http) :port 8888)))

(defclass hello-websocket () ())
(defmethod on-socket-data ((driver hello-websocket) data)
  (write-sequence data *socket*))

(defmethod on-request-upgrade ((driver hello-http) data)
  (setf (socket-driver *socket*) (make-instance 'hello-websocket)
        (socket-external-format-out *socket*) :ascii)
  (let ((crlf (coerce '(#\return #\linefeed) 'string)))
    (format *socket* "HTTP/1.1 101 Web Socket Protocol Handshake~A" crlf)
    (format *socket* "Upgrade: WebSocket~A" crlf)
    (format *socket* "Connection: Upgrade~A~A" crlf crlf))
  (on-socket-data driver data))
