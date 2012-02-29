(cl:defpackage #:conserv.examples.http.upgrade
  (:use #:cl)
  (:import-from #:conserv
                #:with-event-loop)
  (:import-from #:conserv.http
                #:*request*
                #:http-listen
                #:set-headers
                #:on-http-request
                #:on-request-upgrade)
  ;; Upgrading a connection gives us the raw *socket*, so we need to pull those symbols in.
  (:import-from #:conserv.tcp
                #:*socket*
                #:on-socket-data
                #:socket-driver
                #:socket-external-format-out))
(cl:in-package #:conserv.examples.http.upgrade)

(defclass http () ())

(defmethod on-http-request ((driver http))
  ;; Our HTTP server is only really meant to handle websocket requests, so if we don't get an
  ;; upgrade (which prevents on-http-request from firing), respond to the client with some generic
  ;; message and end the request.
  (set-headers *request* :content-type "text/plain")
  (format *request* "Upgrade or die.")
  (close *request*))

(defclass websocket () ())
(defmethod on-socket-data ((driver websocket) data)
  ;; Our websocket handler will simply be an echo server.
  (write-sequence data *socket*))

;; To trigger the upgrade, simply make an HTTP request with either the CONNECT request method, or an
;; Upgrade: headers from your favorite http client.
(defmethod on-request-upgrade ((driver http) data)
  (setf (socket-driver *socket*) (make-instance 'websocket)
        (socket-external-format-out *socket*) :ascii)
  (let ((crlf (coerce '(#\return #\linefeed) 'string)))
    (format *socket* "HTTP/1.1 101 Switching Protocols~A" crlf)
    (format *socket* "Upgrade: WebSocket~A" crlf)
    (format *socket* "Connection: Upgrade~A~A" crlf crlf))
  (setf (socket-external-format-out *socket*) nil)
  (on-socket-data driver data))

(defun start ()
  (with-event-loop ()
    (http-listen (make-instance 'http) :port 8888)))
