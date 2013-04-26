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
                #:*tcp-client*
                #:on-tcp-client-data
                #:tcp-client-driver
                #:tcp-client-external-format-out))
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
(defmethod on-tcp-client-data ((driver websocket) data)
  ;; Our websocket handler will simply be an echo server.
  (write-sequence data *tcp-client*))

;; To trigger the upgrade, simply make an HTTP request with either the CONNECT request method, or an
;; Upgrade: headers from your favorite http client.
(defmethod on-request-upgrade ((driver http) data)
  (setf (tcp-client-driver *tcp-client*) (make-instance 'websocket)
        (tcp-client-external-format-out *tcp-client*) :ascii)
  (let ((crlf (coerce '(#\return #\linefeed) 'string)))
    (format *tcp-client* "HTTP/1.1 101 Switching Protocols~A" crlf)
    (format *tcp-client* "Upgrade: WebSocket~A" crlf)
    (format *tcp-client* "Connection: Upgrade~A~A" crlf crlf))
  (setf (tcp-client-external-format-out *tcp-client*) nil)
  (on-tcp-client-data driver data))

(defun start ()
  (with-event-loop ()
    (http-listen (make-instance 'http) :port 8888)))
