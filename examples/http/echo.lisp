(cl:defpackage #:conserv.examples.http.echo
  (:use #:cl)
  (:import-from #:conserv
                #:with-event-loop)
  (:import-from #:conserv.http
                #:*request*
                #:http-listen
                #:set-headers
                #:on-http-request))
(cl:in-package #:conserv.examples.http.echo)

(defclass echo () ())

;; Users have control over when the request is completed, and are able to keep a request connection
;; alive and stream data in and out of it. In this case, we set up an echo server over HTTP, with
;; the incoming request body being echoed back in the response.
(defmethod on-http-request ((driver echo))
  (set-headers *request* :content-type "text/plain")
  (format *request* "Welcome to the HTTP echo server."))

(defmethod on-request-data ((driver echo) data)
  (write-sequence data *request*))

(defun start ()
  (with-event-loop ()
    (http-listen (make-instance 'echo) :port 8888 :external-format-in nil)))
