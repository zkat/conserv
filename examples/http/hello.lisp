(cl:defpackage #:conserv.examples.http.hello
  (:use #:cl)
  ;; Note -- :conserv and :conserv.http should be safe to :use. Manual imports are here purely for
  ;;         educational purposes (and personal taste)
  (:import-from #:conserv
                #:with-event-loop)
  (:import-from #:conserv.http
                #:*request*
                #:http-listen
                #:set-headers
                #:on-http-request))
(cl:in-package #:conserv.examples.http.hello)

(defclass hello () ())

(defmethod on-http-request ((driver hello))
  (set-headers *request* :content-type "text/html")
  (format *request* "<h1>Hello, world!</h1>")
  (close *request*))

(defun start ()
  (with-event-loop ()
    (http-listen (make-instance 'hello) :port 8888)))