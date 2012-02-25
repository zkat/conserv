(cl:defpackage #:conserv-hello-http
  (:use :cl :conserv.http))
(cl:in-package #:conserv-hello-http)

(defclass hello-http () ())

(defmethod on-http-request ((driver hello-http))
  #+nil(let ((content-length (babel:string-size-in-octets "<h1>Hello, world!</h1>" :encoding :utf-8)))
    (set-headers *reply*
                 :content-type "text/html"
                 :content-length content-length)
    )
  (set-headers *reply* :content-type "text/html")
  (format *reply* "<h1>Hello, world!</h1>")
  (close *reply*))

(defun start ()
  (conserv:with-event-loop ()
    (http-listen (make-instance 'hello-http) :port 8888)))
