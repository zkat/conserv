(cl:defpackage #:conserv-hello-http
  (:use :cl :conserv.http))
(cl:in-package #:conserv-hello-http)

(defclass hello-http () ())

(defmethod on-http-request ((driver hello-http))
  (setf (reply-headers*) '(("Content-type" . "text/html")))
  (format *reply* "<h1>Hello, world!</h1>~%")
  (close *reply*))

;; Possible target:
#+nil
(defmethod on-http-request ((driver hello-http))
  (setf (reply-header* :content-type) "text/plain")
  (format *reply* "Hello, world!~%")
  (close *reply*))

(defun start ()
  (conserv:with-event-loop ()
    (http-listen (make-instance 'hello-http) :port 8888)))
