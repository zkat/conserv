(cl:defpackage #:conserv-hello-http
  (:use :cl :conserv))
(cl:in-package #:conserv-hello-http)

(defclass hello-http () ())

(defmethod on-http-request ((driver hello-http) server request reply)
  (declare (ignore server request))
  (setf (reply-headers reply) '(("Content-type" . "text/plain")))
  (format reply "Hello, world!~%")
  (close reply))

;; Possible target:
#+nil
(defmethod on-http-request ((driver hello-http))
  (setf (reply-header* :content-type) "text/plain")
  (format *reply* "Hello, world!~%")
  (close *reply*))

(defun start ()
  (with-event-loop ()
    (http-listen (make-instance 'hello-http) :port 8080)))

