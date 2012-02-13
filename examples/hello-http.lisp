(cl:defpackage #:conserv-hello-http
  (:use :cl :conserv))
(cl:in-package #:conserv-hello-http)

(defclass hello-http () ())

(defmethod on-http-request ((driver hello-http) server request response)
  (declare (ignore server request))
  (setf (response-header response :content-type) "text/plain")
  (format response "Hello, world!~%")
  (close response))

;; Possible target:
#+nil
(defmethod on-http-request ((driver hello-http))
  (setf (reply-header* :content-type) "text/plain")
  (format *reply* "Hello, world!~%")
  (close *reply*))

(defun start ()
  (with-event-loop ()
    (server-listen (make-http-server (make-instance 'hello-http)) :port 8080)))

