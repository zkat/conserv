(in-package #:conserv)

(defvar *event-base* nil)
(defvar *active-sockets*)
(defun call-with-event-loop (loop-init-function)
  (let ((*event-base* (make-instance 'iolib:event-base))
        (event-base-initialized-p nil)
        (*active-sockets* (make-hash-table)))
    (unwind-protect
         (progn
           (funcall loop-init-function)
           (setf event-base-initialized-p t)
           (iolib:event-dispatch *event-base*))
      (maphash-values (rcurry #'close :abort t) *active-sockets*)
      (when event-base-initialized-p (close *event-base*)))))

(defmacro with-event-loop (() &body body)
  `(call-with-event-loop (lambda () ,@body)))

(defun register-socket (socket)
  (setf (gethash socket *active-sockets*) socket))

(defun unregister-socket (socket)
  (remhash socket *active-sockets*))

(defun add-timer (function timeout &key one-shot)
  (iolib:add-timer *event-base* function timeout :one-shot one-shot))

(defun remove-timer (timer)
  (iolib:remove-timer *event-base* timer))
