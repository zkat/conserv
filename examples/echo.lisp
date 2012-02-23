(defpackage #:conserv-echo
  (:use :cl :conserv))
(in-package #:conserv-echo)

;; 'Bare-minimum' echo server
(defclass echo-test () ())

(defmethod on-socket-data ((driver echo-test) data)
  (write-sequence data *socket*))

(defun basic-echo ()
  (with-event-loop ()
    (server-listen (make-instance 'echo-test)
                   :host "127.0.0.1"
                   :port 1337)))

;; Local IPC sockets!
(defun ipc-socket-test ()
  (let ((driver (make-instance 'echo-test)))
    (with-event-loop ()
      (server-listen driver :host #p"/tmp/test.sock")
      (socket-connect driver #p"/tmp/test.sock" :wait nil))))

;; Scalability test
;; To take this much higher than this number, you'll likely need to mess with file descriptor
;; limits.
;; See http://www.xenoclast.org/doc/benchmark/HTTP-benchmarking-HOWTO/node7.html#SECTION00072100000000000000
(defparameter *max-echo-clients* 10)
(defvar *num-clients*)
(defclass scalability (echo-test) ())
(defmethod on-server-connection ((driver scalability) client)
  ;; This will set the clients into an echo loop with the server.
  (format client "~&Welcome to ~A. Please echo this back.~%" *server*)
  (incf *num-clients*))
(defvar *total-exchanges*)
(defmethod on-socket-data :after ((driver scalability) data)
  (declare (ignore data))
  (incf *total-exchanges*))
(defmethod on-server-close ((driver scalability))
  (print *num-clients*)
  (print *total-exchanges*))
(defun super-echo ()
  (let ((driver (make-instance 'scalability))
        (*total-exchanges* 0)
        (*num-clients* 0))
    (with-event-loop ()
      (server-listen driver :port 1338)
      (loop repeat *max-echo-clients* do
           (socket-connect driver "127.0.0.1" :port 1338 :wait nil))
      (exit-event-loop :delay 20))))
