(defpackage #:conserv-echo
  (:use :cl :conserv))
(in-package #:conserv-echo)

;; Mostly-bare-minimum echo server
(defclass echo-test () ())
(defmethod on-tcp-server-connection ((driver echo-test) server client)
  (format t "~&Client connected: ~S~%" client)
  (format client "~&Hello. Welcome to the server (~S).~%" server))
(defmethod on-socket-data ((driver echo-test) socket data)
  (write-sequence data socket))
(defun basic-echo ()
  (let ((driver (make-instance 'echo-test)))
    (with-event-loop ()
      (server-listen (make-server driver) :host "127.0.0.1" :port 1337))))

;; Local IPC sockets!
(defun ipc-socket-test ()
  (let ((driver (make-instance 'echo-test)))
    (with-event-loop ()
      (server-listen (make-server driver) :host #p"/tmp/test.sock")
      (socket-connect (make-socket driver) #p"/tmp/test.sock" :wait nil))))

;; Scalability test
;; To take this much higher than this number, you'll likely need to mess with file descriptor
;; limits.
;; See http://www.xenoclast.org/doc/benchmark/HTTP-benchmarking-HOWTO/node7.html#SECTION00072100000000000000
(defparameter *max-echo-clients*  500)
(defun super-echo ()
  (let ((server (make-server (make-instance 'echo-test))))
    (with-event-loop ()
      (server-listen server :port 1338)
      (loop repeat *max-echo-clients* do
           (socket-connect (make-socket (make-instance 'echo-test)) "127.0.0.1" :port 1338 :wait nil)))))
