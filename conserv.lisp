;;;; conserv.lisp

(in-package #:conserv)

;;; "conserv" goes here. Hacks and glory await!

;; ;; TCP server events
(defmacro defevent (name lambda-list &rest options)
  `(defgeneric ,name ,lambda-list ,@options
               (:method ,lambda-list (declare (ignore ,@lambda-list)) nil)))
(defevent on-listen (driver))
(defevent on-close (driver))
(defevent on-error (driver error))
(defevent on-connect (driver client))
(defevent on-disconnect (driver client))
(defevent on-data (driver client data))
(defevent on-timeout (driver client))
(defevent on-client-error (driver client error))
(defevent on-drain (driver client))

(defparameter *max-buffer-size* 16384)
(defstruct (tcp-client
             (:constructor make-client (socket remote-name remote-port
                                               &optional (buffer-size *max-buffer-size*))))
  socket
  remote-name
  remote-port
  (read-buffer (make-array buffer-size :element-type '(unsigned-byte 8)))
  (write-buffer (make-array buffer-size :element-type '(unsigned-byte 8))))

(defclass tcp-server ()
  ((driver :initarg :driver)))
(defun make-tcp-server (driver)
  (make-instance 'tcp-server :driver driver))

(defun start-listener (server &key (host iolib:+ipv4-loopback+) (port 1337))
  (let ((event-base (make-instance 'iolib:event-base))
        (connections (make-hash-table :test 'equalp))
        (driver (slot-value server 'driver)))
    (unwind-protect
         (iolib:with-open-socket (server-sock :connect :passive
                                              :address-family :internet
                                              :type :stream
                                              :ipv6 nil
                                              :local-host host
                                              :local-port port
                                              :reuse-address t
                                              :backlog 5)
           (on-listen driver)
           (iolib:set-io-handler event-base (iolib:socket-os-fd server-sock)
                                 :read (lambda (&rest ig)
                                         (declare (ignore ig))
                                         (when-let (client-sock (iolib:accept-connection server-sock))
                                           (multiple-value-bind (name port)
                                               (iolib:remote-name client-sock)
                                             (let ((client (make-client client-sock name port)))
                                               (setf (gethash (cons name port) connections)
                                                     client)
                                               (on-connect driver client)
                                               (start-reads driver event-base client))))))
           (handler-case
               (iolib:event-dispatch event-base)
             ((or iolib:socket-connection-reset-error iolib:hangup end-of-file) (e)
               (on-error driver e)))
           (on-close driver))
      (maphash-values (lambda (client) (close (tcp-client-socket client)))
                      connections)
      (close event-base))))

(defun start-reads (driver event-base client)
  (iolib:set-io-handler event-base (iolib:socket-os-fd (tcp-client-socket client))
                        :read (lambda (&rest ig)
                                (declare (ignore ig))
                                (handler-case
                                    (let* ((buffer (tcp-client-read-buffer client))
                                           (bytes-read
                                            (nth-value
                                             1 (iolib:receive-from (tcp-client-socket client) :buffer buffer))))
                                      (when (zerop bytes-read)
                                        (error 'end-of-file))
                                      (on-data driver client (subseq buffer 0 bytes-read)))
                                  ((or iolib:socket-connection-reset-error end-of-file) (e)
                                    (on-client-error driver client e))))))

(defun stop-reads (driver event-base client)
  (declare (ignore driver))
  (iolib:remove-fd-handlers event-base (iolib:socket-os-fd (tcp-client-socket client)) :read t))

(defun start-writes (driver event-base client &aux (buffer (tcp-client-write-buffer client)))
  ;; TODO - this blows and doesn't work. Make it not blow.
  (iolib:set-io-handler event-base (iolib:socket-os-fd (tcp-client-socket client))
                        :write
                        (lambda (&rest ig)
                          (declare (ignore ig))
                          (handler-case
                              (let ((bytes-written (iolib:send-to (tcp-client-socket client) buffer)))
                                (when t ; (buffer-drained-p buffer)
                                  (on-drain driver client)))
                            ((or iolib:socket-connection-reset-error isys:ewouldblock isys:epipe) (e)
                              (on-client-error driver client e))))))

;; TCP server test
(defclass echo () ())
(defmethod on-connect ((driver echo) client)
  (format t "~&Client connected."))
(defmethod on-data ((driver echo) client data)
  (format t "~&Data length: ~S~%" (length data)))
(defmethod on-disconnect ((driver echo) client)
  (format t "~&Client disconnected: ~S~%" client))
#+nil(start-listener (make-tcp-server (make-instance 'echo)) :host "127.0.0.1" :port 1337 :external-format :utf8)
