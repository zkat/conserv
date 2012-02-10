;;;; conserv.lisp

(in-package #:conserv)

;;; "conserv" goes here. Hacks and glory await!

;; ;; TCP server events
(defmacro defevent (name lambda-list &rest options)
  `(defgeneric ,name ,lambda-list ,@options
               (:method ,lambda-list (declare (ignore ,@lambda-list)) nil)))
(defevent on-listen (server))
(defevent on-close (server))
(defevent on-error (server error))
(defevent on-connect (server client))
(defevent on-disconnect (server client))
(defevent on-data (server client data))
(defevent on-timeout (server client))
(defevent on-client-error (server client error))
(defevent on-drain (server client))

(defparameter *max-buffer-size* 16384)
(defstruct (tcp-client
             (:constructor make-client (socket remote-name remote-port
                                               &optional (buffer-size *max-buffer-size*))))
  socket
  remote-name
  remote-port
  (read-buffer (make-array buffer-size :element-type '(unsigned-byte 8)))
  (write-buffer (make-array buffer-size :element-type '(unsigned-byte 8))))

(defun start-listener (server &key (host iolib:+ipv4-loopback+) (port 1337))
  (let ((event-base (make-instance 'iolib:event-base))
        (connections (make-hash-table :test 'equalp)))
    (unwind-protect
         (iolib:with-open-socket (server-sock :connect :passive
                                              :address-family :internet
                                              :type :stream
                                              :ipv6 nil
                                              :local-host host
                                              :local-port port
                                              :reuse-address t
                                              :backlog 5)
           (on-listen server)
           (iolib:set-io-handler event-base (iolib:socket-os-fd server-sock)
                                 :read (lambda (&rest ig)
                                         (declare (ignore ig))
                                         (when-let (client-sock (iolib:accept-connection server-sock))
                                           (multiple-value-bind (name port)
                                               (iolib:remote-name client-sock)
                                             (let ((client (make-client client-sock name port)))
                                               (setf (gethash (cons name port) connections)
                                                     client)
                                               (on-connect server client)
                                               (start-reads server event-base client))))))
           (handler-case
               (iolib:event-dispatch event-base)
             ((or iolib:socket-connection-reset-error iolib:hangup end-of-file) (e)
               (on-error server e)))
           (on-close server))
      (maphash-values (lambda (client) (close (tcp-client-socket client)))
                      connections)
      (close event-base))))

(defun start-reads (server event-base client &aux (buffer (tcp-client-read-buffer client)))
  ;; TODO - We need to compensate for partial buffer reads, even though this passes basic tests.
  (iolib:set-io-handler event-base (iolib:socket-os-fd (tcp-client-socket client))
                        :read (lambda (&rest ig)
                                (declare (ignore ig))
                                (handler-case
                                    (multiple-value-bind (buffer bytes-read)
                                        (iolib:receive-from (tcp-client-socket client)
                                                            :buffer buffer
                                                            :end (length buffer))
                                      (when (zerop bytes-read)
                                        (error 'end-of-file))
                                      (on-data server client (map-into (make-array bytes-read) #'identity buffer)))
                                  ((or iolib:socket-connection-reset-error end-of-file) (e)
                                    (on-client-error server client e))))))
(defun stop-reads (server event-base client)
  (declare (ignore server))
  (iolib:remove-fd-handlers event-base (iolib:socket-os-fd (tcp-client-socket client)) :read t))

(defun start-writes (server event-base client &aux (buffer (tcp-client-write-buffer client)))
  ;; TODO - this blows and doesn't work. Make it not blow.
  (iolib:set-io-handler event-base (iolib:socket-os-fd (tcp-client-socket client))
                        :write
                        (lambda (&rest ig)
                          (declare (ignore ig))
                          (handler-case
                              (let ((bytes-written (iolib:send-to (tcp-client-socket client) buffer)))
                                (print bytes-written)
                                (when t ; (buffer-drained-p buffer)
                                  (on-drain server client)))
                            ((or iolib:socket-connection-reset-error isys:ewouldblock isys:epipe) (e)
                              (on-client-error server client e))))))

;; TCP server test
(defclass echo () ())
(defmethod on-connect ((server echo) client)
  (print "Client connected."))
(defmethod on-data ((server echo) client data)
  (format t "~&Data time: ~S~%" (flex:octets-to-string data :external-format :utf8)))
(defmethod on-disconnect ((server echo) client)
  (format t "~&Client disconnected: ~S~%" client))
#+nil(start-listener (make-instance 'echo) :host "127.0.0.1" :port 1337)
