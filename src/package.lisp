(cl:defpackage #:conserv
  (:use #:cl
        #:alexandria
        #:trivial-gray-streams)
  (:export
   ;; event loop
   #:call-with-event-loop
   #:with-event-loop
   #:add-timer
   #:remove-timer

   ;; Sockets
   #:on-socket-error
   #:on-socket-connect
   #:on-socket-data
   #:on-socket-close
   #:on-socket-output-empty
   #:drop-connection

   #:*max-buffer-size*
   #:socket-driver
   #:socket-external-format-in
   #:socket-external-format-out
   #:socket-binary-p
   #:socket-connect
   #:socket-local-p
   #:socket-remote-name
   #:socket-remote-port
   #:socket-local-name
   #:socket-local-port
   #:socket-paused-p
   #:socket-pause
   #:socket-resume

   ;; Servers

   #:on-tcp-server-listen
   #:on-tcp-server-connection
   #:on-tcp-server-error
   #:on-tcp-server-close

   #:server-driver
   #:server-socket
   #:server-external-format-in
   #:server-external-format-out
   #:server-binary-p
   #:server-list-clients
   #:server-count-clients
   #:server-pause
   #:server-resume
   #:server-listen))
