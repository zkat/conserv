(cl:defpackage #:conserv
  (:use #:cl
        #:alexandria
        #:trivial-gray-streams)
  (:export
   #:defprotocol

   #:*default-external-format*
   ;; event loop
   #:call-with-event-loop
   #:with-event-loop
   #:add-timer
   #:remove-timer
   #:exit-event-loop

   ;; Sockets
   #:*socket*
   #:on-socket-error
   #:on-socket-connect
   #:on-socket-data
   #:on-socket-close
   #:on-socket-output-empty
   #:drop-connection

   #:*max-buffer-size*
   #:socket-driver
   #:socket-server
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
   #:*server*
   #:on-server-listen
   #:on-server-connection
   #:on-server-error
   #:on-server-close

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
