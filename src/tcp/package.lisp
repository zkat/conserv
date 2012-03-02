(cl:defpackage #:conserv.tcp
  (:use #:cl
        #:alexandria
        #:trivial-gray-streams
        #:conserv
        #:conserv.utils)
  (:export

   ;; Sockets
   #:*socket*
   #:on-socket-error
   #:on-socket-end-of-file
   #:on-socket-connect
   #:on-socket-data
   #:on-socket-close
   #:on-socket-output-empty
   #:drop-connection

   #:socket-connect
   #:socket-driver
   #:socket-server
   #:socket-external-format-in
   #:socket-external-format-out
   #:socket-local-p
   #:socket-remote-name
   #:socket-remote-port
   #:socket-local-name
   #:socket-local-port
   #:socket-paused-p
   #:socket-pause
   #:socket-resume
   #:socket-bytes-read
   #:socket-bytes-written

   ;; Servers
   #:*server*
   #:on-server-listen
   #:on-server-connection
   #:on-server-error
   #:on-server-close

   #:server-listen
   #:server-driver
   #:server-external-format-in
   #:server-external-format-out
   #:server-list-clients
   #:server-count-clients
   #:server-paused-p
   #:server-pause
   #:server-resume))
