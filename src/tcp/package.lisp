(cl:defpackage #:conserv.tcp
  (:use #:cl
        #:alexandria
        #:trivial-gray-streams
        #:conserv
        #:conserv.utils)
  (:export

   ;; Tcp-Clients
   #:*tcp-client*
   #:on-tcp-client-error
   #:on-tcp-client-end-of-file
   #:on-tcp-client-connect
   #:on-tcp-client-data
   #:on-tcp-client-close
   #:on-tcp-client-output-empty
   #:drop-connection

   #:tcp-client-connect
   #:tcp-client-driver
   #:tcp-client-server
   #:tcp-client-external-format-in
   #:tcp-client-external-format-out
   #:tcp-client-local-p
   #:tcp-client-remote-name
   #:tcp-client-remote-port
   #:tcp-client-local-name
   #:tcp-client-local-port
   #:tcp-client-paused-p
   #:tcp-client-pause
   #:tcp-client-resume
   #:tcp-client-bytes-read
   #:tcp-client-bytes-written

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
