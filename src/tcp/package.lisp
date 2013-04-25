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

   ;; Tcp-Listeners
   #:*tcp-listener*
   #:on-tcp-listener-listen
   #:on-tcp-listener-connection
   #:on-tcp-listener-error
   #:on-tcp-listener-close

   #:tcp-listen
   #:tcp-listener-driver
   #:tcp-listener-external-format-in
   #:tcp-listener-external-format-out
   #:tcp-listener-list-clients
   #:tcp-listener-count-clients
   #:tcp-listener-paused-p
   #:tcp-listener-pause
   #:tcp-listener-resume))
