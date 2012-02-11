(cl:defpackage #:conserv
  (:use #:cl
        #:alexandria
        #:trivial-gray-streams)
  (:export
   ;; Events
   #:on-server-listen
   #:on-server-error
   #:on-server-close
   #:on-client-connect
   #:on-client-data
   #:on-client-error
   #:on-client-close
   #:on-client-output-empty
   ;; #:on-timeout ; Unimplemented

   ;; Client functions
   #:*max-buffer-size*
   #:client-remote-name
   #:client-remote-port
   #:client-server
   #:client-pause
   #:client-resume
   #:client-bytes-read
   #:client-bytes-written
   #:drop-connection

   ;; Server functions
   #:make-server
   #:server-name
   #:server-port
   #:server-external-format-in
   #:server-external-format-out
   #:server-binary-p
   #:server-list-clients
   #:server-count-clients
   #:server-listen))
