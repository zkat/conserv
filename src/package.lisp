(cl:defpackage #:conserv
  (:use #:cl
        #:alexandria
        #:trivial-gray-streams)
  (:export
   ;; Events
   #:on-listen
   #:on-error
   #:on-connect
   #:on-close
   #:on-data
   #:on-client-error
   #:on-drain
   ;; #:on-timeout ; Unimplemented

   ;; Client functions
   #:*max-buffer-size*
   #:client-remote-name
   #:client-remote-port
   #:client-server
   #:client-pause
   #:client-resume

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
