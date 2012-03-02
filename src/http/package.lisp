(cl:defpackage #:conserv.http
  (:use #:cl
        #:alexandria
        #:trivial-gray-streams
        #:conserv
        #:conserv.utils
        #:conserv.tcp)
  (:export
   ;; Requests
   #:*request*
   #:on-request-data
   #:on-request-continue
   #:on-request-upgrade
   #:on-request-close

   #:request-remote-name
   #:request-remote-port
   #:request-method
   #:request-url
   #:request-headers-in
   #:request-http-version
   #:request-external-format-in

   #:request-response-status
   #:request-headers-out
   #:request-external-format-out

   #:set-headers
   #:write-headers

   ;; HTTP server
   #:*http-server*
   #:on-http-listen
   #:on-http-request
   #:on-http-connection
   #:on-http-close
   #:on-http-error

   #:http-server-driver
   #:http-server-external-format-in
   #:http-server-external-format-out
   #:http-listen))
