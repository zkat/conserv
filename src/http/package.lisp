(cl:defpackage #:conserv.http
  (:use #:cl
        #:alexandria
        #:trivial-gray-streams
        #:conserv)
  (:export
   #:*http-server*
   #:*request*
   #:*reply*
   #:reply-headers*
   #:reply-header
   ;; HTTP
   #:http-listen
   #:on-http-request
   #:on-request-data
   #:reply-headers))

