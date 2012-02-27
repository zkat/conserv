(cl:defpackage #:conserv
  (:use #:cl
        #:alexandria
        #:conserv.utils)
  (:export
   ;; event loop
   #:call-with-event-loop
   #:with-event-loop
   #:add-timer
   #:remove-timer
   #:exit-event-loop
   #:register-socket
   #:unregister-socket
   #:*event-base*))
