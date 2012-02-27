;;;; conserv.asd

(asdf:defsystem #:conserv
  :serial t
  :depends-on (#:iolib
               #:babel
               #:trivial-gray-streams
               #:alexandria
               #:cl-ppcre)
  :components ((:module src
                        :serial t
                        :components
                        ((:file "utils")
                         (:file "package")
                         (:file "event-loop")
                         (:module tcp
                                  :serial t
                                  :components
                                  ((:file "package")
                                   (:file "socket")
                                   (:file "server")))
                         (:module http
                                  :serial t
                                  :components
                                  ((:file "package")
                                   (:file "headers")
                                   (:file "request")
                                   (:file "reply")
                                   (:file "http")))))))
