;;;; conserv.asd

(asdf:defsystem #:conserv
  :serial t
  :depends-on (#:iolib
               #:flexi-streams
               #:alexandria
               #:cl-ppcre)
  :components ((:module src
                        :serial t
                        :components
                        ((:file "package")
                         (:file "utils")
                         (:file "event-loop")
                         (:file "socket")
                         (:file "server")
                         (:module http
                                  :serial t
                                  :components
                                  ((:file "package")
                                   (:file "headers")
                                   (:file "http")))))))
