;;;; conserv.asd

(asdf:defsystem #:conserv
  :serial t
  :depends-on (#:iolib
               #:flexi-streams
               #:alexandria)
  :components ((:module src
                        :components
                        ((:file "conserv")))))
