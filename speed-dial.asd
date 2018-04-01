;;;; speed-dial.asd

(asdf:defsystem #:speed-dial
  :description "Describe speed-dial here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:quicklisp
               #:split-sequence
               #:inferior-shell
               #:ntutils)
  :serial t
  :components ((:file "package")
               (:file "functions")
               (:file "speed-dial" :depends-on ("package" "functions"))
               ))

