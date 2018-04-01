;;;; speed-dial.asd

(asdf:defsystem #:speed-dial
  :description "Describe speed-dial here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:quicklisp
               #:split-sequence
               #:inferior-shell)
  :serial t
  :components ((:file "package")
               (:file "functions")
               (:file "ntutils-cl/ntutils-string")
               (:file "ntutils-cl/ntutils-file")
               (:file "speed-dial" :depends-on ("package" "functions" "utils-cl/utils-string" "utils-cl/utils-file"))
               ))

