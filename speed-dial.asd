;;;; speed-dial.asd

(asdf:defsystem #:speed-dial
  :description "Describe speed-dial here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:quicklisp)
  :serial t
  :components ((:file "package")
               (:file "menu-functions")
               (:file "functions")
               (:file "cl_generic/functions")
               (:file "speed-dial" :depends-on ("package" "menu-functions" "functions" "cl_generic/functions"))
               ))

