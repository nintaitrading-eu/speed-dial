;;;; utils-cl.asd

(asdf:defsystem #:utils-cl
  :description "Generic function collections for common lisp development"
  :author "Andy Nagels <thereisanewway@gmail.com>"
  :license "MIT license"
  :depends-on (#:quicklisp
               #:split-sequence
               #:inferior-shell)
  :serial t
  :components ((:file "package")
               (:file "functions")
               (:file "utils-cl/utils-string")
               (:file "utils-cl/utils-file")
               (:file "utils-cl" :depends-on ("package" "functions" "utils-cl/utils-string" "utils-cl/utils-file"))
               ))

