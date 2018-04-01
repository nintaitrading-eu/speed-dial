;;;; ntutils.asd

(asdf:defsystem #:ntutils
  :description "Generic functions for common lisp development across projects for Nintai bvba."
  :author "Andy Nagels <thereisanewway@gmail.com>"
  :license "MIT license"
  :depends-on (#:quicklisp
               #:split-sequence
               #:inferior-shell)
  :serial t
  :components ((:file "package")
               (:file "ntutils-string")
               (:file "ntutils-file")
               (:file "ntutils-system")
               ;(:file "ntutils" :depends-on ("package" "ntutils-string" "ntutils-file" "ntutils-system"))
               ))

