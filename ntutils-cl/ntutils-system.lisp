;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ntutils-system.lisp:
;;;; File that contains general functions, related to system utils.
;;;; This includes, but is not limited to:
;;;; executing shell commands
;;;;
;;;; See LICENSE.txt for license information.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ntutils)

(defun run-command (a-command-pipe)
  "This function runs a shell command, via inferior-shell."
  (inferior-shell:run/ss `(inferior-shell:pipe (,a-command-pipe))))

; TODO: Open in default browser.
(defun open-url ()
  "Opens a given url in firefox."
  (lambda (a-url)
    (format t "firefox ~a~%" a-url)))

(defun sh (a-shell a-cmd)
  "A multi-implementation function equivalent for the C function system."
  #+clisp (shell a-cmd)
  #+ecl (si:system a-cmd)
  #+sbcl (sb-ext:run-program a-shell (list "-c" a-cmd) :input nil :output *standard-output*)
  #+clozure (ccl:run-program a-shell (list "-c" a-cmd) :input nil :output *standard-output*))

(defun my-getenv (name &optional default)
  "A multi-implementation function, for retrieving environment variables."
    #+CMU
    (let ((x (assoc name ext:*environment-list*
                    :test #'string=)))
      (if x (cdr x) default))
    #-CMU
    (or
     #+Allegro (sys:getenv name)
     #+CLISP (ext:getenv name)
     #+ECL (si:getenv name)
     #+SBCL (sb-unix::posix-getenv name)
     #+LISPWORKS (lispworks:environment-variable name)
     default))
