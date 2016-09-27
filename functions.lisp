;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; functions.lisp:
;;;; File that contains general functions, related to this application.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :speed-dial)

(defun print-header (a-title)
  "Prints a formatted header, with the given title."
  (format t "~a~%" a-title)
  (format t "-----------------------------~%"))

(defun print-menu-error ()
  "Prints an error message when a given option is unknown."
  (progn
    (format t "Unknown option. Choose wisely.~%")
    (sleep 1)))

; TODO: Open in default browser.
(defun open-url ()
  "Opens a given url in firefox."
  (lambda (a-url)
    (format t "firefox ~a~%" a-url)))

(defun display-prompt ()
  "Displays a prompt."
    (format t "~a " *c-prompt*))

(defun sh (cmd)
  "A multi-implementation function equivalent for the C function system."
  #+clisp (shell cmd)
  #+ecl (si:system cmd)
  #+sbcl (sb-ext:run-program *c-sh-cmd* (list "-c" cmd) :input nil :output *standard-output*)
  #+clozure (ccl:run-program *c-sh-cmd* (list "-c" cmd) :input nil :output *standard-output*))

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

(defun run-quit ()
  "Quits, with a fancy message."
  (progn
    (sh "clear")
    (format t "Bye.~%")
    (exit)))
