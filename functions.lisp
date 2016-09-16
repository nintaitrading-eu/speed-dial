;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; functions.lisp:
;;;; File that contains general functions, related to this application.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :speed-dial)

(defun print-header (a-title)
  "Prints a formatted header, with the given title."
  (fprintf (current-output-port) "~a~%" a-title)
  (fprintf (current-output-port) "-----------------------------~%"))

(defun print-menu-error ()
  "Prints an error message when a given option is unknown."
  (lambda ()
    (printf "Unknown option. Choose wisely.~%")
    (sleep 1)))

; TODO: Open in default browser.
(defun open-url ()
  "Opens a given url in firefox."
  (lambda (a-url)
    (printf "firefox ~a~%" a-url)))

(defun display-prompt ()
  "Displays a prompt."
    (printf *c-prompt*))

(defun sh (cmd)
  "A multi-implementation function equivalent for the C function system."
  #+clisp (shell cmd)
  #+ecl (si:system cmd)
  #+sbcl (sb-ext:run-program *c-sh-cmd* (list "-c" cmd) :input nil :output *standard-output*)
  #+clozure (ccl:run-program *c-sh-cmd* (list "-c" cmd) :input nil :output *standard-output*))

(defun run-quit ()
  "Quits, with a fancy message."
  (progn
    (sh "clear")
    (printf "Bye.")
    (exit)))
