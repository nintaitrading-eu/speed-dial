;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; speed-dial.lisp:
;;;; Command line menu for easy access to applications and urls.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:speed-dial)

;;; Global variables
(defvar *c-speed-dial-menu-items* "speed-dial-menu-items.conf")
(defvar *c-comment-chars* '("#"))
(defvar *c-delimiter* ";")
(defvar *c-prompt* "> ")
;(defvar *c-sh-cmd* "/bin/sh") ; FreeBSD
(defvar *c-sh-cmd* "C:\\Program Files (x86)\\Gow\\bin\\bash.exe") ; Windows

;;; functions.lisp
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
  (lambda ()
    (printf *c-prompt*)))

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

;;; Functions

(defun load-menus-from-file (a-file)
  "Main logic that loads all the menu info from the configuration file."
  (split-list-of-strings
    (filter-empty-strings (load-lines-from-file a-file *c-comment-chars*))
    *c-delimiter*))

(defun print-menu (a-menu-items)
  "Write the main menu items, based on a given list
of options. The retrieved categories are normally used for this."
  (map (lambda (x)
    (format t "[~a] ~a~%" (nth 3 x) (nth 2 x)))
    a-menu-items))

(defun print-menu-ending (a-parent-menu-id)
  "Add extra options to the menu, for quitting
the program and/or going back one level."
  (if (> a-parent-menu-id 0) (printf "[b] back~%") (printf ""))
  (if (equal? a-parent-menu-id 0) (printf "[q] quit~%") (printf "")))

(defun show-menu (a-menu-items a-parent-menu-id)
  "Show the menu, as given by the list a-menus.
Note: Used for displaying the main menu.
This also starts the option parsing loop."
  (sh "clear")
  (print-header "Menu")
  ;(print-menu (retrieve-menu-items a-menu-items a-parent-menu-id))
  (print-menu-ending a-parent-menu-id)
  (newline))

(defun main ()
  " Main entry point to the application."
  (show-menu (list-of-menus "") 0))

(main)
