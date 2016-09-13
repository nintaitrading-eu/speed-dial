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

;;; Functions

(defun sh (cmd)
  "A multi-implementation function equivalent for the C function system."
  #+clisp (shell cmd)
  #+ecl (si:system cmd)
  #+sbcl (sb-ext:run-program *c-sh-cmd* (list "-c" cmd) :input nil :output *standard-output*)
  #+clozure (ccl:run-program *c-sh-cmd* (list "-c" cmd) :input nil :output *standard-output*))

(define (load-menus-from-file a-file)
  "Main logic that loads all the menu info from the configuration file."
  (split-list-of-strings
    (filter-empty-strings (load-lines-from-file a-file *c-comment-chars*))
    *c-delimiter*))

(define (print-menu a-menu-items)
  "Write the main menu items, based on a given list
of options. The retrieved categories are normally used for this."
  (map (lambda (x)
    (format t "[~a] ~a~%" (list-ref x 3) (list-ref x 2)))
    a-menu-items))

(define (print-menu-ending a-parent-menu-id)
  "Add extra options to the menu, for quitting
the program and/or going back one level."
  (if (> a-parent-menu-id 0) (printf "[b] back~%") (printf ""))
  (if (equal? a-parent-menu-id 0) (printf "[q] quit~%") (printf "")))

(define (show-menu a-menu-items a-parent-menu-id)
  "Show the menu, as given by the list a-menus.
Note: Used for displaying the main menu.
This also starts the option parsing loop."
  (sh "clear")
  (print-header "Menu")
  (print-menu (retrieve-menu-items a-menu-items a-parent-menu-id))
  (print-menu-ending a-parent-menu-id)
  (newline)

(defun main ()
  " Main entry point to the application."
  (show-menu (list-of-menus "") 0))

(main)
