;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; speed-dial.lisp:
;;;; Command line menu for easy access to applications and urls.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Global variables
(defvar *c-speed-dial-menu-items* "speed-dial-menu-items.conf")
(defvar *c-comment-chars* '("#"))
(defvar *c-delimiter* ";")
(defvar *c-prompt* "> ")
(defvar *c-sh-cmd* "/bin/sh") ; FreeBSD
;(defvar *c-sh-cmd* "C:\\Program Files (x86)\\Gow\\bin\\bash.exe") ; Windows

;;; Functions

(defun load-menus-from-file (a-file)
  "Main logic that loads all the menu info from the configuration file."
  (speed-dial::split-list-of-strings
    (speed-dial::filter-empty-strings (speed-dial::load-lines-from-file a-file *c-comment-chars*))
    *c-delimiter*))

(defun filter-menu-items (a-menu-items a-parent-menu-id)
  "Filter the list-of-menus, to show only a certain type of menus."
; Example:
; (("0" "1" "Menu issues" "option 1" ...) ("0" "1" "Menu issues" "option 2" ...) ("0" "2" "Menu blabla" "option A" ...))
; gives, for filter on "1":
; (("0" "1" "Menu issues" "option 1" ...) ("0" "1" "Menu issues" "option 2" ...))
  (filter (lambda (x) (equal (string->number (list-ref x 0)) a-parent-menu-id)) a-menu-items))

; TODO: lots of fixes needed here.
(defun retrieve-menu-items (a-menu-items a-parent-menu-id)
  "Retrieves the menu-items with a given a-parent-menu-id, so we know what items we have.
Only the unique values are returned, sorted by menu-id."
  (sort
    (remove-duplicates (filter-menu-items a-menu-items a-parent-menu-id))
    #:key (lambda (x) (car (cdr x))) string<?))

(defun retrieve-menu-options (a-menu-items a-parent-menu-id)
  "Retrieves the menu-options for a given parent-menu-id."
; Example:
; '(("1" "1" "a"...) ("1" "2" "b"...))
; will give '("a" "b") as a result.
  (map (lambda (x) (intern x))
    (map (lambda (x) (nth 3 x)) (retrieve-menu-items a-menu-items a-parent-menu-id))))

(defun print-menu (a-menu-items)
  "Write the main menu items, based on a given list of options.
The retrieved categories are normally used for this."
  (map (lambda (x)
    (format t "[~a] ~a\n" (nth 3 x) (nth 2 x)))
    a-menu-items))

(defun print-menu (a-menu-items)
  "Write the main menu items, based on a given list
of options. The retrieved categories are normally used for this."
  (map (lambda (x)
    (format t "[~a] ~a~%" (nth 3 x) (nth 2 x)))
    a-menu-items))

(defun print-menu-ending (a-parent-menu-id)
  "Add extra options to the menu, for quitting
the program and/or going back one level."
  (if (> a-parent-menu-id 0) (format t "[b] back~%") (format t ""))
  (if (equal a-parent-menu-id 0) (format t "[q] quit~%") (format t "")))

(defun show-menu (a-menu-items a-parent-menu-id)
  "Show the menu, as given by the list a-menus.
Note: Used for displaying the main menu.
This also starts the option parsing loop."
  (speed-dial::sh  *c-sh-cmd* "clear")
  (speed-dial::print-header "Menu")
  ;(print-menu (retrieve-menu-items a-menu-items a-parent-menu-id))
  (print-menu-ending a-parent-menu-id)
  (terpri))

; TODO: make the choice condition dependend on a-choice-list.
(defun loop-choice (a-menu-items a-parent-menu-id a-choice-list)
  "Loop that displays the menu, until an option is chosen."
    (let ((l-choice (read)))
    (cond ((member l-choice a-choice-list) (run-choice l-choice a-menu-items))
          ((eq? l-choice 'q) (run-quit))
          (else (print-menu-error)))
    (show-menu a-menu-items a-parent-menu-id)))

; run-choice:
(defun run-choice (a-choice a-list-of-menus)
  "Execute the correct action, belonging to the chosen option for that menu-item."
  (progn
    ; TODO: do something else than printing
    (format t "~a chosen~%" a-choice)
    (sleep 1)))

; TODO: how to implement current-basedir-program-name
(defun list-of-menus (a-menu-items-conf)
  "Get list-of-menus from the given file location.
Defaults to $XDG_CONF_DIR/speed-dial/speed-dial-menu-items.conf,
if no location was given, based on the
filename in the constants.rkt module."
  ;(current-basedir-program-name "speed-dial")
  (concatenate 'string (speed-dial::my-getenv "XDG_BASE_DIR") "/speed-dial")
  (cond
    ((equal (string-trim '(#\Space #\e #\t #\m) a-menu-items-conf) "") 
      (load-menus-from-file *c-speed-dial-menu-items*))
    (else (load-menus-from-file a-menu-items-conf))))

(defun main ()
  " Main entry point to the application."
  (show-menu (list-of-menus "") 0))

(main)
