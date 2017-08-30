;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; speed-dial.lisp:
;;;; Command line menu for easy access to applications and urls.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Global variables
(defvar *c-speed-dial-menu-items* "speed-dial-menu-items.conf")
(defvar *c-comment-chars* '("#"))
(defvar *c-delimiter* ";")
(defvar *c-prompt* "> ")
;(defvar *c-sh-cmd* "/bin/sh") ; FreeBSD
(defvar *c-sh-cmd* "C:\\Program Files (x86)\\Gow\\bin\\bash.exe") ; Windows
(defvar *menu-items* nil)

;;; Functions

;(defun load-menus-from-file (a-file)
;  "Main logic that loads all the menu info from the menu-items file."
;    (load-menu-items *c-speed-dial-menu-items*))
  ;(speed-dial::split-list-of-strings
  ;  (speed-dial::filter-empty-strings (speed-dial::load-lines-from-file a-file *c-comment-chars*))
  ;  *c-delimiter*))

;(defun filter-menu-items (a-menu-items a-menu-id)
;  "Filter the list-of-menus, to show only a certain type of menus."
;; Example:
;; ((-1 0 1 "Menu issues" "option 1" ...) (-1 0 1 "Menu issues" "option 2" ...) ("0" "2" "Menu blabla" "option A" ...))
;; gives, for filter on "1":
;; ((-1 0 1 "Menu issues" "option 1" ...) (-1 0 1 "Menu issues" "option 2" ...))
;  (remove-if-not (lambda (x) (equal (parse-integer (nth 0 x) :junk-allowed t) a-menu-id)) a-menu-items))

; TODO: lots of fixes needed here.
;(defun retrieve-menu-items (a-menu-id)
;  "Retrieves the menu-items with a given a-menu-id, so we know what items we have.
;Only the unique values are returned, sorted by menu-id."
;; Example:
;; (("0" "1" "Menu issues" "option 1" ...) ("1" "2" "Menu blabla" "option A" ...))
;; gives, for parent-id 1:
;; (("1" "2" "Menu blabla" "option A" ...))
;  (print (filter-menu-items a-menu-id))
;  (sort (remove-duplicates (filter-menu-items a-menu-id)) #'string<= :key #'second))

; TODO: Idea: loop over lists and cons the getf KEYCHAR into a new list.
(defun retrieve-menu-options (a-menu-id)
  "Retrieves the menu-options for a given parent-menu-id."
; Example:
; '((-1 0 1 "a"...) (-1 0 2 "b"...))
; will give '("a" "b") as a result.
  (mapcar #' get-keychar *menu-items*))
  ;(mapcar #' intern (mapcar #' get-keychar *menu-items*)))

(defun get-keychar (a-menu-item)
  "Get the KEYCHAR from a menu-item list."
  ; Example:
  ; '(-1 0 1 "a"...)
  ; gives ("a")
  (getf a-menu-item :KEYCHAR))

(defun print-menu-items (a-menu-items)
  "Write the main menu items, based on a given list of options.
  The retrieved categories are normally used for this."
  ; TODO: implement filtering on a-menu-id 
  (map 'list (lambda (x) (format t "[~a] ~a~%" (string-downcase (symbol-name (getf x :KEYCHAR))) (getf x :TITLE))) a-menu-items))

(defun print-menu-ending (a-menu-id)
  "Add extra options to the menu, for quitting
the program and/or going back one level."
; TODO: use a macro for generating a menu item?
; See the make-cd function?
  (if (> a-menu-id 0) 
    (print-menu-items '((:PARENT-MENU-ID (-1 a-menu-id) :MENU-ID a-menu-id :TITLE "back" :KEYCHAR b :COMMAND "" :MESSAGE "" :MESSAGE-DURATION-SECONDS 0))) (format t ""))
  (if (equal a-menu-id 0)
    (print-menu-items '((:PARENT-MENU-ID (-1 a-menu-id) :MENU-ID a-menu-id :TITLE "quit" :KEYCHAR q :COMMAND "" :MESSAGE "" :MESSAGE-DURATION-SECONDS 0))) (format t ""))) 

(defun select (selector-fn a-menu-items)
  "Select only menu-items with the given selector."
  (remove-if-not selector-fn a-menu-items))

(defun where (&key a-parent-menu-id a-menu-id)
  "Where clause for filtering on a combination of parent-menu-id and menu-id."
  #'(lambda (a-menu-item) 
    (and
      (if a-parent-menu-id (equal (getf a-menu-item :PARENT-MENU-ID) a-parent-menu-id) t)
      (if a-menu-id (equal (getf a-menu-item :MENU-ID) a-menu-id) t))))

(defun menu-selector (a-parent-menu-id a-menu-id)
  "Select only menu-items with the given menu-id.
This depends on the select function."
; Example:
; (select (menu-selector 0) *menu-items*) gives the main menu.
#'(lambda (id) (and (equal (getf id :MENU-ID) a-menu-id))))

(defun show-menu (a-parent-menu-id a-menu-id)
  "Show the menu, as given by the list a-menus.
Note: Used for displaying the main menu.
This also starts the option parsing loop."
  (progn
    (speed-dial::sh *c-sh-cmd* "clear")
    (apply #' append (speed-dial::print-header "Menu")
      (print-menu-items (select (where :a-parent-menu-id a-parent-menu-id :a-menu-id a-menu-id) *menu-items*))
      (print-menu-ending a-menu-id)
      (format t "~%~a " *c-prompt*)
      (format t ""))
    (loop-choice a-menu-id (retrieve-menu-options a-menu-id))))

(defun loop-choice (a-menu-id a-choice-list)
  "Loop that displays the menu, until an option is chosen."
    (let ((l-choice (read)))
    (cond ((member l-choice a-choice-list) (run-choice l-choice))
          ((equalp l-choice 'q) (speed-dial::run-quit *c-sh-cmd*))
          (t (speed-dial::print-menu-error)))
    (show-menu (- a-menu-id 1) a-menu-id)))

(defun run-choice (a-choice)
  "Execute the correct action, belonging to the chosen option for that menu-item."
  (progn
    ; TODO: do something else than printing
    (format t "~a chosen~%" (string-downcase (symbol-name a-choice)))
    (sleep 1)))

; TODO: how to implement current-basedir-program-name
;(defun list-of-menus (a-menu-items-conf)
;  "Get list-of-menus from the given file location.
;Defaults to $XDG_CONF_DIR/speed-dial/speed-dial-menu-items.conf,
;if no location was given, based on the
;filename in the constants.rkt module."
  ;(current-basedir-program-name "speed-dial")
;  (concatenate 'string (speed-dial::my-getenv "XDG_BASE_DIR") "/speed-dial")
;  (cond
;    ((equal (string-trim '(#\Space #\e #\t #\m) a-menu-items-conf) "")
;      (load-menus-from-file *c-speed-dial-menu-items*))
;    (else (load-menus-from-file a-menu-items-conf))))

(defun load-menu-items (a-filename)
  "Load the menu structure from the given menu-items file."
  (with-open-file (in a-filename)
    (with-standard-io-syntax
      ;(setf *menu-items* (speed-dial::filter-comment-lines (read in))))))
      (setf *menu-items* (read in)))))   

(defun main ()
  "Main entry point to the application."
  ;(print (speed-dial::load-lines-from-file *c-speed-dial-menu-items* *c-comment-chars*)))
  (progn
    (load-menu-items *c-speed-dial-menu-items*) ; TODO: implement the xdg_basedir logic
    (show-menu -1 0)))

(main)
