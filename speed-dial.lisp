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

(defun get-parent-menu-id (a-menu-id a-menu-items)
  "Return the parent-menu-id, based on the given menu-id."
  (car (map 'list (lambda (x) (getf x :PARENT-MENU-ID)) (select (where :a-menu-id a-menu-id) a-menu-items))))

(defun get-child-menu-id (a-keychar a-menu-id a-menu-items)
  "Return the child-menu-id, based on the given menu-id."
  (car (map 'list (lambda (x) (getf x :CHILD-MENU-ID)) (select (where :a-keychar a-keychar :a-menu-id a-menu-id) a-menu-items))))

(defun get-menu-items (a-menu-items)
  "Return the given menu-items in a nicely formatted form."
  (map 'list (lambda (x) (format nil "[~a] ~a~%" (string-downcase (symbol-name (getf x :KEYCHAR))) (getf x :TITLE))) a-menu-items))

(defun get-menu-options (a-menu-items)
  "Return the menu options from the given menu-items."
  (map 'list (lambda (x) (getf x :KEYCHAR)) a-menu-items))

(defun get-menu-commands (a-menu-items)
  "Return the commands from the given menu-items."
  (map 'list (lambda (x) (getf x :COMMAND)) a-menu-items))

(defun get-menu-messages (a-menu-items)
  "Return the messages + durations from the given menu-items."
  (map 'list (lambda (x) (append (list (getf x :MESSAGE)) (list (getf x :MESSAGE-DURATION-SECONDS)))) a-menu-items))

(defun get-menu-ending (a-menu-id)
  "Return extra options to the menu, for quitting
the program and/or going back one level."
; TODO: use a macro for generating a menu item?
; See the make-cd function?
(cond
  ((> a-menu-id 0)
   (append (get-menu-items '((:KEYCHAR b
                              :MENU-ID a-menu-id
                              :PARENT-MENU-ID (get-parent-menu-id a-menu-id *menu-items*)
                              :CHILD-MENU-ID a-menu-id
                              :TITLE "back"
                              :COMMAND ""
                              :MESSAGE ""
                              :MESSAGE-DURATION-SECONDS 0)))
           (get-menu-items '((:KEYCHAR q
                              :MENU-ID a-menu-id
                              :PARENT-MENU-ID (get-parent-menu-id a-menu-id *menu-items*)
                              :CHILD-MENU-ID a-menu-id
                              :TITLE "quit"
                              :COMMAND ""
                              :MESSAGE ""
                              :MESSAGE-DURATION-SECONDS 0)))))
  (t (get-menu-items '((:KEYCHAR q
                        :MENU-ID a-menu-id
                        :PARENT-MENU-ID (get-parent-menu-id a-menu-id *menu-items*)
                        :CHILD-MENU-ID a-menu-id
                        :TITLE "quit"
                        :COMMAND ""
                        :MESSAGE ""
                        :MESSAGE-DURATION-SECONDS 0))))))

(defun select (selector-fn a-menu-items)
  "Select only menu-items with the given selector."
  (remove-if-not selector-fn a-menu-items))

(defun where (&key a-keychar a-menu-id a-parent-menu-id a-child-menu-id a-title a-command a-message a-message-duration-seconds)
  "Where clause for filtering menus."
  #'(lambda (x)
      (and
        (if a-keychar (equal (getf x :KEYCHAR) a-keychar) t)
        (if a-menu-id (equal (getf x :MENU-ID) a-menu-id) t)
        (if a-parent-menu-id (equal (getf x :PARENT-MENU-ID) a-parent-menu-id) t)
        (if a-child-menu-id (equal (getf x :CHILD-MENU-ID) a-child-menu-id) t)
        (if a-title (equal (getf x :TITLE) a-title) t)
        (if a-command (equal (getf x :COMMAND) a-command) t)
        (if a-message (equal (getf x :MESSAGE) a-message) t)
        (if a-message-duration-seconds (equal (getf x :MESSAGE-DURATION-SECONDS) a-message-duration-seconds) t))))

(defun show-menu (a-menu-id)
  "Show the menu."
  (progn
    (speed-dial::sh *c-sh-cmd* "clear")
    (format t "~a~{~a~}~{~a~}~a"
      (speed-dial::get-header "Menu")
      (get-menu-items (select (where :a-menu-id a-menu-id) *menu-items*))
      (get-menu-ending a-menu-id)
      *c-prompt*)
    (force-output) ; Note: The prompt came later. Buffered output in combination with the read function perhaps?
    (let ((l-valid-options (get-menu-options (select (where :a-menu-id a-menu-id) *menu-items*))))
      (let ((l-retval (ask-for-option l-valid-options)))
        (process-chosen-option l-retval l-valid-options a-menu-id)))))

(defun process-chosen-option (a-option a-valid-options a-menu-id)
  "When a menu option is chosen, this function does the processing
of that option."
  (if a-option
    (cond ((equalp a-option 'b) (if (equalp a-menu-id 0)
                                    (show-menu a-menu-id)
                                    (show-menu (get-parent-menu-id a-menu-id *menu-items*))))
          ((equalp a-option 'q) (speed-dial::run-quit *c-sh-cmd*))
          ((member a-option a-valid-options)
            (let ((l-message-with-duration (car (get-menu-messages (select (where :a-keychar a-option :a-menu-id a-menu-id) *menu-items*))))
                  (l-command (car (get-menu-commands (select (where :a-keychar a-option :a-menu-id a-menu-id) *menu-items*)))))
              (if (not (= (length (car l-message-with-duration)) 0))
                  (progn
                    (format t "~a~%" (car l-message-with-duration))
                    (if (cadr l-message-with-duration)
                        (sleep (cadr l-message-with-duration)))
                    (force-output))) ; Note: To solve another issue with buffered output.
              (if (not (= (length l-command) 0))
                  (run-command l-command))
              (show-menu (get-child-menu-id a-option a-menu-id *menu-items*))))
          (t (show-menu (get-child-menu-id a-option a-menu-id *menu-items*))))
    (speed-dial::run-quit *c-sh-cmd*)))

(defun ask-for-option (a-valid-options)
  "Ask for an option and react to it in the appropriate way."
  (let ((l-choice (read)))
    (cond ((member l-choice a-valid-options) l-choice)
          ((equalp l-choice 'b) l-choice)
          ((equalp l-choice 'q) nil)
          (t (progn (speed-dial::print-menu-error) t)))))

(defun run-command (a-command-pipe)
  "This function runs a shell command, via inferior-shell."
  (inferior-shell:run/ss `(inferior-shell:pipe (,a-command-pipe))))

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
    (show-menu 0)))

(main)
