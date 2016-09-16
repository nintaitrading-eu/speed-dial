;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; menu-functions.lisp:
;;;; File that contains functions, specific to the menus.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :speed-dial)

(defun print-menu-error ()
  "Prints a general error message, when a wrong menu options is chosen."
  (progn
    (format t "Unknown option. Choose wisely.~%")
    (sleep 1)
  ))

(defun print-menu-issues ()
  "Print the issues menu."
  (print-menu "Issue management"
    (list "0 - Bugzilla")))

(defun print-menu (a-title a-menu-items)
  "Prints the menu structure."
  (progn
    (print-menu-header a-title)
    (print-menu-middle a-menu-items)
    (print-menu-footer)))

(defun print-menu-header (a-title)
  "Prints the menu header."
  (progn
    (format t "~a~%" a-title)
    (format t "-------------------------~%")))

(defun print-menu-middle (a-menu-items)
  "Prints each menu-item, in a given list of menu-items."
  (for-each print-menu-item a-menu-items))

(defun print-menu-item (a-menu-item)
  "Prints the menu-item."
  (format t "~a~%" a-menu-item))

(defun print-menu-footer ()
  "Prints a general menu-footer."
  (progn
    (format t "b - Back~%")
    (format t "q - Quit~%")
    (terpri)
    (display-prompt)))
