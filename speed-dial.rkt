#lang racket

;(require "menu-main.rkt")
(require
    "constants.rkt"
    "functions.rkt"
    "racket_general/functions.rkt")
(require 2htdp/batch-io) ;read-lines

;-------------------------------------------
; functions
;-------------------------------------------

; load-menus-from-file:
; Main logic that loads all the menu info
; from the configuration file.
(define (load-menus-from-file a-file)
  (split-list-of-strings
    (filter-empty-strings (load-lines-from-file a-file C-COMMENT-CHARS))
    C-DELIMITER))

; filter-menu-items:
; Filter the list-of-menus, to show only
; a certain type of menus.
; Example:
; (("0" "1" "Menu issues" "option 1" ...) ("0" "1" "Menu issues" "option 2" ...) ("0" "2" "Menu blabla" "option A" ...))
; gives, for filter on 1":
; (("0" "1" "Menu issues" "option 1" ...) ("0" "1" "Menu issues" "option 2" ...))
(define (filter-menu-items a-list a-parent-id)
  (filter (lambda (x) (equal? (list-ref x 0) a-parent-id)) a-list))

; retrieve-menu-items:
; Retrieves the menu-items with a given a-parent-id,
; so we know what items we have. Only
; the unique values are returned, sorted by name.
; TODO: add a parent-id filter
; TODO: find out how to replace car with list-ref 1
(define (retrieve-menu-items a-menu-items a-parent-id)
  (sort
    (remove-duplicates (filter-menu-items a-menu-items a-parent-id))
    #:key car string<?))

; write-main-menu:
; Write the main menu items, based on a given list
; of options. The retrieved categories are normally used for this.
(define (print-menu a-menu-items)
  (map (lambda(x)
    (fprintf (current-output-port) "~a. ~a\n" (list-ref x 1) (list-ref x 2)))
    a-menu-items)
  (newline)
  (display-prompt))

; show-menu:
; Show the menu, as given by the
; list a-menus.
; Note: Used for displaying the main menu.
; This also starts the option parsing loop.
; TODO: write a retrieve-menu-options functions, similar
; to retrieve-menu-items
(define (show-menu a-menu-items a-parent-id)
    (system "clear")
    (print-header "Menu")
    (print-menu (retrieve-menu-items a-menu-items a-parent-id))
    (loop-choice a-menu-items a-parent-id (list 'a 'i 'o))
    )

; loop-choice:
; Loop that displays the menu, until an option is chosen.
; TODO: make the choice condition dependend on a-choice-list.
(define (loop-choice a-menu-items a-parent-id a-choice-list)
    (let ((l-choice (read)))
    (cond ((member l-choice a-choice-list) (run-choice l-choice a-menu-items))
          ((eq? l-choice 'q) (run-quit))
          (else (print-menu-error)))
    (show-menu a-menu-items a-parent-id)))

; run-choice:
; Execute the correct action, belonging to the chosen option
; for that menu-item.
(define (run-choice a-choice a-list-of-menus)
  (printf "~a chosen\n" a-choice)
  (sleep 3))

; list-of-menus:
; Get list-of-menus from the file, based on the
; file location in the constants.rkt module.
(define list-of-menus
  (load-menus-from-file C-SPEED-DIAL-MENU-ITEMS))

;-------------------------------------------
; Main
;-------------------------------------------

;(writeln "debug >>> filtered issues menu")
;(filter-list-of-menus list-of-menus "1")
;(writeln "debug >>> categories")
;(retrieve-menu-categories list-of-menus)
(show-menu list-of-menus 0) ; Show main menu
;(show-menu-main)
