#lang racket

;(require "menu-main.rkt")
(require
    "constants.rkt"
    "functions.rkt"
    "racket_general/functions.rkt")
(require 2htdp/batch-io) ;read-lines
(require basedir)

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
(define (filter-menu-items a-menu-items a-parent-menu-id)
  (filter (lambda (x) (equal? (string->number (list-ref x 0)) a-parent-menu-id)) a-menu-items))

; retrieve-menu-items:
; Retrieves the menu-items with a given a-parent-menu-id,
; so we know what items we have. Only
; the unique values are returned, sorted by menu-id.
(define (retrieve-menu-items a-menu-items a-parent-menu-id)
  (sort
    (remove-duplicates (filter-menu-items a-menu-items a-parent-menu-id))
    #:key (lambda (x) (car (cdr x))) string<?))

; retrieve-menu-options:
; Retrieves the menu-options for a given
; parent-menu-id.
; Example:
; '(("1" "1" "a"...) ("1" "2" "b"...))
; will give '("a" "b") as a result.
(define (retrieve-menu-options a-menu-items a-parent-menu-id)
  (map (lambda (x) (string->symbol x))
    (map (lambda (x) (list-ref x 3)) (retrieve-menu-items a-menu-items a-parent-menu-id))))

; write-main-menu:
; Write the main menu items, based on a given list
; of options. The retrieved categories are normally used for this.
(define (print-menu a-menu-items)
  (map (lambda (x)
    (printf "[~a] ~a\n" (list-ref x 3) (list-ref x 2)))
    a-menu-items))

; print-menu-ending:
; Add extra options to the menu, for quitting
; the program and/or going back one level.
(define (print-menu-ending a-parent-menu-id)
  (if (> a-parent-menu-id 0) (printf "[b] back\n") (printf ""))
  (if (equal? a-parent-menu-id 0) (printf "[q] quit\n") (printf "")))

; show-menu:
; Show the menu, as given by the
; list a-menus.
; Note: Used for displaying the main menu.
; This also starts the option parsing loop.
(define (show-menu a-menu-items a-parent-menu-id)
  (system "clear")
  (print-header "Menu")
  (print-menu (retrieve-menu-items a-menu-items a-parent-menu-id))
  (print-menu-ending a-parent-menu-id)
  (newline)
  (display-prompt)
  (loop-choice a-menu-items a-parent-menu-id (retrieve-menu-options a-menu-items a-parent-menu-id)))

; loop-choice:
; Loop that displays the menu, until an option is chosen.
; TODO: make the choice condition dependend on a-choice-list.
(define (loop-choice a-menu-items a-parent-menu-id a-choice-list)
    (let ((l-choice (read)))
    (cond ((member l-choice a-choice-list) (run-choice l-choice a-menu-items))
          ((eq? l-choice 'q) (run-quit))
          (else (print-menu-error)))
    (show-menu a-menu-items a-parent-menu-id)))

; run-choice:
; Execute the correct action, belonging to the chosen option
; for that menu-item.
(define (run-choice a-choice a-list-of-menus)
  (printf "~a chosen\n" a-choice)
  (sleep 1))

; list-of-menus:
; Get list-of-menus from the file, based on the
; file location in the constants.rkt module.
(define (list-of-menus a-menu-items-conf)
  (current-basedir-program-name "speed-dial")
  (load-menus-from-file (path->string (writable-config-file a-menu-items-conf))))

;-------------------------------------------
; Main
;-------------------------------------------

(show-menu (list-of-menus C-SPEED-DIAL-MENU-ITEMS) 0) ; Show main menu
