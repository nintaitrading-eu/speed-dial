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

; filter-list-of-menus:
; Filter the list-of-menus, to show only
; a certain type of menus.
; Example:
; (("1" "Menu issues" "option 1" ...) ("1" "Menu issues" "option 2" ...) ("2" "Menu blabla" "option A" ...))
; gives, for filter on 1":
; (("1" "Menu issues" "option 1" ...) ("1" "Menu issues" "option 2" ...))
(define (filter-list-of-menus a-list a-category-id)
  (filter (lambda (x) (equal? (car x) a-category-id)) a-list))

; retrieve-menu-categories:
; Retrieves the categories from the list-of-menus,
; so we know what categories we have. Only
; the unique values are returned, sorted by name. 
(define (retrieve-menu-categories a-list-of-menus)
  (sort
    ;(remove-duplicates (retrieve-menu-categories-all a-list))
    (remove-duplicates a-list-of-menus)
    #:key car string<?))

; retrieve-menu-categories-all:
; Retrieves the categories from the list-of-menus,
; so we know what categories we have.
; TODO: finish this: change a-list to a call to a loop
; that gets the categories.
; TODO: implement loop?
; TODO: use let?
(define (retrieve-menu-categories-all a-list-of-menus)
  (map (lambda(x) (list-ref x 1)) a-list-of-menus))

; write-main-menu:
; Write the main menu items, based on a given list
; of options. The retrieved categories are normally used for this.
(define (write-main-menu a-menus)
  (map (lambda(x)
    (fprintf (current-output-port) "~a. ~a\n" (list-ref x 1) (list-ref x 0)))
    a-menus))

; list-of-menus:
; Get list-of-menus from the file, based on the
; file location in the constants.rkt module.
(define list-of-menus
  (load-menus-from-file C-SPEED-DIAL-MENUS))

; list-of-menu-items:
; Get list-of-menu-items, for a given
; menu-id.
(define (list-of-menu-items a-menu-id)
  ((list "item-test1" "item-test2" "item-test3")))

;-------------------------------------------
; Main
;-------------------------------------------

;(writeln "debug >>> filtered issues menu")
;(filter-list-of-menus list-of-menus "1")
;(writeln "debug >>> categories")
;(retrieve-menu-categories list-of-menus)
(write-main-menu (retrieve-menu-categories list-of-menus))
;(show-menu-main)
