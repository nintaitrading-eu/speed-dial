#lang racket

;(require "menu-main.rkt")
(require
    "constants.rkt",
    "functions.rkt",
    "racket_general/functions.rkt")
(require 2htdp/batch-io) ;read-lines

;-------------------------------------------
; functions
;-------------------------------------------

; load-lines-from-file:
; Loads all lines from a file, ignoring comments.
(define (load-lines-from-file a-file)
  (car
    (map (lambda (a-comment-char)
      (filter-comment-lines
        (read-lines a-file) a-comment-char)) C-COMMENT-CHARS)))

; load-menus-from-file:
; Main logic that loads all the menu info
; from the configuration file.
(define (load-menus-from-file a-file)
  (split-list-of-strings
    (filter-empty-strings (load-lines-from-file a-file))
    C-DELIMITER))

; filter-list-of-menus:
; Filter the list-of-menus, to show only
; a certain type of menus.
; Example:
; (("1" "Menu issues" "option 1" ...) ("2" "Menu issues" "option 2" ...) ("3" "Menu blabla" "option A" ...))
; gives, for filter on "Menu issues":
; (("1" "Menu issues" "option 1" ...) ("2" "Menu issues" "option 2" ...))
; TODO: finish this
; TODO: don't use Menu issues as a category, but use an id.
(define (filter-list-of-menus a-category-id)
  (filter (lambda (a-list) (??? a-list)) a-category-id))

; Map a filter to a list. Add this above?
;(define (category-id-is-equal-to a-list a-category-id)
;    (map (lambda (a-list)
;      (list-ref a-list 0)) a-list) a-category-id))

;-------------------------------------------
; Main
;-------------------------------------------

(define list-of-menus (
  load-menus-from-file C-SPEED-DIAL-CONF))
(writeln "debug >>> list-of-menus:")
list-of-menus
(writeln "debug >>> Option of second entry")
(list-ref (list-ref list-of-menus 1) 1)
(writeln "debug >>> filtered issues menu")
(filter-list-of-menus list-of-menus)
; TODO: filter list-of-menus on a certain ID (add ID to file?)
; TODO: map write function to list-of-menus
; TODO: sort list-of-menus first?
;(show-menu-main)
