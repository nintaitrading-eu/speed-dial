#lang racket

;(require "menu-main.rkt")
(require "constants.rkt")
(require 2htdp/batch-io) ;read-lines

;; constants

; take-first-char:
; Take first char of string, unless string is empty.
(define (take-first-char a-string)
  (if (eq? (string-length a-string) 0) "" (substring a-string 0 1)))

; filter-comment-lines:
; Filter out strings that start with a comment-character,
; so only the data lines are added.
; Note: This function trims spaces on the left.
(define (filter-comment-lines a-lines a-comment-char)
  (filter (lambda (a-line)
    (not (equal? (take-first-char (string-trim a-line)) a-comment-char)))
     a-lines))

; filter-empty-strings:
; Removes empty strings from a list of strings.
(define (filter-empty-strings a-stringlist)
  (filter (lambda (a-string)
     (not (equal? (string-length (string-trim a-string)) 0)))
      a-stringlist))

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
  (filter-empty-strings (load-lines-from-file a-file)))

;; Main
(load-menus-from-file C-SPEED-DIAL-CONF)
;(show-menu-main)


