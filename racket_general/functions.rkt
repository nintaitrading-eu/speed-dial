#lang racket

(provide
  split-list-of-strings
  take-first-char
  filter-comment-lines
  filter-empty-strings
  load-lines-from-file
  )

(require 2htdp/batch-io) ;read-lines


; split-list-of-strings:
; Gets a list of <delimiter>-separated strings
; and turns it into a list of a list of strings
; Example:
; ("test1a;test1b;test1c" "test2a;;test2c")
; -> (("test1a" "test1b" "test1c") ("test2a" "" "test2c"))
(define (split-list-of-strings a-list-of-strings a-delimiter)
  (map (lambda (a-string)
    (string-split a-string a-delimiter)) a-list-of-strings))

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
(define (load-lines-from-file a-file a-comment-chars)
  (car
    (map (lambda (a-comment-char)
      (filter-comment-lines
        (read-lines a-file) a-comment-char)) a-comment-chars)))

