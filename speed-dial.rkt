#lang racket

;(require "menu-main.rkt")
(require "constants.rkt")
(require 2htdp/batch-io) ;read-lines

;; constants


; filter-comment-lines:
; Filter out strings that start with a #,
; so 
(define (filter-comment-lines a-lines a-comment-char)
  (filter (lambda (a-line)
    (not (equal? (substring a-line 0 1) a-comment-char)))
     a-lines))
  
(define (load-menus-from-file a-file)
  (car
    (map (lambda (a-comment-char)
      (filter-comment-lines
        (read-lines a-file) a-comment-char)) C-COMMENT-CHARS)))

;; Main
(load-menus-from-file C-SPEED-DIAL-CONF)
;(show-menu-main)


