#lang racket

;(require "menu-main.rkt")
(require "constants.rkt")
(require 2htdp/batch-io) ;read-lines

;; constants


;; code
(define (filter-comment-lines a-lines)
  (filter (lambda (a-line)
    (not (equal? (substring a-line 0 1) "#")))
     a-lines))
  
(define (load-menus-from-file a-file)
   (filter-comment-lines (read-lines a-file)))

;; Main
(load-menus-from-file C-SPEED-DIAL-CONF)
;(show-menu-main)


