#lang racket

;(require "menu-main.rkt")
(require 2htdp/batch-io) ;read-lines

(define (filter-comment-lines a-lines)
  (filter (lambda (a-line)
    (not (equal? (substring a-line 0 1) "#")))
     a-lines))

(define speed-dial-conf
  (filter-comment-lines (read-lines "speed-dial.conf")))
  
(define (load-menus-from-file a-file)
   (printf "Load file here~%")
   (writeln speed-dial-conf)
   )

;; Main
(load-menus-from-file "speed-dial.conf")
;(show-menu-main)


