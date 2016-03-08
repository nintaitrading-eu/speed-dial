#lang racket

(provide
  C-SPEED-DIAL-CONF
  t-menu-item
  t-menu)

(define C-SPEED-DIAL-CONF "speed-dial.conf")

; menu-item
(struct t-menu-item
  (name option))

; menu
(struct t-menu
  (title menu-items))

; create menu-issues
;(define menu-issues
;  (t-menu "Issue management"
;   (t-menu-item "test1" "i")))

;; menu-issues
;(define menu-issues
;  "Issue management"
;  (("test" "i")))
;(define c-issues-menu-title "Issue management")
;(define c-issues-menu-items
;  (list
;    "0 - Bugzilla"))
;(define c-issues-url "http://webster:88/bugzilla")

; menu-trading
