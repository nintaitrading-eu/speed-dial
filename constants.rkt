#lang racket

(provide
  C-SPEED-DIAL-MENUS
  C-SPEED-DIAL-MENU-ITEMS
  C-COMMENT-CHARS
  C-DELIMITER
  t-menu-item
  t-menu)

(define C-SPEED-DIAL-MENUS "speed-dial-menus.conf")
(define C-SPEED-DIAL-MENU-ITEMS "speed-dial-menu-items.conf")
(define C-COMMENT-CHARS '("#"))
(define C-DELIMITER ";")

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
