#lang racket

(provide
  c-issues-menu-title
  c-issues-menu-items
  c-issues-url)


; menu-items
(struct t-menu-item
  (name option))

; menu
(struct t-menu
  (title menu-items))

; create menu-issues
(define menu-issues
  (t-menu "Issue management"
   (t-menu-item "test1" "i")))

(struct menu-item
  (name option))

; menu
(struct menu
  (title
    (list menu-item)


; menu-issues
(define menu-issues
  "Issue management"
  (("test" "i")))
(define c-issues-menu-title "Issue management")
(define c-issues-menu-items
  (list
    "0 - Bugzilla"))
(define c-issues-url "http://webster:88/bugzilla")

; menu-trading
