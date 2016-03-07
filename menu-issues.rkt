#lang racket

(require
  "functions.rkt"
  "constants.rkt"
  "menu-functions.rkt")

(provide
  show-menu-issues)

(define show-menu-issues
  (lambda ()
    (system "clear")
    (print-header)
    (print-menu
      c-issues-menu-title
      c-issues-menu-items)
    (loop-choice-issues)))
;TODO: refactor to show-menu with params?

(define loop-choice-issues
  (lambda ()
    (let ((l-choice (read)))
    (cond ((eq? l-choice '0) (run-issues-opt0))
          ((eq? l-choice 'b) (display "")) ; do nothing
          ((eq? l-choice 'q) (run-quit))
          (else (print-menu-error)))
    (if
      (not
        (or
          (eq? l-choice 'q)
          (eq? l-choice 'b)))
        (show-menu-issues)
        (printf "Processing choice...~%")))))

(define run-issues-opt0
  (lambda ()
    (open-url c-issues-url)
    (sleep 1)))
