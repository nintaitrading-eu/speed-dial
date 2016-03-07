#lang racket

(require
  "functions.rkt"
  "menu-functions.rkt"
  "menu-accounting.rkt"
  "menu-development.rkt"
  "menu-trading.rkt"
  "menu-issues.rkt")

(provide
  show-menu-main)

(define print-menu-main
  (lambda ()
    (printf "Main~%")
    (printf "-------------------------------------------------------------------------------~%")
    (printf "a - Accounting~%")
    (printf "d - Development~%")
    (printf "t - Trading~%")
    (printf "i - Issue management~%")
    (printf "q - Quit~%")
    (newline)
    (display-prompt)))

(define loop-choice-main
  (lambda ()
    (let ((l-choice (read)))
    (cond ((eq? l-choice 'a) (show-menu-accounting))
          ((eq? l-choice 'd) (show-menu-development))
          ((eq? l-choice 't) (show-menu-trading))
          ((eq? l-choice 'i) (show-menu-issues))
          ((eq? l-choice 'q) (run-quit))
          (else (print-menu-error)))
    (show-menu-main))))

(define show-menu-main
  (lambda ()
    (system "clear")
    (print-header)
    (print-menu-main)
    (loop-choice-main)))
