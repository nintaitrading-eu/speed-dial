#lang racket

(require
  "functions.rkt"
  "menu-functions.rkt")

(provide
  show-menu-accounting)

(define show-menu-accounting
  (lambda ()
    (system "clear")
    (print-header)
    (print-menu-accounting)
    (loop-choice-accounting)))

(define print-menu-accounting
  (lambda ()
    (print-menu "Accounting"
        (list "0 - Ledger reports?"))))

(define loop-choice-accounting
  (lambda ()
    (let ((l-choice (read)))
    (cond ((eq? l-choice '0) (run-accounting-opt0))
          ((eq? l-choice 'b) (display "")) ; do nothing
          ((eq? l-choice 'q) (run-quit))
          (else (print-menu-error)))
    (if
      (not
        (or
          (eq? l-choice 'q)
          (eq? l-choice 'b)))
        (show-menu-accounting)
        (printf "Processing choice...~%")))))

(define run-accounting-opt0
  (lambda ()
    (printf "show ledger reports?~%")
    (newline)
    (sleep 1)))
