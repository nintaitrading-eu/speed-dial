#lang racket

(require
  "functions.rkt"
  "menu-functions.rkt")

(provide
  show-menu-other)

(define show-menu-other
  (lambda ()
    (system "clear")
    (print-header)
    (print-menu-other)
    (loop-choice-other)))

(define print-menu-other
  (lambda ()
    (printf "Other~%")
    (printf "-------------------------~%")
    (printf "0 - Do something else...~%")
    (printf "b - Back~%")
    (printf "q - Quit~%")
    (newline)
    (display-prompt)))

(define loop-choice-other
  (lambda ()
    (let ((l-choice (read)))
    (cond ((eq? l-choice '0) (run-other-opt0))
          ((eq? l-choice 'b) (display "")) ; do nothing
          ((eq? l-choice 'q) (run-quit))
          (else (print-menu-error)))
    (if
      (not
        (or
          (eq? l-choice 'q)
          (eq? l-choice 'b)))
        (show-menu-other)))))

(define run-other-opt0
  (lambda ()
    (writeln "Doing something else.")
    (sleep 1)))
