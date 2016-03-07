#lang racket

(require
  "functions.rkt"
  "menu-functions.rkt")

(provide
  show-menu-development)

(define show-menu-development
  (lambda ()
    (system "clear")
    (print-header)
    (print-menu-development)
    (loop-choice-development)))

(define print-menu-development
  (lambda ()
    (print-menu "Development"
    (list "0 - Update all Nintai-related repositories"))))

(define loop-choice-development
  (lambda ()
    (let ((l-choice (read)))
    (cond ((eq? l-choice '0) (run-development-opt0))
          ((eq? l-choice 'b) (display "")) ; do nothing
          ((eq? l-choice 'q) (run-quit))
          (else (print-menu-error)))
    (if
      (not
        (or
          (eq? l-choice 'q)
          (eq? l-choice 'b)))
        (show-menu-development)
        (printf "Processing choice...~%")))))

(define run-development-opt0
  (lambda ()
    (printf "cd $HOME/dev/c/calculator_finance && git pull origin master~%")
    (printf "cd $HOME/dev/python/fade && git pull origin master~%")
    (printf "cd $HOME/dev/chicken/speed-dial && git pull origin master~%")
    (printf "cd $HOME/dev/website && git pull origin master~%")
    (sleep 1)))
