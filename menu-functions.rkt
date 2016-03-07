#lang racket

(require
  "functions.rkt")

(provide
  print-menu
  run-quit
  print-menu-error)

(define print-menu-error
  (lambda ()
    (printf "Unknown option. Choose wisely.~%")
    (sleep 1)))

(define print-menu-issues
  (lambda ()
    (print-menu "Issue management"
      (list "0 - Bugzilla"))))

(define print-menu
  (lambda (a-title a-menu-items)
    (print-menu-header a-title)
    (print-menu-middle a-menu-items)
    (print-menu-footer)))

(define print-menu-header
  (lambda (a-title)
    (printf "~a~%" a-title)
    (printf "-------------------------~%"))) 

(define print-menu-middle
  (lambda (a-menu-items)
    (for-each print-menu-item a-menu-items)))

(define print-menu-item
  (lambda (a-menu-item)
    (printf "~a~%" a-menu-item)))

(define print-menu-footer
  (lambda ()
    (printf "b - Back~%")
    (printf "q - Quit~%")
    (newline)
    (display-prompt)))

(define run-quit
  (lambda ()
    (system "clear")
    (writeln "Bye.")
    (exit)))
