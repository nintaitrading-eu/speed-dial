#lang racket

(require
  "functions.rkt"
  "menu-functions.rkt")

(provide
  show-menu-trading)

(define show-menu-trading
  (lambda ()
    (system "clear")
    (print-header)
    (print-menu-trading)
    (loop-choice-trading)))

(define print-menu-trading
  (lambda ()
    (print-menu "Trading"
        (list "0 - Trading journal (ods)"
          "1 - Trading application"
          "2 - Market hours"
          "3 - Market expiration dates"))))
    
(define loop-choice-trading
  (lambda ()
    (let ((l-choice (read)))
    (cond ((eq? l-choice '0) (run-trading-opt0))
          ((eq? l-choice '1) (run-trading-opt1))
          ((eq? l-choice '2) (run-trading-opt2))
          ((eq? l-choice '3) (run-trading-opt3))
          ((eq? l-choice 'b) (display "")) ; do nothing
          ((eq? l-choice 'q) (run-quit))
          (else (print-menu-error)))
    (if
      (not
        (or
          (eq? l-choice 'q)
          (eq? l-choice 'b)))
        (show-menu-trading)
        (printf "Processing choice...~%")))))

(define run-trading-opt0
  (lambda ()
    (printf "fish /usr/local/sh/trading_ods.fish~%")
    (newline)
    (sleep 1)))

(define run-trading-opt1
  (lambda ()
    (writeln "Start VirtualBox (gui) if it is not running.Start the winny VM...")
    (writeln "")
    (sleep 1)))

;TODO: Write open-url function
(define run-trading-opt2
  (lambda ()
    (open-url "firefox \"http://www.whselfinvest.com/nl/CFD_Market_Information_Sheets.php?sheet=3\"")
    (sleep 1)))

(define run-trading-opt3
  (lambda ()
    (open-url "firefox \"http://www.whselfinvest.com/nl/expiring_markets.php\"")
    (sleep 1)))
