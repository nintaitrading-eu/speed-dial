#lang racket

(provide
  print-header
  print-menu-error
  display-prompt
  open-url
  run-quit)

(require "constants.rkt")

; print-header:
; Prints a hard-coded application header.
(define print-header
  (lambda ()
    (printf "speed-dial v1.0~%")
    (printf "---------------~%")))

; print-menu-error:
; Prints an error message when a given option is unknown.
(define print-menu-error
  (lambda ()
    (printf "Unknown option. Choose wisely.~%")
    (sleep 1)))

; display-prompt:
; Displays a prompt.
(define display-prompt
  (lambda ()
    (printf C-PROMPT)))

; open-url:
; Opens a given url in firefox.
; TODO: Open in default browser.
(define open-url
  (lambda (a-url)
    (printf "firefox ~a~%" a-url)))

; run-quit:
; Quits, with a fancy message.
(define run-quit
  (lambda ()
    (system "clear")
    (writeln "Bye.")
    (exit)))
