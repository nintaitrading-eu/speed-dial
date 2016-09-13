#lang racket

(provide
  print-header
  print-menu-error
  display-prompt
  open-url
  run-quit)

(require "constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; print-header:
; Prints a formatted header, with the given title.
; TODO: make length of underlining dynamic.
(define (print-header a-title)
  (fprintf (current-output-port) "~a\n" a-title)
  (fprintf (current-output-port) "-----------------------------\n"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (printf "Bye.")
    (exit)))
