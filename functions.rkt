#lang racket

(provide
  print-header
  display-prompt
  open-url)

(define print-header
  (lambda ()
    (printf "speed-dial v1.0~%")
    (printf "---------------~%")))

(define display-prompt
  (lambda ()
    (printf "> ")))

(define open-url
  (lambda (a-url)
    (printf "firefox ~a~%" a-url)))
