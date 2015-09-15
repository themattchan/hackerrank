#lang racket

(define (staircase n)
  (define (go s h)
    (display (make-string s #\space))
    (display (make-string h #\#))
    (newline)
    (when (not (zero? s))
          (go (sub1 s) (add1 h))))
  (go (sub1 n) 1))

(staircase (read))
