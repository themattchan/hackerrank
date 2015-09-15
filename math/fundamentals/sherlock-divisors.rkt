#lang racket
(require math/number-theory)

(define (even-factors n)
  (length (filter even? (divisors n))))

(for ((i (read)))
  (displayln (even-factors (read))))

