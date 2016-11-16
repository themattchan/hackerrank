#lang racket
(require math/number-theory)

(for ((_ (read)))
  (displayln (length (divisors (gcd (read) (read))))))
