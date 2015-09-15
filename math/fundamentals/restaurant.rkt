#lang racket

(for ((i (read)))
  (let ((a (read))
        (b (read)))
    (displayln
     (/ (* a b)
        (sqr (gcd a b))))))