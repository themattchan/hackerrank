#lang racket
(define (height n)
  (cond
    [(= 0 n) 1]
    [(= 1 (remainder n 2)) (* 2 (height (sub1 n)))]
    [true (add1 (height (sub1 n)))]))

(for ([i (read)])
  (displayln (height (read))))