#lang racket

(define (pascal-at r c)
  (if (or (zero? c) (= c r))
      1
      (+ (pascal-at (sub1 r) (sub1 c))
         (pascal-at (sub1 r) c))))

(let ([r (string->number (read-line))])
  (for ([i (in-range r)])
    (for ([j (in-range (add1 i))])
      (display (pascal-at i j))
      (display " "))
    (newline)))
