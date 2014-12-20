#lang racket

(define jumps (map string->number (string-split (read-line))))

(define (gcd-pair a b)
  (if (= 0 b)
      a
      (gcd-pair b (remainder a b))))

(define (lcm-pair a b)
  (/ (* a b) (gcd-pair a b)))

(define (lcm lst)
  (foldl lcm-pair 1 lst))

(display (lcm jumps))
