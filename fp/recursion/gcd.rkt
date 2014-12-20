#lang racket
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(let* ([args (map string->number (string-split (read-line)))]
       [a (car args)]
       [b (cadr args)])
  (display (gcd a b)))
