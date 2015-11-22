#lang racket

(define (digits n)
  (if (zero? n) '()
      (cons (remainder n 10) (digits (quotient n 10)))))

(for ((i (read)))
  (let ((n (read)))
    (displayln
     (foldl 
      (λ (d a)
        (if (and (not (zero? d))
                 (zero? (remainder n d)))
            (add1 a)
            a))
      0
      (digits n)))))