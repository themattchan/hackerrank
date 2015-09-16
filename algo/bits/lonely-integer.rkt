#lang racket

; this is O(nlogn + n) because of the sort
(define nums
  (sort (for/list ([i (read)]) (read))
        <))

(car (foldl
      (lambda (x a)
        (cond
          [(null? a) (cons x a)]
          [(= x (car a)) (cdr a)]
          [true (cons x a)]))
      '()
      nums))
