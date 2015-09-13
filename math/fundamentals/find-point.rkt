#lang racket

(define (vector p q)
  (map - q p))

(define (smult c v)
  (map (curry * c) v))

(for ([i (read)])
  (let* ([p `(,(read) ,(read))]
         [q `(,(read) ,(read))]
         [v (vector p (smult 2 q))])    
    (displayln (string-join (map ~a v)))))                         
