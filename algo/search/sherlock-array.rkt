#lang racket

(define (partitions as)
  (define (step l r lst)
    (cond
      [(null? lst) "NO"]
      [(= l (- r (car lst))) "YES"]
      [else (step (+ l (car lst)) (- r (car lst)) (cdr lst))]))
  (step 0 (foldl + 0 as) as))

(for [(i (read))]
  (displayln (partitions
              (for/list ([j (read)])
                (read)))))
