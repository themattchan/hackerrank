#lang racket

(define ns (make-vector 100 (cons 0 0)))

(define (do-row action)
  (for/last ((_ (read)))
    (let*-values (((q r) (quotient/remainder (read) 100))
                 ((n) (vector-ref ns r)))
      (vector-set! ns r (cons (* 100 q) (action (cdr n)))))))

(let ((a (do-row add1))
      (b (do-row sub1)))
  (for (((n i) (in-indexed ns))
             #:when (< (cdr n) 0))
    (printf "~a " (+ (car n) i))))
             