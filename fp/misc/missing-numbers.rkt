#lang racket

(define ns (make-vector 100 (cons null 0)))

(define (do-row action)
  (for/last ((_ (read)))
    (let* ((v (read))
           (r (modulo v 100))
           (n (vector-ref ns r)))
      (vector-set! ns r (cons (if (null? (car n))
                                  (- v r)
                                  (car n))
                              (action (cdr n)))))))

(let ((a (do-row add1))
      (b (do-row sub1)))
  (for-each (λ (e) (printf "~a " e))
            (sort
             (for/list (((n i) (in-indexed ns))
                        #:when (< (cdr n) 0))
               (+ (car n) i)) <)))
