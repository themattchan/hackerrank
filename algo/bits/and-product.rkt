#lang racket

(define (MSB-exp n)
  (- (integer-length n) 1))

(define (pow2 e)
  (expt 2 e))

(define (and-product hi lo)
  (define (go hi lo a)
    (let* ((ehi (MSB-exp hi))
           (elo (MSB-exp lo))
           (p   (pow2 ehi)))
      (if (or (and (zero? hi) (zero? lo))
              (not (eq? ehi elo)))
          a
        (go (- hi p) (- lo p) (+ a p)))))
  (go hi lo 0))

(for ((i (read)))
  (let ((lo (read))
        (hi (read)))
    (displayln (and-product lo hi))))