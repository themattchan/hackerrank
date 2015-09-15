#lang racket
(require rnrs/arithmetic/bitwise-6)

(define (MSB-exp n)
  (- (integer-length n) 1))

(define (go d pows)
  (if (or (null? pows)
          (> d (car pows)))
      '(0)
      (cons (car pows) (go d (cdr pows)))))

(define (and-product lo hi MSB-exp)
  (let* ((es (for/list ((i (in-range MSB-exp 0 -1))) i))
         (pow2 (map (curry expt 2) es))
         (diff (- hi lo)))    
    (apply + (go diff pow2))))


(for ((i (read)))
  (let ((lo (read))
        (hi (read)))
    (displayln (and-product lo hi (MSB-exp hi)))))