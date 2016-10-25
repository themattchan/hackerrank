#lang racket

(define (sum-of-powers goal power)
  (define (go goal m)
    (let ((m^p (expt m power)))
    (cond
      ((= goal 0) 1)
      ((< goal 0) 0)
      ((< goal m^p) 0)
      (else
       (+ (go goal (add1 m))
          (go (- goal m^p) (add1 m)))))))

  (go goal 1))

(display (sum-of-powers (read) (read)))
