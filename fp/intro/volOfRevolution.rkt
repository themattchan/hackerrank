#lang racket
;; Enter your code here. Read input from STDIN. Print output to STDOUT
;; first line: n num of coefficients
;; second line: n num of powers
;; thrid line: limits

;; Read from stdin
(define (get-num-input) (map string->number (string-split (read-line))))
(define coeffs (get-num-input))
(define powers (get-num-input))
(define limits (get-num-input))

;; ;; Dummy values for test
;; (define coeffs '(1 2 3 4 5))
;; (define powers '(6 7 8 9 10))
;; (define limits '(1 4))

;; zipped list
(define coeff-pows (map cons coeffs powers))

;; The sum (a1)x^b1 + (a2)x^b2 + (a3)x^b3 ......(an)x^bn
(define (combo x)
  ;; Takes a pair of (coeff . pow) -> (c)x^pow
  (define (make-fx cp-pair)
    (lambda (x)
      (* (car cp-pair) (expt x (cdr cp-pair)))))
  (foldl + 0
         (map (lambda (proc) (apply proc (list x)))
              (map make-fx coeff-pows))))

(define (generic-integrate limits function)
  (let ([llim (car limits)]
        [ulim (cadr limits)])
    (define (go acc x)
      (if (> x ulim) acc
          (go (+ acc (* 0.001 (function x)))
              (+ 0.001 x))))
    (go 0 llim)))

(define (integrate limits) (generic-integrate limits combo))
(define (area limits)
  (generic-integrate
   limits
   (lambda (x)
     (* (combo x) (combo x) pi))))


(display (~r (integrate limits) #:precision 2))
(newline)
(display (~r (area limits) #:precision 2))
