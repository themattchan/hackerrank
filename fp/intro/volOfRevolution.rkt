#lang racket
;; Enter your code here. Read input from STDIN. Print output to STDOUT
;; first line: n num of coefficients
;; second line: n num of powers
;; thrid line: limits

;; Read params from stdin
(define (get-num-input)
  (map string->number (string-split (read-line))))

(define coeffs (get-num-input))
(define powers (get-num-input))
(define limits (get-num-input))

;; ;; Dummy values for test
;; (define coeffs '(1 2 3 4 5))
;; (define powers '(6 7 8 9 10))
;; (define limits '(1 4))

(define (combo x)
  ;; zipped list of coefficients and powers
  (define coeff-pows (map cons coeffs powers))
  ;; Takes a pair of (coeff . pow) -> (c)x^pow
  (define (make-fx cp)
    (lambda (x)
      (* (car cp) (expt x (cdr cp)))))
  ;; The sum (a1)x^b1 + (a2)x^b2 + (a3)x^b3 ......(an)x^bn
  (foldl + 0
         (map (lambda (f) (f x))
              (map make-fx coeff-pows))))

(define (integrate limits f)
  (let ([llim (car limits)]
        [ulim (cadr limits)])
    (define (go acc x)
      (if (> x ulim) acc
          (go (+ acc (* 0.001 (f x)))
              (+ 0.001 x))))
    (go 0 llim)))

(define (area) (integrate limits combo))

(define (volume)
  (integrate
   limits
   (lambda (x)
     (* (combo x) (combo x) pi))))

;; Print the solution
(display (~r (area) #:precision 2))
(newline)
(display (~r (volume) #:precision 2))
