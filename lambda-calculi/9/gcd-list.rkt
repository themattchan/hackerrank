#lang racket
(require math)

(define nums
  (for/list ([i (string->number (read-line))])
    (map string->number (string-split (read-line)))))

;; Takes two numbers represented as
;; '(prime-divisor expt p-d expt...). IN INCREASING ORDER
;; Returns a gcd in the same representation
(define (gcd-pair al bl)
  (if (or (null? al) (null? bl))
      '()
      (let ([a-div (car al)]
            [a-exp (cadr al)]
            [b-div (car bl)]
            [b-exp (cadr bl)])
        (cond
         [(< a-div b-div)
          (gcd-pair (drop al 2) bl)]
         [(> a-div b-div)
          (gcd-pair al (drop bl 2))]
         [else (cons a-div
                     (cons (min a-exp b-exp)
                           (gcd-pair (drop al 2)
                                     (drop bl 2))))]))))

(for ([i (foldl gcd-pair
                (car nums)
                (cdr nums))])
  (display i)
  (display " "))
