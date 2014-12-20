#lang racket
(require math)

(define (gcd-pair a b)
 (let ([a-facs (apply hash (flatten (factorize a)))]
       [b-facs (apply hash (flatten (factorize b)))]
       [factors (set (cons (prime-divisors a) (prime-divisors b)))])
   (for/list ([i factors])
     (* i (min (hash-ref a-facs i) (hash-ref b-facs i))))
   
   ))
   

(define (gcd lst)
  (foldl gcd-pair
         (car lst)
         (cdr lst)))
  
(for ([i (string->number (read-line))])
  (let* ([lst (map string->number (string-split (read-line)))]
		 [g (gcd lst)])
	(display g)
	(display " ")))
