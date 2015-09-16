#lang racket

(define (is0? c)
  (eq? #\0 c))

(define (is1? c)
  (eq? #\1 c))

(define (pow2 n)
  (expt 2 n))

; "binary" with strings, is this faster?
(define (and-product lo hi)
  
  (define hs 
    (string->list (number->string hi 2)))
  
  (define ls 
    (string->list
     (let ((ls1 (number->string lo 2)))
       (string-append 
        (make-string (- (length hs)
                        (string-length ls1))
                     #\0)
        ls1))))

    (define (go a msb hs ls)
      (cond
        ; whole number checks out
        ((null? hs) a)
        
        ; two 0s, continue
        ((and (is0? (car hs)) (is0? (car ls)))
         (go a (sub1 msb) (cdr hs) (cdr ls)))
        
        ; two 1s, add msb
        ((and (is1? (car hs)) (is1? (car ls)))
         (go (+ a (pow2 msb)) (sub1 msb) (cdr hs) (cdr ls)))
        
        ; 0 1 pair, return
        (else a)))
 
  (go 0 (sub1 (length hs)) hs ls))

(for ((i (read)))
     (displayln (and-product (read) (read))))

; a very constant 0.25s for every test case!