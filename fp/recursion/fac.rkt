#lang racket
(define (fib n)
  (cond
   ((= n 1) 0)
   ((= n 2) 1)
   (else
    (+ (fib (- n 1)) (fib (- n 2))))))

(let ([n (string->number (read-line))])
  (display (fib n)))
