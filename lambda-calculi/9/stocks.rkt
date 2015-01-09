#lang racket
(define nums 
  (for/list ([i (read)])
    (read)))

(define queries
  (for/list ([i (read)])
    (cons (cons (car (drop nums (read))) (read)) 0)))

(define (inits lst)
  (if (null? lst) '()
      (cons lst (inits (cdr lst)))))

(define (sublists lst)
  (if (null? lst) 
      '()
      (append (inits lst) (sublists (drop-right lst 1)))))

(define (sorted-sublists sub)
  (sort sub (lambda (a b) (> (length a) (length b)))))

(set! nums (sorted-sublists (sublists nums)))

;(define (find-max-sublist tup)
;  (let ([ad (car tup)]
;        [M (cdr tup)])
;    (define (all-in-range lst)
;      (andmap (lambda (x)
;                (and (>= x ad)
;                     (<= x (+ ad M))))
;              lst))
;    (define (go lst)
;      (if (all-in-range (car lst))
;          (length (car lst))
;          (go (cdr lst))))
;    (go nums)))

;
;(define (group tup)
;  (let ([nums1 (map (lambda (x) (- x (car (drop nums (car tup))))) nums)])
;    (apply max (foldl
;                (lambda (x a)
;                  (if (and (>= x 0)
;                           (<= x (cdr tup)))
;                      (if (= 0 (car a))
;                          (cons 1 a)
;                          (cons (add1 (car a)) (cdr a)))
;                      (cons 0 a)))
;                '(0)
;                nums1))))

(define (run-queries)
  (for* ([sublist nums])
     
          
          
(for-each (compose displayln find-max-sublist) queries)