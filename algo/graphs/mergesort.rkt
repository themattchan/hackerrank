#lang racket

(define (split lst) 
  (cond 
    ((null? lst) '(() ()))
    ((null? (cdr lst)) `((,(car lst)) ()))
    (else 
     (let ((ss (split (cddr lst))))
       `(,(cons (car lst) (car ss))
         ,(cons (cadr lst) (cadr ss)))))))

(define (merge xs ys #:cmp [cmp <])
  (cond
    ((null? xs) ys)
    ((null? ys) xs)
    ((cmp (car xs) (car ys))
     (cons (car xs) (merge (cdr xs) ys #:cmp cmp)))
    (else 
     (cons (car ys) (merge xs (cdr ys) #:cmp cmp)))))

(define (mergesort lst #:cmp [cmp <])
  (cond 
    ((null? lst) lst)
    ((null? (cdr lst)) lst)
    (else 
     (let ((splits (split lst)))
       (merge (mergesort (car splits) #:cmp cmp)
              (mergesort (cadr splits) #:cmp cmp)
              #:cmp cmp)))))