#lang racket

;; (int int) -> (int . int)
(define (make-pt lst) (cons (car lst) (cadr lst)))
(define (get-x pt) (car pt))
(define (get-y pt) (cdr pt))

(define points
  (for/list ([i (string->number (read-line))])
    (make-pt (map string->number (string-split (read-line))))))

(define (graham-scan points)
