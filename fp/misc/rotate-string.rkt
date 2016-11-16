#lang racket
(define lines
  (for/list ([i (string->number (read-line))])
    (read-line)))

(define (rotate str)
  (define (rotate-left n lst)
    (if (= 0 n)
        lst
        (rotate-left
         (sub1 n)
         (if (null? lst)
             '()
             (append (cdr lst)
                     (list (car lst)))))))
  (let ([chars (string->list str)])
    (map list->string
         (for/list ([i (length chars)])
           (rotate-left (add1 n) chars)))))

(for ([l lines])
  (displayln (string-join (rotate l))))
