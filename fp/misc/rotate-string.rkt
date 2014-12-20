#lang racket
(define lines
  (for/list ([i (string->number (read-line))])
    (read-line)))

(define (rotate str)
  (define (rotate-left n lst)
    (if (= 0 n)
        lst
        (rotate-left
         (- n 1)
         (if (null? lst)
             '()
             (append (cdr lst)
                     (list (car lst)))))))
  (let ([chars (string->list str)])
    (map list->string
         (for/list ([i (length chars)])
           (rotate-left (+ 1 i) chars)))))

(for ([i (map string-join (map rotate lines))])
  (display i)
  (newline))
