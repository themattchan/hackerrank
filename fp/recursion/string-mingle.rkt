#lang racket
(let* ([a (string->list (read-line))]
       [b (string->list (read-line))]
       [zip (map cons a b)])
  (display
   (list->string
    (foldl
     (lambda (x a)
       (append a (list (car x) (cdr x))))
     '()
     zip))))
