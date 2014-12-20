#lang racket
(let* ([a (string->list (read-line))]
       [b (string->list (read-line))]
       [zip (map cons a b)])
  (display
   (list->string
    (foldl
     (lambda (x a)
       (append
        (append a (list (car x)))
        (list (cdr x))))
     '()
     zip))))
