#lang racket
(define (undup str)
  (list->string (remove-duplicates (string->list str))))

(display (undup (read-line)))
