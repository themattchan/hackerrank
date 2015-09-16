#lang racket

; a xor a = 0
; if there is only one number that shows up once, xoring all numbers will
; return it
;
; Complexity: O(n)

(define (go n)
  (if (zero? n)
      0
      (bitwise-xor (read) (go (sub1 n)))))

(go (read))
