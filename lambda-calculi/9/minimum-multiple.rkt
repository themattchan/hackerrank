#lang racket
(require math)
(require racket/vector)

(define MOD-CONST (+ 7 (expt 10 9)))

(void (read-line))                      ; delete first line
(define ARRAY
  (apply vector (map string->number (string-split (read-line)))))

;; (define ARRAY
;;   (let ([size (string->number (read-line))])
;;   (for/vector #:length size
;;               ([i #t])
;;               (read)))

(define (Q a b)
  (let* ([arr (vector-take (vector-drop ARRAY a) (add1 (- b a)))]
         [lcm (apply lcm (vector->list arr))])
    (if (> lcm MOD-CONST)
        (displayln (modulo lcm MOD-CONST))
        (displayln lcm))))

(define (U pos mul)
  (let* ([new-val (* (vector-ref ARRAY pos) mul)])
    (vector-set! ARRAY pos new-val)))

;; Process queries
(for ([i (string->number (read-line))])
  (let ([op (let ([o (read)])
              (cond
               [(eq? 'Q o) Q]
               [(eq? 'U o) U]))]
        [a  (read)]
        [b  (read)])
    (apply op (list a b))))
