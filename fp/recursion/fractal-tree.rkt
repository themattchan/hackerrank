#lang racket

(define ROWS 63)
(define COLS 100)

(define (is-centred? tree)
  (check-duplicates (map string-length tree))

; depth 1 -> 16
; depth 2 -> 9
; depth 3 -> 5
; depth 4 -> 3
; depth 5 -> 1
(define (centre width rows)
  (map (λ (r)
         (let* ((pl (/ (- width (string-length r)) 2))
                (pad (make-string pl #\_)))
           (string-append pad r pad)))
       rows))

; draw one tree. height denotes height given by depth
(define (draw-tree1 height)
  (define h1 (/ height 2))

  (define (draw-branch h)
    (if (zero? h) '()
        (cons `(,(string-append "1"
                                (make-string (sub1 (* 2 h)) #\_)
                                "1"))
          (draw-branch (sub1 h)))))

  (define (draw-base h) (build-list h "1"))

  (append (draw-branch h1)
          (draw-base h1))
  )

; horizontally compose two trees
(define (hcat tree tree) ...)

; draw the main thing
(define (draw-tree depth) ...)
