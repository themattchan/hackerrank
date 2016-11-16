#lang racket

(define ROWS 63)
(define COLS 100)

(define (pad l) (make-string l #\_))

(define (repeat e n) (build-list n (const e)))

; depth 1 -> 16
; depth 2 -> 8
; depth 3 -> 4
; depth 4 -> 2
; depth 5 -> 1

(define (centre width rows)
  (map (λ (r)
         (let* ((rl (string-length r))
                (pl (floor (/ (- width rl) 2)))
                (pad1 pl)
                (pad2 (if (< (+ rl pl pl) width) (add1 pl) pl)))
           (string-append (pad pad1) r (pad pad2))))
       rows))

(define (sep-width height) (* 2 height))

; draw one tree. height denotes height given by depth
(define (draw-tree1 height)

  (define (draw-branch h)
    (if (zero? h) '()
        (cons (string-append "1" (pad (sub1 (sep-width h))) "1")
              (draw-branch (sub1 h)))))

  (define (draw-base h) (repeat "1" h))

  (append (draw-branch height)
          (draw-base height)))


; horizontally compose two trees
; assume length tree1 == length tree2
(define (hcat height                    ; height of parent
              tree1 tree2)
  (define SEP (sep-width height))
  (map (λ (t1 t2)
         (string-append t1
                        (pad (- SEP (string-length t1)))
                        t2))
       tree1 tree2))

(define (vcat tree1 tree2) (append tree1 tree2))

; draw the main thing
(define (draw-tree depth)
  (define (go depth height)
    (if (= depth 1)
        (draw-tree1 height)
        (let ((subtree (go (sub1 depth) (/ height 2))))
          (vcat (hcat height subtree subtree)
                (draw-tree1 height)))))

  (let ((pict (centre COLS (go depth 16))))

    (append (repeat (pad COLS) (- ROWS (length pict)))
            pict)))


; runner
(void (map displayln (draw-tree (read))))
