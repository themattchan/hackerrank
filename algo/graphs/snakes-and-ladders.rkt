#lang racket
(require data/heap)

;; Dijkstra's algorithm for adjacency lists
;; Graph is association list of (node . nodes)
(define (dijkstra graph start)
  (define graph-nodes
    (map car graph))
  
  (define dist
    (make-vector 110 +inf.0))
  
  (define (node<=? n1 n2)
    (<= (vector-ref dist n1) (vector-ref dist n2)))
  
  (define (dijkstra1 heap)
    (when (not (zero? (vector-length heap)))    
      (let*-values
          (((first rest) (vector-split-at heap 1))
           ((u) (vector-ref first 0))
           ((distU) (vector-ref dist u))
           ((vs) (cadr (assoc u graph)))) ; neighbours
        (for ((v vs))
          (when (< (add1 distU) (vector-ref dist v))
            (vector-set! dist v (add1 distU))))
        (heap-sort! rest node<=?)
        (dijkstra1 rest))))
  
  (define to-visit (list->vector (range 1 100)))
  (vector-set! dist start 0)
  (heap-sort! to-visit node<=?)
  (dijkstra1 to-visit)
  dist
  )

(define (assoc-with-default d k alist)
  (let ((v? (assoc k alist)))
    (if (false? v?) d
        (cdr v?))))

(define (gen-board snake-ladders)

  (define (reachable n)
    (define (step-maybe-jump x)
      (assoc-with-default (+ x n) (+ x n) snake-ladders))
    (filter (curryr <= 100) (map step-maybe-jump (range 1 7))))

  (map (λ (n) (list n (reachable n)))
       (range 1 101)))

(define (read-snakes-ladders)
    (let ((ss (for/list ((_ (read))) (cons (read) (read))))
          (ls (for/list ((_ (read))) (cons (read) (read)))))
      (append ss ls)))

(define (guard-invalid x)
  (if (= x +inf.0) -1 x))

(for ((i (read)))
  (let* ((board (gen-board (read-snakes-ladders)))
         (dist (dijkstra board 1)))
  (displayln 
   (guard-invalid 
    (vector-ref dist 100)))))