#lang racket
(require data/heap)

;; Dijkstra's algorithm for adjacency lists
;; Graph is association list of (node . nodes)
(define (dijkstra graph start)
  (define graph-nodes 
    (map cdr graph))
  
  (struct heap-node (vertex [dist #:mutable])
    #:transparent)
  
  (define (node<=? n1 n2)
    (<= (heap-node-dist n1) (heap-node-dist n2)))
  (define to-visit (make-heap node<=?))
  
  (heap-add-all! 
   to-visit 
   (cons (heap-node start 0) ; start node is 1, set to 0, everything else is inf
         (map (λ (id) (heap-node id +inf.0)) (remove start graph-nodes))))
  
  (define (dijkstra1 heap)
    (if (= 0 (heap-count heap))
        '()
        ;; finalise the min elem. Visit its children and remove from heap
        (let ((minE (heap-min heap)))
          
        (cons (heap-min heap)
         (dijkstra1 (heap-remove-min! ))
    )
  
  (dijkstra1 to-visit)
  )


(define (assoc-with-default d k alist)
  (let ((v? (assoc k alist)))
    (if (false? v?) d
        (cdr v?))))

(define (gen-board snake-ladders)
  (define (reachable n)
    
    (define (step-maybe-jump x) 
      (assoc-with-default (+ x n) (+ x n) snake-ladders))
    
    (define (lte-100 x) (<= x 100))
    
    (filter lte-100
            (map step-maybe-jump
                 (range 1 7))))
  
  (map (λ (n) (list n (reachable n)))
       (range 1 101)))

(define (read-snakes-ladders)
    (let ((ss (for/list ((_ (read))) (cons (read) (read))))
          (ls (for/list ((_ (read))) (cons (read) (read)))))
      (append ss ls)))

;(define board (gen-board (read-snakes-ladders)))

#|
(for ((i (read)))
  (let ((board (gen-board (read-snakes-ladders))))
    (displayln (dijkstra board 1))))
|#
