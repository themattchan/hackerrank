#lang racket
(require test-engine/racket-tests)
(require math)

;; Convex hull - Monotone chain algorithm
;; http://en.wikibooks.org/wiki/Algorithm_Implementation/Geometry/Convex_hull/Monotone_chain

(define (read-points)
  (remove-duplicates
   (for/list ([i (read)])
     (cons (read) (read)))))

;; distance between two points
(define/match (dist p1 p2)
  [(`(,x1 . ,y1) `(,x2 . ,y2))
   (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1))))])


;; Left turn, Right turn, Collinear

; ccw = (p2.x - p1.x)*(p3.y - p1.y) - (p2.y - p1.y)*(p3.x - p1.x)
(define/match (ccw p1 p2 p3)
  [(`(,x1 . ,y1) `(,x2 . ,y2) `(,x3 . ,y3))
    (- (* (- x2 x1) (- y3 y1))
       (* (- y2 y1) (- x3 x1)))])


(define (turn? n)
  (cond
    [(< n 0) 'R]
    [(> n 0) 'L]
    [(= n 0) 'C]))


;; sort by min x and min y
(define/match (sorter p1 p2)
  [(`(,x1 . ,y1) `(,x2 . ,y2))
   (if (= x1 x2)
       (< y1 y2)
       (< x1 x2))])


(define (lower-hull pts) 
  (define (go acc pts) 
    (match acc
      [`(,a ,b ,xs ...)
       (if (not (eq? 'L (turn? (ccw a b (car pts)))))
           (go (cdr acc) pts)
           (go (cons (car pts) acc) (cdr pts)))
      
       ])
    
  (go '() pts))

(define (upper-hull pts) (lower-hull (reverse pts)))

(define (compute-hull pts)
  (let ((pts (sort pts sorter)))
  `(,@(init (lower-hull pts)) ,@(init (upper-hull pts)))))

(define (perimeter pts)      
  (if (> (length pts) 2)
      (let ([permute `(,@(cdr pts) ,(car pts))])
        (sum (map dist pts permute)))
      0))

  
  
  ;;; Test on sample inputs
  
  (check-within (perimeter 
                 (compute-hull `(,`(1 . 1) 
                                 ,`(2 . 5) 
                                 ,`(3 . 3)
                                 ,`(5 . 3)
                                 ,`(3 . 2) 
                                 ,`(2 . 2))))
                12.2 0.05)
  
  
  (check-within (perimeter 
                 (compute-hull `(,`(3 . 2) 
                                 ,`(2 . 5) 
                                 ,`(4 . 5))))
                8.3 0.05)
  
  
  
  (test)
  
  
  ;; Run
  (let ((points (read-points)))
    (displayln (real->decimal-string (perimeter (compute-hull points)) 1)))
  