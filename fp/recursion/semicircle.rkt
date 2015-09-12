#lang racket
(require test-engine/racket-tests)
(require math)

(define (read-points)
  (remove-duplicates
   (for/list ([i (read)])
     (cons (read) (read)))))

;;; Convex hull - Semicircle method
;;; A functional variation of the Graham Scan

;; distance between two points
(define/match (dist p1 p2)
  [(`(,x1 . ,y1) `(,x2 . ,y2))
   (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1))))])
  
;; pick lowest y, then lowest x if tie.
(define/match (find-min pt1 pt2)
  [(`(,x1 . ,y1) `(,x2 . ,y2))
    (if (= y1 y2) 
        (< x1 x2)
        (< y1 y2))])

(define (semicircle pts)
  (match-let*-values
   (; 1. find min
    [(`(,minY ,rest ...))         
     (sort pts find-min)]
    
    ; 2. partition
    [(left right) 
     (partition rest (λ (p) (<= (car p) (car minY))))])
   
   (define increasing 
     (let ((xs (sort right )
   
   (define decreasing ...)
   
   
   
    
    
(define (perimeter pts)      
  (if (> (length pts) 2)
      (let ([permute `(,@(cdr pts) ,(car pts))])
        (sum (map dist pts permute)))
      0))



;;; Test on sample inputs

(check-within (perimeter 
               (semicircle `(,`(1 . 1) 
                              ,`(2 . 5) 
                              ,`(3 . 3)
                              ,`(5 . 3)
                              ,`(3 . 2) 
                              ,`(2 . 2))))
              12.2 0.05)


(check-within (perimeter 
               (semicircle `(,`(3 . 2) 
                              ,`(2 . 5) 
                              ,`(4 . 5))))
              8.3 0.05)



(test)


;; Run
;(graham-scan (read-points))


(let ((points (read-points)))
  (displayln (real->decimal-string (perimeter (graham-scan points)) 1)))
