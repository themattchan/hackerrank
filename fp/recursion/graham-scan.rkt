#lang racket
(require test-engine/racket-tests)
(require math)

(define (read-points)
  (remove-duplicates
   (for/list ([i (read)])
     (cons (read) (read)))))

;; distance between two points
(define/match (dist p1 p2)
  [(`(,x1 . ,y1) `(,x2 . ,y2))
   (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1))))])
  
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


;; pick lowest y, then lowest x if tie.
(define/match (find-min pt1 pt2)
  [(`(,x1 . ,y1) `(,x2 . ,y2))
    (if (= y1 y2) 
        (< x1 x2)
        (< y1 y2))])


(define (compare-angle min)
  (lambda (p1 p2)
    (match-let*
        ([`(,xm . ,ym) min]
         [`(,x1 . ,y1) p1]
         [`(,x2 . ,y2) p2]
         
         [min-pt-theta 
          (λ (x y)
            (let ((quad (- x xm)))  ; quadrant 1 or 2
              (cond
                [(< 0 quad) (atan (/ (- y ym) quad))]
                [(= 0 quad) (/ pi 2)]
                [(> 0 quad) (+ pi (atan (/ (- y ym) quad)))])))]
         
         [theta1 (min-pt-theta x1 y1)]
         [theta2 (min-pt-theta x2 y2)])      
      
      (cond
        [(= theta1 theta2) 
         (< (dist min p1) (dist min p2))]
        
        [else
         (< theta1 theta2)]))))        


(define (compare-angle-origin min)
  (lambda (p1 p2)
    (let* ((calc-theta ; absolute angle relative to origin
            (λ (p)
              (if (= 0 (car p))
                  (/ pi 2)
                  (atan (/ (cdr p) ( car p))))))
           
           (minT (calc-theta min))
           (p1T  (calc-theta p1))
           (p2T  (calc-theta p2))
           (d1 (- minT p1T))
           (d2 (- minT p2T)))
            
      (cond
        [(= d1 d2) (< (dist min p1) (dist min p2))]
        ;; always pick *biggest* number
        [else      (> d1 d2)]))))


;; sort points s.t. you can trace ccw from min.
;; idea: sort ascending the points to R of min and
;;       descending the points to L of min.
(define (compare-circle min)
  (lambda (p1 p2)
    (let* ((divider (car min))
           (p1-part (>= (car p1) divider))
           (p2-part (>= (car p2) divider)))
      
      (match `(,p1-part ,p2-part)
        ['(#t #t)
         (if (= (cdr p1) (cdr p2)) 
             (> (car p1) (car p2))
             (< (cdr p1) (cdr p2)))]
        
        ['(#f #f)
         (if (= (cdr p1) (cdr p2)) 
             (< (car p1) (car p2))
             (> (cdr p1) (cdr p2)))]
        
        [_ p1-part]))))


(define (graham-scan pts)
  (match-let*
      ([`(,minY ,rest ...) (sort pts find-min)]
       [sorted-pts         (cons minY (sort rest (compare-circle minY)))])
    
    (define (remove-concave pts)
      (if (> 3 (length pts)) pts
          
          (let ((a (car pts))
                (b (cadr pts))
                (c (caddr pts))
                (r (cdddr pts)))
            
            (case (turn? (ccw a b c))           
              ['R (remove-concave `(,a ,c ,@r))]
              [else (cons a (remove-concave (cdr pts)))]))))
    
    (remove-concave sorted-pts)))


(define (perimeter pts)      
  (if (> (length pts) 2)
      (let ([permute `(,@(cdr pts) ,(car pts))])
        (sum (map dist pts permute)))
      0))



;;; Test on sample inputs

(check-within (perimeter 
               (graham-scan `(,`(1 . 1) 
                              ,`(2 . 5) 
                              ,`(3 . 3)
                              ,`(5 . 3)
                              ,`(3 . 2) 
                              ,`(2 . 2))))
              12.2 0.05)


(check-within (perimeter 
               (graham-scan `(,`(3 . 2) 
                              ,`(2 . 5) 
                              ,`(4 . 5))))
              8.3 0.05)



(test)


;; Run
;(graham-scan (read-points))


(let ((points (read-points)))
  (displayln (real->decimal-string (perimeter (graham-scan points)) 1)))
