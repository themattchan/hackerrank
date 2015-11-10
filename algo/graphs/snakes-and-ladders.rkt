#lang racket

(define maximum
  (curry argmax identity))

(define (possible-steps cur)
  (map (curry + cur) (range 1 7)))


(define (step-to m n)
  (hash-ref m n n))  
      

(define (play m n)
  (if (>= n 100)
      0
      (+ 1 (play m 
                 (maximum (map (curry step-to m) 
                               (possible-steps n)))))))

(for ((i (read)))
  (let ((ss 
         (for/list ((s (read)))
           (cons (read) (read))))
        (ll 
          (for/list ((s (read)))
           (cons (read) (read)))))
    (let ((m (make-hash (append ss ll))))
      (displayln (play m 0)))))
    