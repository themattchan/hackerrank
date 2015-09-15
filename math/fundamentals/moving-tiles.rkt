#lang racket

(let* ((L (read))
       (vs `(,(read) ,(read)))
       (v1 (apply min vs))
       (v2 (apply max vs))
       
       (calc-time
        (lambda (a) 
          (/ (* (- (sqrt a) L)
                (sqrt 2))
             (- v1 v2)))))
       
  (for ((i (read)))
    (let ((ans (calc-time (read))))
      (displayln (~r ans #:precision 4)))))
