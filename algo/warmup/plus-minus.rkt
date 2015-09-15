#lang racket

(define (go c lt z gt)
  (if (zero? c) (values lt z gt)
      (let ((n (read)))
        (cond
          ((negative? n) (go (sub1 c) (add1 lt) z gt))
          ((zero?     n) (go (sub1 c) lt (add1 z) gt))
          ((positive? n) (go (sub1 c) lt z (add1 gt)))))))

(let*-values (((t) (read))
              ((lt z gt) (go t 0 0 0)))
  (displayln (~r (/ gt t)))
  (displayln (~r (/ lt t)))
  (displayln (~r (/ z  t))))
