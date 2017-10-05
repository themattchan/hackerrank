#lang racket

(for ((i (read)))
  (let* ((n (read))
         (k (read))
         (times (for/list ((_ n)) (read))))
    (if (> k (length (filter (compose not positive?) times)))
        (displayln "YES")
        (displayln "NO"))))
