#lang racket

(void (read-line))
(let ((sticks (map string->number (string-split (read-line)))))
  (define (cut-sticks sticks)
    (when (not (null? sticks))
        (let* ((m (apply min sticks))
               (sticks1
                (foldl (λ (s a)
                         (if (> (- s m) 0)
                             (cons s a)
                             a))
                       '()
                       sticks)))
          (displayln (length sticks))
          (cut-sticks sticks1))))
  (cut-sticks sticks))