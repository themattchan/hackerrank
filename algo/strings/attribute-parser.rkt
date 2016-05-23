#lang racket
(require xml
         xml/path)

(define parse-query
  (lambda (s)
    (define spl (string-split s #rx"\\.|~"))
    (define-values (path attrib)
      (split-at spl (sub1 (length spl))))
    (append
     (map string->symbol path)
     (list (string->keyword (car attrib))))))

(let ((lines (read))
      (queries (read)))
  (read-line)   ; clear trailing newline gunk
  (let ((doc
         (string->xexpr
          (apply string-append
                 (for/list ((i (range 0 lines)))
                           (read-line))))))
    (for ((_ (range 0 queries)))
      (let ((found (se-path* (parse-query (read-line)) doc)))
      (if found
          (displayln found)
          (displayln "Not Found!"))))))
