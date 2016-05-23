#lang racket
(require xml
         xml/path)

(define (parse-query s)
  (define spl (string-split s "~"))
  `(root
    ,@(map string->symbol (string-split (car spl) "."))
    ,@(map string->keyword (cdr spl))))

(define (read-xml lines)
  (string->xexpr
   (apply string-append
          `("<root>"
            ,@(for/list ((i (range 0 lines))) (read-line))
            "</root>"))))

(let ((lines (read))
      (queries (read)))
  (read-line)   ; clear trailing newline gunk
  (let ((doc (read-xml lines)))
    (for ((_ (range 0 queries)))
      (let ((found (se-path* (parse-query (read-line)) doc)))
        (if found
            (displayln found)
            (displayln "Not Found!"))))))
