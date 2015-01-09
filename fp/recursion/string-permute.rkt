#lang racket
;; Attempt 1: purely functional, but too slow. Wasted cycles converting to a
;; list and back. Now if this was Haskell...
(define words
  (for/list ([i (string->number (read-line))])
    (read-line)))

(define (swap str)
  (define (go acc lst)
    (if (null? lst)
        acc
        (go (append acc (list (cadr lst) (car lst)))
            (cddr lst))))
  (list->string (go '() (string->list str))))

(for ([i (map swap words)])
  (display i)
  (newline))

;; Attempt 2: Mutate the string. But it feels sooooooo dirty.
(define words
  (for/list ([i (string->number (read-line))])
    (read-line)))

(define (swap str)
  (let ([len (string-length str)])
    (for ([i (in-range 0 len 2)]
          [j (in-range 1 len 2)])
      (let ([tmp (string-ref str i)])
        (string-set! str i (string-ref str j))
        (string-set! str j tmp)))))

(for-each swap words)                   ; Mutate each string in words
(for ([i words])
  (display i)
  (newline))

;; Attempt 3: Operate on the IO stream itself. Purely functional and without any
;; memory overhead.
(define (stream-swap)
  (let ([a (read-char)])
    (if (or (char=? a #\newline)
            (eof-object? a))
        (display #\newline)             ; Stop at end of word
        (begin
          (display (read-char))         ; Read next char too
          (display a)
          (stream-swap)))))

(for ([i (string->number (read-line))]) ; Process # of words
  (stream-swap))

;; Attempt 4: Even simpler, just process the whole stream, instead of by line.
(define (stream-swap)
  (let ([a (read-char)])
    (cond [(eof-object? a) (newline)]   ; Stop at EOF instead
          [(char=? a #\newline)
           (newline)
           (stream-swap)]
          [else
           (display (read-char))        ; Read next char too
           (display a)
           (stream-swap)])))

(void (read-line))                      ; Remove number (first line)
(stream-swap)                           ; Go
