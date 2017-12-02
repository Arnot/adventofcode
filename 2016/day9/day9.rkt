#lang racket

;; (define in (file->list "day9.input"))
(define in (file->list "short.in"))

(define (process-list lst)
  (let* ([vals (map string->number (string-split (symbol->string (car lst)) "x"))]
         [skip (car vals)]
         [amnt (cadr vals)]
         [len (* amnt skip)])
    (values len skip)))

(define (process-string lst)
  (let* ([str (symbol->string lst)]
         [len (string-length str)])
    (values len 0)))

(define (decompress in [skip-marker #f])
  (define (loop input len)
    (let-values ([(decompressed-len skip)
                  (if (and (not (empty? input))
                           (list? (car input)))
                      (process-list (car input))
                      (process-string (car input)))])
      ;; check skip-marker
      (if (empty? (cdr input))
          len
          (loop (cdr input) (+ len decompressed-len)))))

  (loop in 0))

(decompress in)
