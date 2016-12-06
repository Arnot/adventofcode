#lang racket

(define input (file->lines "day6.input"))

;; From day4
(define (count-letters str)
  (define counts (make-hash))
  (for ([c (in-string str)])
    (hash-set! counts c (add1 (hash-ref counts c 0))))
  counts)

(define (letters lst i)
  (list->string
   (map (λ (str) (string-ref str i)) input)))

(displayln "Part 1")
(for ([i (in-range 0 8)])
  (display (caar
            (sort (hash->list
                   (count-letters (letters input i)))
                  > #:key cdr))))
(displayln "\nPart 2")
(for ([i (in-range 0 8)])
  (display (caar
            (sort (hash->list
                   (count-letters (letters input i)))
                  < #:key cdr))))
