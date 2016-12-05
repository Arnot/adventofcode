#lang racket

(define input (map (λ (in) (map string->number (string-split in)))
                   (file->lines "day3.input")))

(define (check-triangle triangle)
  (match-let ([(list a b c) triangle])
    (and (> (+ a b) c)
         (> (+ b c) a)
         (> (+ c a) b))))

(count identity (map check-triangle input))
