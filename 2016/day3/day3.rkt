#lang racket

(define input (map (λ (in) (map string->number (string-split in)))
                   (file->lines "day3.input")))

(define (check-triangle triangle)
  (match-let ([(list a b c) triangle])
    (and (> (+ a b) c)
         (> (+ b c) a)
         (> (+ c a) b))))

(displayln "Part 1:")
(count identity (map check-triangle input))

(define (check-3-triangles row1 row2 row3)
  (let* ([tri1 (map first (list row1 row2 row3))]
         [tri2 (map second (list row1 row2 row3))]
         [tri3 (map third (list row1 row2 row3))])
    (count identity (map check-triangle (list tri1 tri2 tri3)))))

(define (check-rotated-triangles input)
  (define (loop input num-triangles)
    (if (empty? input)
        num-triangles
        (loop (cdddr input) (+ num-triangles (check-3-triangles (first input)
                                                                (second input)
                                                                (third input))))))
    (loop input 0))

(displayln "Part 2:")
(check-rotated-triangles input)
