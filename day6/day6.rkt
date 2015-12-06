#lang racket

(require data/bit-vector
         racket/match)

(define width  1000)
(define height 1000)

(define lights (make-bit-vector (* width height)))

(define (calc-pos x y) (+ x (* y width)))

(define (turn-on pos)
  (bit-vector-set! lights pos #t))
(define (turn-off pos)
  (bit-vector-set! lights pos #f))
(define (toggle pos)
  (bit-vector-set! lights pos (not (bit-vector-ref lights pos))))

(define (process-square positions operation)
  (for ([y (in-range (cadr positions) (add1 (cadddr positions)))])
    (for ([x (in-range (car positions) (add1  (caddr positions)))])
      (let ([pos (calc-pos x y)])
        (operation pos)))))

(define (switch-lights in)
  (let ([line (read-line in)])
    (if (eof-object? line)
        (bit-vector-popcount lights)
        (let ([positions (map string->number
                              (regexp-match* #rx"[0-9]+" line))]
              [operation (regexp-match #rx"[a-z]+ [a-z]*" line)])
          (process-square positions (match operation
                                      ['("turn on") turn-on]
                                      ['("turn off") turn-off]
                                      ['("toggle ")  toggle]))
          (switch-lights in)))))

(call-with-input-file "day6input"
  (λ (in-port)
    (switch-lights in-port)))
