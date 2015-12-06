#lang racket

(require data/bit-vector
         racket/match)

(define width  1000)
(define height 1000)

(define lights (make-vector (* width height)))

(define (calc-pos x y) (+ x (* y width)))

(define (turn-on pos)
  (vector-set! lights pos (add1 (vector-ref lights pos))))
(define (turn-off pos)
  (vector-set! lights pos (max 0 (sub1 (vector-ref lights pos)))))
(define (toggle pos)
  (vector-set! lights pos (+ 2 (vector-ref lights pos))))

(define (process-square positions operation)
  (for ([y (in-range (cadr positions) (add1 (cadddr positions)))])
    (for ([x (in-range (car positions) (add1  (caddr positions)))])
      (let ([pos (calc-pos x y)])
        (operation pos)))))

(define (switch-lights in)
  (let ([line (read-line in)])
    (if (eof-object? line)
        (foldl + 0 (vector->list lights))
        (let ([positions (map string->number (regexp-match* #rx"[0-9]+" line))]
              [operation (regexp-match #rx"[a-z]+ [a-z]*" line)])
          (process-square positions (match operation
                                      ['("turn on") turn-on]
                                      ['("turn off") turn-off]
                                      ['("toggle ")  toggle]))
          (switch-lights in)))))

(call-with-input-file "day6input"
  (λ (in-port)
    (switch-lights in-port)))

(vector-argmin identity lights)
