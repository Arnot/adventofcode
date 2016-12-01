#lang racket

(define input (port->string (open-input-file "day1.input")))
(define tokens (string-split input ", "))

(define (rotate dir rotation)
  (let ([x (car dir)]
        [y (cdr dir)])
    (match rotation
      [#\L (cons y (- x))]
      [#\R (cons (- y) x)])))

(define (move pos dir distance)
  (cons (+ (car pos) (* (car dir) distance))
        (+ (cdr pos) (* (cdr dir) distance))))

(define (single-step position direction step)
  (move position direction (string->number (substring step 1))))

(define (follow-route route)
  (define (process-route lst position direction)
    (if (empty? lst)
        position
        (let ([step (car lst)])
          (define newdir (rotate direction (string-ref step 0)))
          (define newpos (single-step position newdir step))
          (process-route (cdr lst) newpos newdir))))

  (process-route route (cons 0 0) (cons 0 1)))

(writeln "Part 1:")
(define part1-result (follow-route tokens))
(writeln part1-result)
(writeln (+ (abs (car part1-result))
            (abs (cdr part1-result))))
