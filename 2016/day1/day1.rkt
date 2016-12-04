#lang racket

(define visited (list))

(define input (port->string (open-input-file "day1.input")))
(define tokens (string-split input ", "))
;; (define tokens (list "R8" "R4" "R4" "R8"))

(define (rotate dir rotation)
  (let ([x (car dir)]
        [y (cdr dir)])
    (match rotation
      [#\L (cons (- y) x)]
      [#\R (cons y (- x))])))

(define (move pos dir distance)
  (cons (+ (car pos) (* (car dir) distance))
        (+ (cdr pos) (* (cdr dir) distance))))

(define (single-step pos dir step)
  (move pos dir (string->number (substring step 1))))

(define (follow-route route)
  (define (process-route lst pos dir)
    (if (empty? lst)
        pos
        (let ([step (car lst)])
          (define newdir (rotate dir (string-ref step 0)))
          (define newpos (single-step pos newdir step))
          (process-route (cdr lst) newpos newdir))))

  (process-route route (cons 0 0) (cons 0 1)))

(define double-location #f)
(define (small-steps pos dir endpos)
  ;; (writeln (~a "member? :" (member pos visited) " -- " pos))
  (if (or (and (eq? (car pos) (car endpos))
               (eq? (cdr pos) (cdr endpos)))
          (member pos visited))
      (begin
        (when (member pos visited)
          (set! double-location #t))
        pos)
      (begin
        (set! visited (append visited (list pos)))
        (small-steps (move pos dir 1) dir endpos))))

(define (follow-route-small-steps route)
  (define (process-route lst pos dir)
    ;; (writeln lst)
    ;; (writeln dir)
    ;; (writeln pos)
    (if (empty? lst)
        pos
        (let ([step (car lst)])
          (define newdir (rotate dir (string-ref step 0)))
          (define newpos (single-step pos newdir step))
          (define endpos (small-steps pos newdir newpos))
          (if double-location
              endpos
              (process-route (cdr lst) newpos newdir)))))

  (process-route route (cons 0 0) (cons 0 1)))

(writeln "Part 1:")
(define part1-result (follow-route tokens))
(writeln part1-result)
(writeln (+ (abs (car part1-result))
            (abs (cdr part1-result))))

(writeln "Part 2:")
(define part2-result (follow-route-small-steps tokens))
(writeln part2-result)
(writeln (+ (abs (car part2-result))
            (abs (cdr part2-result))))
;; (writeln visited)
