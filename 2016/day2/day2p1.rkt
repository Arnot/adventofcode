#lang racket

(define input (open-input-file "day2.input"))

(define keypad '((1 2 3)
                 (4 5 6)
                 (7 8 9)))

(define (keypad-move dir pos)
  (cons (min 2 (max 0 (+ (car dir) (car pos))))
        (min 2 (max 0 (+ (cdr dir) (cdr pos))))))

(define (letter->move dir pos)
  (match dir
    [#\L (keypad-move '(-1 . 0) pos)]
    [#\R (keypad-move '(1 . 0)  pos)]
    [#\U (keypad-move '(0 . -1) pos)]
    [#\D (keypad-move '(0 . 1)  pos)]))

(define (output-number pos)
  (display (list-ref (list-ref keypad (cdr pos)) (car pos))))

(define (enter-sequence input)
  (define (loop input pos)
    (if (empty? input)
        (output-number pos)
        (loop (cdr input) (letter->move (car input) pos))))

  (loop input '(1 . 1)))

(displayln "Part 1:")
(for ([ln (in-lines input)])
  (enter-sequence (string->list ln)))
