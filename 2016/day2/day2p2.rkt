#lang racket

(define input (open-input-file "day2.input"))

(define keypad '((#f  #f  #\1 #f  #f )
                 (#f  #\2 #\3 #\4 #f )
                 (#\5 #\6 #\7 #\8 #\9)
                 (#f  #\A #\B #\C #f )
                 (#f  #f  #\D #f  #f )))

(define (keypad-move dir pos)
  (let ([newpos (cons (min 4 (max 0 (+ (car dir) (car pos))))
                      (min 4 (max 0 (+ (cdr dir) (cdr pos)))))])
    (if (pos->number newpos)
        newpos
        pos)))

(define (letter->move dir pos)
  (match dir
    [#\L (keypad-move '(-1 . 0) pos)]
    [#\R (keypad-move '(1 . 0)  pos)]
    [#\U (keypad-move '(0 . -1) pos)]
    [#\D (keypad-move '(0 . 1)  pos)]))

(define (pos->number pos)
  (list-ref (list-ref keypad (cdr pos)) (car pos)))

(define (enter-sequence input)
  (define (loop input pos)
    (if (empty? input)
        (display (pos->number pos))
        (loop (cdr input) (letter->move (car input) pos))))

  (loop input '(0 . 2)))

(displayln "Part 1:")
(for ([ln (in-lines input)])
  (enter-sequence (string->list ln)))
