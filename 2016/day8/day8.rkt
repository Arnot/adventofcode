#lang racket

(define width 50)
(define height 6)

(define input (map string-split (file->lines "day8.input")))
;; (define input (list (list "rect" "4x2")))

(define disp (make-vector (* width height) #f))

(define (pixel x y)
  (vector-ref disp (+ x (* y width))))

(define (pixel-set! x y [val #t])
  (vector-set! disp (+ x (* y width)) val))

(define (draw-display)
  (for ([y (in-range 0 height)])
    (for ([x (in-range 0 width)])
      (display (if (pixel x y) "#" ".")))
    (displayln "")))

(define (rectangle str)
  (let ([dimensions (string-split str "x")])
    (for* ([x (in-range (string->number (first dimensions)))]
           [y (in-range (string->number (second dimensions)))])
      (pixel-set! x y))))

(define (horizontal-slice y)
  (map (λ (x) (pixel x y)) (stream->list (in-range width))))

(define (vertical-slice x)
  (map (λ (y) (pixel x y)) (stream->list (in-range height))))

(define (rotate-list lst n)
  (let [(m (modulo (- n) (length lst)))]
    (append (drop lst m) (take lst m))))

(define (rotate-row lst)
  (let* ([row (string->number (car (string-split (car lst) "y=")))]
         [slice (horizontal-slice row)]
         [amount (string->number (last lst))]
         [rotated-slice (rotate-list slice amount)])
    (for ([x (in-range width)])
      (pixel-set! x row (list-ref rotated-slice x)))))

(define (rotate-col lst)
  (let* ([col (string->number (car (string-split (car lst) "x=")))]
         [slice (vertical-slice col)]
         [amount (string->number (last lst))]
         [rotated-slice (rotate-list slice amount)])
    (for ([y (in-range height)])
      (pixel-set! col y (list-ref rotated-slice y)))))

(define (rotate lst)
  (match (car lst)
    ["row" (rotate-row (cdr lst))]
    ["column" (rotate-col (cdr lst))]
    [_ (displayln "Rotate: Input error")]))

(for ([ln (in-list input)])
  (match (list-ref ln 0)
    ["rect" (rectangle (list-ref ln 1))]
    ["rotate" (rotate (cdr ln))]
    [_ (displayln "Unknown input")])
  (draw-display)
  (displayln "\n")
  (sleep 0.05))

(count identity (vector->list disp))
