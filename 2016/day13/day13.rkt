#lang racket

(define magic-number 10)

(define cells (make-hash))

(define (cell-number x y)
  (+ (* x x)
     (* 3 x)
     (* 2 x y)
     y
     (* y y)
     magic-number))

(define (open-space? x y)
  (let ([str (number->string (cell-number x y) 2)])
    (even? (count (lambda (c) (eq? c #\1))
                 (string->list str)))))

(define (check-neighbors x y)
  (define neighbors (list))

  (when (and (<= 0 (sub1 x))
             (open-space? (sub1 x) y))
    (set! neighbors (append neighbors (list (cons (sub1 x) y)))))

  (when (and (<= 0 (sub1 y))
             (open-space? x (sub1 y)))
    (set! neighbors (append neighbors (list (cons x (sub1 y))))))

  (when (and (<= 0 (sub1 x))
             (<= 0 (sub1 y))
             (open-space? (sub1 x) (sub1 y)))
    (set! neighbors (append neighbors (list (cons (sub1 x) (sub1 y))))))

  (when (and (<= 0 (sub1 y))
             (open-space? (add1 x) (sub1 y)))
    (set! neighbors (append neighbors (list (cons (add1 x) (sub1 y))))))

  (when (and (<= 0 (sub1 x))
             (open-space? (sub1 x) (add1 y)))
    (set! neighbors (append neighbors (list (cons (sub1 x) (add1 y))))))

  (when (open-space? x (add1 y))
    (set! neighbors (append neighbors (list (cons x (add1 y))))))

  (when (open-space? (add1 x) y)
    (set! neighbors (append neighbors (list (cons (add1 x) y)))))

  (when (open-space? (add1 x) (add1 y))
    (set! neighbors (append neighbors (list (cons (add1 x) (add1 y))))))

  neighbors)

;; (define (walk-to-cell x y)
;;   (define (loop lst)
;;     (let ([pos (car loop)])
;;       )))
