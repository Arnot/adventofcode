#lang racket

(define magic-number 1362)

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
    (and (even? (count (lambda (c) (eq? c #\1))
                       (string->list str)))
         (not (hash-ref cells (cons x y) #f))))) ;; Cell shouldn't be visited before

(define (check-neighbors x y)
  (define neighbors (list))

  (when (and (<= 0 (sub1 x))
             (open-space? (sub1 x) y))
    (set! neighbors (append neighbors (list (cons (sub1 x) y)))))

  (when (and (<= 0 (sub1 y))
             (open-space? x (sub1 y)))
    (set! neighbors (append neighbors (list (cons x (sub1 y))))))

  (when (open-space? x (add1 y))
    (set! neighbors (append neighbors (list (cons x (add1 y))))))

  (when (open-space? (add1 x) y)
    (set! neighbors (append neighbors (list (cons (add1 x) y)))))

  ;; No diagonals!
  ;; (when (and (<= 0 (sub1 x))
  ;;            (<= 0 (sub1 y))
  ;;            (open-space? (sub1 x) (sub1 y)))
  ;;   (set! neighbors (append neighbors (list (cons (sub1 x) (sub1 y))))))

  ;; (when (and (<= 0 (sub1 y))
  ;;            (open-space? (add1 x) (sub1 y)))
  ;;   (set! neighbors (append neighbors (list (cons (add1 x) (sub1 y))))))

  ;; (when (and (<= 0 (sub1 x))
  ;;            (open-space? (sub1 x) (add1 y)))
  ;;   (set! neighbors (append neighbors (list (cons (sub1 x) (add1 y))))))

  ;; (when (open-space? (add1 x) (add1 y))
  ;;   (set! neighbors (append neighbors (list (cons (add1 x) (add1 y))))))

  neighbors)

(define (walk-to-cell destx desty)
  (define (loop lst)
    (let* ([pos (car lst)]
           [x (car pos)]
           [y (cdr pos)]
           [cell-dist (hash-ref cells pos #f)]
           [neighbors (check-neighbors x y)]
           [target-destination (hash-ref cells (cons destx desty) #f)])
      (map (λ (p) (hash-set! cells p (add1 cell-dist))) neighbors)
      (if target-destination
          target-destination
          (loop (append (cdr lst) neighbors)))
      ))
  (loop (list (cons 1 1))))

(hash-set! cells (cons 1 1) 0)

(walk-to-cell 31 39)
