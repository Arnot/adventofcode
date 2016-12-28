#lang racket

(define magic-number 1362)
(define starting-point (cons 1 1))

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
          (loop (append (cdr lst) neighbors)))))
  (loop (list starting-point)))

(define (find-locations n)
  (define (loop lst)
    (let* ([pos (car lst)]
           [x (car pos)]
           [y (cdr pos)]
           [cell-dist (hash-ref cells pos #f)]
           [neighbors (check-neighbors x y)])
      (map (λ (p) (hash-set! cells p (add1 cell-dist))) neighbors)
      (if (>= cell-dist n)
          (length (hash->list cells))
          (loop (append (cdr lst) neighbors)))))

  (loop (list starting-point)))


(hash-set! cells starting-point 0)
(walk-to-cell 31 39)

(set! cells (make-hash))
(hash-set! cells starting-point 0)

(find-locations 50)
