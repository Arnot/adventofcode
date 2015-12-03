#lang racket

(define (visit-houses input)
  (define x 0)
  (define y 0)
  ;; (0 . 0) is always in the list
  (define houses (list (cons 0 0))) 

  (define (visit-one-house dir)
    (cond [(char=? dir #\^) (set! y (+ y 1))]
          [(char=? dir #\v) (set! y (- y 1))]
          [(char=? dir #\<) (set! x (- x 1))]
          [(char=? dir #\>) (set! x (+ x 1))]
          [else 'error-bad-input])

    (let ((current-house (cons x y)))
      (if (eq? (member current-house houses) #f)
          (set! houses (append houses (list current-house)))
          #f)))

  (sequence-for-each visit-one-house input)

  (length houses)
  )

;; test cases
;; (visit-houses "^>v<")
;; (visit-houses ">")
;; (visit-houses "^v^v^v^v^v")
;; (visit-houses ">>") ;; 3
;; (visit-houses "<><><>") ;; 2

(call-with-input-file "day3input"
  (λ (in)
    (visit-houses (in-input-port-chars in))))

