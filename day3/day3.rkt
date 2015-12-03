#lang racket
(require scheme/mpair)

(define (visit-houses input task)
  ;; Sleigh driver positions
  (define human (mcons 0 0))
  (define robot (mcons 0 0))
  (define driver 'human)

  ;; (0 . 0) is always in the list
  (define houses (list (cons 0 0)))

  (define (up)
    (if (eq? driver 'human)
        (set-mcdr! human (+ (mcdr human) 1))
        (set-mcdr! robot (+ (mcdr robot) 1))))

  (define (down)
    (if (eq? driver 'human)
        (set-mcdr! human (- (mcdr human) 1))
        (set-mcdr! robot (- (mcdr robot) 1))))

  (define (left)
    (if (eq? driver 'human)
        (set-mcar! human (- (mcar human) 1))
        (set-mcar! robot (- (mcar robot) 1))))

  (define (right)
    (if (eq? driver 'human)
        (set-mcar! human (+ (mcar human) 1))
        (set-mcar! robot (+ (mcar robot) 1))))

  (define (visit-one-house dir)
    (cond [(char=? dir #\^) (up)]
          [(char=? dir #\v) (down)]
          [(char=? dir #\<) (left)]
          [(char=? dir #\>) (right)]
          [else 'error-bad-input])

    (let ((current-house ;; mcons->cons is ugly?
           (if (eq? driver 'human)
               (cons (mcar human) (mcdr human))
               (cons (mcar robot) (mcdr robot)))))
      (if (eq? (member current-house houses) #f)
          (set! houses (append houses (list current-house)))
          #f))

    ;; Switch driver if this is assignment 2
    (if (eq? task 2)
        (if (eq? driver 'human)
            (set! driver 'robot)
            (set! driver 'human))
        #f))

  (sequence-for-each visit-one-house input)

  (length houses)
  )

;; test cases
;; (visit-houses "^>v<" 2)
;; (visit-houses ">" 2)
;; (visit-houses "^v" 2)
;; (visit-houses "<><><>" 2) ;; 2

(display "Task 1: ")
(call-with-input-file "day3input"
  (λ (in)
    (visit-houses (in-input-port-chars in) 1)))

(display "Task 2: ")
(call-with-input-file "day3input"
  (λ (in)
    (visit-houses (in-input-port-chars in) 2)))
