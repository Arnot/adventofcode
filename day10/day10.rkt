#lang racket

(define input-list (list 1 1 1 3 1 2 2 1 1 3))

(define (look-and-say iter maxiter input-list)
  (let ([counter 0]
        [current-number (car input-list)]
        [result-list (list)])
    (for ([i input-list])
      (if (eq? current-number i)
          (set! counter (add1 counter))
          (begin
            (set! result-list (append result-list
                                      (list counter current-number)))
            (set! counter 1)
            (set! current-number i))))
    ;; Add last number to result-list
    (set! result-list (append result-list
                              (list counter current-number)))
    (if (eq? iter maxiter)
        (length result-list)
        (look-and-say (add1 iter) maxiter result-list))))


(time (newline)
      (display (look-and-say 1 10 input-list))
      (newline))
(time (newline)
      (display (look-and-say 1 20 input-list))
      (newline))
(time (newline)
      (display (look-and-say 1 30 input-list))
      (newline))

;; SLOW
;; (time (newline)
;;       (display (look-and-say 1 40 input-list))
;;       (newline))
;; (time (newline)
;;       (display (look-and-say 1 50 input-list))
;;       (newline))
