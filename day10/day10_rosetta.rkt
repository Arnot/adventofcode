#lang racket
;; Found on rosetta code
;; http://rosettacode.org/wiki/Look-and-say_sequence#Racket

(define (encode str)
  (regexp-replace* #px"(.)\\1*" str
                   (lambda (m c)
                     (~a (string-length m) c))))

(define (mark str iter max-iter)
  (if (> iter max-iter)
      (length (string->list str))
      (mark (encode str) (add1 iter) max-iter)))

(time (mark "1113122113" 1 40))
(time (mark "1113122113" 1 50))
