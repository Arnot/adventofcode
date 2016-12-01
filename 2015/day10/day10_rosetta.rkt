#lang racket
;; Found on rosetta code
;; http://rosettacode.org/wiki/Look-and-say_sequence#Racket

(define (encode str)
  (regexp-replace* #px"(.)\\1*" str
                   (lambda (m c)
                     (~a (string-length m) c))))

(define (look-and-say str iter max-iter)
  (if (> iter max-iter)
      (length (string->list str))
      (look-and-say (encode str) (add1 iter) max-iter)))

(time (look-and-say "1113122113" 1 40))
(time (look-and-say "1113122113" 1 50))
