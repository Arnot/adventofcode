#lang racket

(require file/md5)

(define magic "iwrupvqb")

(define (mine number target)
  (define test-string
    (string-append magic (number->string number)))

  (if (string=? (substring
                 (bytes->string/utf-8
                  (md5 test-string))
                 0 (string-length target))
                target)
      (display number)
      (mine (add1 number) target)))

(display "Part 1: ")
(mine 10000 "00000")
(newline)
(display "Part 2: ")
(mine 10000 "000000")
