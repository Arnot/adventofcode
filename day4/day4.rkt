#lang racket

(require file/md5
         racket/format)

(define found #f)
(define magic "iwrupvqb")

(define (mine number)
  (define test-string
    (string-append magic (number->string number)))

  (if (string=? (substring
                 (bytes->string/utf-8
                  (md5 test-string))
                 0 6)
                "000000")
      (format "Found number: ~a" number)
      (mine (+ 1 number))))

(time  (mine 0))
