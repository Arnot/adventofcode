#lang racket

(require file/md5
         racket/format)

(define found #f)
(define magic "iwrupvqb")

(define (mine number)
  (define test-string
    (format "~a~a" magic number))

  (if (string=? (substring
                 (bytes->string/utf-8
                  (md5 test-string))
                 0 5)
                "00000")
      (format "Found number: ~a" number)
      (mine (+ 1 number))))

(mine 0)
