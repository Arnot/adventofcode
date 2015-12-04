#lang racket

(require file/md5)

(define (mine input target)
  (define (mine-helper iteration)
    (let ([test-string (string-append input (number->string iteration))])
      (let ([hashed-string (bytes->string/utf-8 (md5 test-string))])
        (if (string=? (substring hashed-string 0 (string-length target))
                      target)
            (display iteration)
            (mine-helper (add1 iteration))))))

  (mine-helper 0))

(display "Part 1: ")
(mine "iwrupvqb" "00000")
(newline)
(display "Part 2: ")
(mine "iwrupvqb" "000000")
