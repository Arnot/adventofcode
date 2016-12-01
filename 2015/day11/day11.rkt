#lang racket

(require rackunit)

(define rx-increasing-straight #rx"abc|bcd|cde|def|efg|fgh|ghi|hij|ijk|jkl|klm|lmn|mno|nop|opq|pqr|qrs|rst|stu|tuv|uvw|vwx|wxy|xyz")
(define rx-forbidden-letters #rx".*[iol].*")
(define rx-pair #px"(.)\\1")

(define forbidden-letters '(#\i #\o #\l))

(define (increment-char c)
  (if (char=? c #\z)
      #\a
      ((compose integer->char add1 char->integer) c)))

;; Skip forbidden letters, saves time
(define (increment-check-char c)
  (let ([next-c (increment-char c)])
    (if (member next-c forbidden-letters)
        (increment-char next-c)
        next-c)))

(define (increment-list lst)
  (if (char=? (car lst) #\z)
      (cons (increment-check-char (car lst))
            (increment-list (cdr lst)))
      (cons (increment-check-char (car lst))
            (cdr lst))))

(define (increment-string str)
  (let ([lst (reverse (string->list str))])
    ((compose list->string reverse)
     (increment-list lst))))

(define (password-ok? pass)
  (and (regexp-match? rx-increasing-straight pass)
       (not (regexp-match? rx-forbidden-letters pass))
       (= 2 (length (regexp-match* rx-pair pass)))))

(define (next-password pass)
  (let loop ([nextpass (increment-string pass)])
    (if (password-ok? nextpass)
        nextpass
        (loop (increment-string nextpass)))))

(check-equal? (increment-string "xx") "xy")
(check-equal? (increment-string "xz") "ya")
(check-equal? (password-ok? "hijklmmn") #f)
(check-equal? (password-ok? "abbceffg") #f)
(check-equal? (password-ok? "abbcegjk") #f)
;; Slow:
;; (check-equal? (next-password "abcdefgh") "abcdffaa")
;; (time  (check-equal? (next-password "ghijklmn") "ghjaabcc"))

(time (next-password "vzbxkghb"))
(time (next-password "vzbxxyzz"))
