#lang racket

(define input (file->lines "day7.input"))
;; (define input (list "abba[mnop]qrst"
                    ;; "abcd[bddb]xyyx"
                    ;; "aaaa[qwer]tyui"
                    ;; "ioxxoj[asdfgh]zxcvbn"))

(define (abba? str)
  (for/or ([i (in-range 0 (- (string-length str) 3))])
    (let ([a (string-ref str i)]
          [b (string-ref str (+ i 1))]
          [c (string-ref str (+ i 2))]
          [d (string-ref str (+ i 3))])
      (and (equal? a d)
           (equal? b c)
           (not (equal? a b))))))

(define (split-strings line)
  (define bracket-matcher (regexp "\\[.+?\\]"))
  (let ([outside-brackets (regexp-split bracket-matcher line)]
        [inside-brackets (regexp-match* bracket-matcher line)])
    (values outside-brackets inside-brackets)))

(define (TLS? str)
  (let-values ([(outside-brackets inside-brackets) (split-strings str)])
    (and (for/or ([s (in-list outside-brackets)])
           (abba? s))
         (for/and ([s (in-list inside-brackets)])
           (not (abba? s))))))

(define (bab? inside-brackets a b)
  (for/or ([inside (in-list inside-brackets)])
    (for/or ([i (in-range 0 (- (string-length inside) 2))])
      (let ([x (string-ref inside i)]
            [y (string-ref inside (+ i 1))]
            [z (string-ref inside (+ i 2))])
        (and (equal? x b)
             (equal? y a)
             (equal? z b))))))

(define (aba-bab? outside-brackets inside-brackets)
  (for/or ([outside (in-list outside-brackets)])
    (for/or ([i (in-range 0 (- (string-length outside) 2))])
      (let ([a (string-ref outside i)]
            [b (string-ref outside (+ i 1))]
            [c (string-ref outside (+ i 2))])
        (if (and (equal? a c)
                 (not (equal? a b)))
            (bab? inside-brackets a b)
            #f)))))

(define (SSL? str)
  (let-values ([(outside inside) (split-strings str)])
    (aba-bab? outside inside)))

(displayln "Part 1")
(displayln (count identity (map TLS? input)))

(displayln "Part 1")
(displayln (count identity (map SSL? input)))
