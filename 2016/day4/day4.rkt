#lang racket

(define input (file->lines "day4.input"))
;; (define input (list "aaaaa-bbb-z-y-x-123[abxyz]"
;;                     "a-b-c-d-e-f-g-h-987[abcde]"
;;                     "not-a-real-room-404[oarel]"
;;                     "totally-real-room-200[decoy]"))

(define (count-letters str)
  (define counts (make-hash))
  (for ([c (in-string str)])
    (hash-set! counts c (add1 (hash-ref counts c 0))))
  counts)

(define (get-input-string lst)
  (string-join (take lst (sub1 (length lst))) ""))

(define (get-room-id lst)
  (string->number
   (car (regexp-match #rx"^[0-9]+"
                      (car (drop lst (sub1 (length lst))))))))

(define (get-checksum lst)
  (string->list
   (car (regexp-match #rx"[a-z]+"
                      (car (drop lst (sub1 (length lst))))))))

(define (compare-hash-entries a b)
  (if (> (cdr a) (cdr b))
      #t
      (if (and (= (cdr a) (cdr b))
               (< (char->integer (car a)) (char->integer (car b))))
          #t
          #f)))

;; Returns #f is string is incorrect, room ID if string is correct
(define (check-string lst)
  (let* ([input-string (get-input-string lst)]
         [id (get-room-id lst)]
         [checksum (get-checksum lst)]
         [lettercount (count-letters input-string)])
    (if (equal? checksum
                (map car (take (sort (hash->list lettercount) compare-hash-entries) 5)))
        id
        #f)))

(define results (for/list ([ln (in-list input)])
                  (check-string (regexp-split #rx"-" ln))))

(displayln "Part 1:")
(foldl
 (λ (next sum) (if next (+ sum next) sum)) 0
 results)

;; Part 2:
;; Caesar cipher is from Rosetta code
(define A (char->integer #\A))
(define Z (char->integer #\Z))
(define a (char->integer #\a))
(define z (char->integer #\z))

(define (rotate c n)
  (define cnum (char->integer c))
  (define (shift base) (integer->char (+ base (modulo (+ n (- cnum base)) 26))))
  (cond [(<= A cnum Z) (shift A)]
        [(<= a cnum z) (shift a)]
        [else c]))

(define (caesar s n)
  (list->string (for/list ([c (in-string s)]) (rotate c n))))

(define part2results (for/list ([ln (in-list input)])
                       (let* ([str (regexp-split #rx"-" ln)]
                              [id (check-string str)])
                         (if id
                             (cons (caesar (get-input-string str) id) id)
                             #f))))

(displayln "Part 2:")
(for ([r (in-list part2results)])
  (when r
    (displayln r)))
