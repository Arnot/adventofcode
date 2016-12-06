#lang racket

(require file/md5)

(define key "ojvtpuvg")

(define (generate-hash start)
  (let loop ([i start])
    (let ([hashed-key (bytes->string/utf-8 (md5 (~a key i)))])
      (if (equal? (substring hashed-key 0 5)
                  "00000")
          (cons hashed-key i)
          (loop (add1 i))))))

(define start-val 1000000)
(displayln "Part 1:")
(for ([i (in-range 0 8)])
  (let ([h (generate-hash start-val)])
    (set! start-val (add1 (cdr h)))
    (display (substring (car h) 5 6))))

(displayln "")
(displayln "Part 2:")
(set! start-val 1000000)
(define password (list #f #f #f #f #f #f #f #f))
(for ([i (in-range 0 8)])
  (let loop ([h (generate-hash start-val)])
    (let ([pos (string->number (substring (car h) 5 6) 16)])
      (set! start-val (add1 (cdr h)))
      (if (and (< pos 8)
               (eq? (list-ref password pos) #f))
          (set! password (list-set password pos (substring (car h) 6 7)))
          (loop (generate-hash start-val)))))
  (displayln password))
