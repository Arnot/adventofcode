#lang racket
;; I probably should have used structs here

;; Reads all input into a list of ("OPERATION" inputs...)
;; Then iterates through this list until all inputs are calculated
;; If an operation is found that does not have two calculated inputs yet,
;; it is removed from the head of the list and appended again.
;; Once a port has been processed, it is removed.

;; Topological sort beforehand would be nicer and probably faster, but this
;; works pretty ok too.

;; Cool way to show all wires:
;(for ([(k v) all-wires])
;  (printf "~a: ~a\n" k v))


;; Hash table of all wires
(define all-wires (make-hash))

;; Parsed input ends up in here
(define inputs (list))

(define (get-wire x)
  (if (number? x)
      x
      (hash-ref all-wires x #f)))

(define (get-number c)
  (if (eq? (string->number c) #f)
      c
      (string->number c)))

(define (apply-op op connects)
  (cond [(string=? op "NOT")
         (hash-set! all-wires (cadr connects)
                    (bitwise-and #xFFFF
                                 (bitwise-not (get-wire (car connects)))))]
        [(string=? op "LSHIFT")
         (hash-set! all-wires (caddr connects)
                    (bitwise-and #xFFFF
                                 (arithmetic-shift (get-wire (car connects))
                                                   (get-wire (cadr connects)))))]
        [(string=? op "RSHIFT")
         (hash-set! all-wires (caddr connects)
                    (bitwise-and #xFFFF
                                 (arithmetic-shift (get-wire (car connects))
                                                   (- (get-wire (cadr connects))))))]
        [(string=? op "AND")
         (hash-set! all-wires (caddr connects)
                    (bitwise-and #xFFFF
                                 (get-wire (car connects))
                                 (get-wire (cadr connects))))]
        [(string=? op "OR")
         (hash-set! all-wires (caddr connects)
                    (bitwise-and #xFFFF
                                 (bitwise-ior (get-wire (car connects))
                                              (get-wire (cadr connects)))))]
        [(string=? op "ASSIGN")
         (hash-set! all-wires (cadr connects) (get-wire (car connects)))]))

;; Check if all input values of a gate are known
(define (inputs-known? gate)
  (or (and (string=? "ASSIGN" (car gate))
           (not (eq? (get-wire (cadr gate)) #f)))
      (and (string=? "NOT" (car gate))
           (not (eq? (get-wire (cadr gate)) #f)))
      (and (not (eq? (get-wire (cadr gate)) #f))
           (not (eq? (get-wire (caddr gate)) #f)))))

(define (process-ops input-list)
  (if (eq? 1 (length input-list))
      (let ([operation (caar input-list)]
            [connects  (cdar input-list)])
        (apply-op operation connects))
      (if (inputs-known? (car input-list))
          (let ([operation (caar input-list)]
                [connects  (cdar input-list)])
            (apply-op operation connects)
            (process-ops (cdr input-list)))
          ;; move head to tail and process the rest first
          ;; if inputs are not yet known
          (process-ops (append (cdr input-list) (list  (car input-list)))))))

(define (process-input-file in-port)
  (let ([line (read-line in-port)])
    (if (eof-object? line)
        (process-ops inputs)
        (let ([operation   (regexp-match #rx"[A-Z]+" line)]
              [connects    (regexp-match* #rx"([a-z][a-z]?|[0-9]+)" line)])
          (if (eq? operation #f)
              (set! inputs (append inputs
                                   (list (cons "ASSIGN" (map get-number connects)))))
              (set! inputs (append inputs
                                   (list (cons (car operation) (map get-number connects))))))
          (process-input-file in-port)))))

(call-with-input-file "day7input"
  (λ (in-port)
    (process-input-file in-port)))

;; Changed the input file for part 2
;; 1674 -> b  in original
;; 46065 -> b in part 2
(for ([(k v) all-wires])
  (printf "~a: ~a\n" k v))
(hash-ref all-wires "a")
(hash-ref all-wires "b")
