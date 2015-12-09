#lang racket

(define all-distances (make-hash))

(struct destination (city distance))

(define (already-added? from to)
  (let ([distances (hash-ref all-distances from #f)])
    (if (eq? distances #f)
        #f
        (member (hash-ref all-distances from) to))))

(define (add-distance from to)
  (let ([distances (hash-ref all-distances from #f)])
    (if (eq? distances #f)
        (hash-set! all-distances from (list to))
        (hash-set! all-distances from (append distances (list to)))))
  (let* ([rev-from (destination-city to)] ;; also add reverse distance
         [rev-to (destination from (destination-distance to))]
         [reverse-distances (hash-ref all-distances rev-from #f)])
    (if (eq? reverse-distances #f)
        (hash-set! all-distances rev-from (list rev-to))
        (hash-set! all-distances rev-from (append reverse-distances (list rev-to))))))

(define (process-input in-port)
  (let ([line (read-line in-port)])
    (if (eof-object? line)
        'end-of-file
        (begin (let* ([line-as-vec (list->vector (string-split line))]
                      [from (vector-ref line-as-vec 0)]
                      [dest (destination (vector-ref line-as-vec 2)
                                         (vector-ref line-as-vec 4))])
                 (add-distance from dest))
               (process-input in-port)))))

(call-with-input-file "day9input"
  (λ (in-port)
    (process-input in-port)))

(for ([(k v) all-distances])
  (let ([cities (map destination-city v)])
    (let ([distances (map destination-distance v)])
      (printf "~a:\t ~a\n\t\t ~a\n" k cities distances))))

