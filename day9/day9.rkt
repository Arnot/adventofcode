#lang racket

(define all-distances (make-hash))
(define all-cities (list))
(define all-routes (list))

(struct destination (city distance))
(struct route (path distance))

(define (traveling-santa current-city visited total-distance)
  (let* ([visited+ (append visited (list current-city))]
         [next-cities (remove* visited+ all-cities)])
    (if (empty? next-cities)
        (set! all-routes (append all-routes (list (route visited+ total-distance))))
        (for ([city-name next-cities])
          (let* ([city-dest (car (filter
                                  (λ (c) (string=? city-name (destination-city c)))
                                  (hash-ref all-distances current-city)))]
                 [dist (+ total-distance (destination-distance city-dest))])
            (traveling-santa city-name visited+ dist))))))

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
                                         (string->number (vector-ref line-as-vec 4)))])
                 (add-distance from dest))
               (process-input in-port)))))

(call-with-input-file "day9input"
  (λ (in-port)
    (process-input in-port)))

(for ([(k v) all-distances])
  (let ([cities (map destination-city v)])
    (let ([distances (map destination-distance v)])
      (set! all-cities (append all-cities (list k))))))


(for ([city all-cities])
  (traveling-santa city empty 0))

(length all-routes)
(define min-route (apply min (map route-distance all-routes)))
(define (output-route rt)
  (display (route-path rt))
  (display ", ")
  (display (route-distance rt))
  (newline))

(map output-route (filter (λ (distance)
                            (eq? min-route (route-distance distance))) all-routes))

