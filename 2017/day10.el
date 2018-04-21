(setf day10-input "189,1,111,246,254,2,0,120,215,93,255,50,84,15,94,62")

(defun reverse-sublist (list beg end)
  (let ((rotated-list (-rotate (- beg)
                               list))
        (nend (- end beg)))
    (setf (subseq rotated-list 0 nend) (reverse (subseq rotated-list 0 nend)))
    (-rotate beg rotated-list)))

(defun make-hash-list (size)
  (let ((result '()))
    (dotimes (i size)
      (push i result))
    (nreverse result)))

(defun day10-p1 ()
  (let ((hash-list (make-hash-list 256))
        (current-pos 0)
        (skip-size 0)
        (input (mapcar #'string-to-number
                       (split-string day10-input ","))))
    (dolist (len input)
      (setf hash-list (reverse-sublist hash-list
                                       current-pos
                                       (+ len current-pos)))
      (setf current-pos (mod (+ current-pos len skip-size)
                             256))
      (incf skip-size))
    (* (first hash-list)
       (second hash-list))))

(message (prin1-to-string (day10-p1)))
;; 38415

(defun sparse-to-dense (hash)
  (let ((result '()))
    (dotimes (blk 16)
      (let ((xor 0))
        (dotimes (i 16)
          (let ((idx (+ (* 16 blk)
                        i)))
            (setf xor (logxor xor
                              (elt hash idx)))))
        (push xor result)))
    (setf result (nreverse result))
    (apply #'concat
           (mapcar (lambda (num)
                     (format "%x" num))
                   result))))

(defun day10-p2 ()
  (let ((hash-list (make-hash-list 256))
        (current-pos 0)
        (skip-size 0)
        (input (append (string-to-list day10-input)
                       '(17 31 73 47 23))))
    (dotimes (round 64)
      (dolist (len input)
        (setf hash-list (reverse-sublist hash-list
                                         current-pos
                                         (+ len current-pos)))
        (setf current-pos (mod (+ current-pos len skip-size)
                               256))
        (incf skip-size)))
    (sparse-to-dense hash-list)))

(message (prin1-to-string (day10-p2)))
