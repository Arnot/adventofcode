(setf day10-input '(189 1 111 246 254 2 0 120 215 93 255 50 84 15 94 62))

(defun reverse-sublist (list beg end)
  (let ((rotated-list (-rotate (- beg) list))
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
        (skip-size 0))
    (dolist (len day10-input)
      (setf hash-list (reverse-sublist hash-list current-pos (+ len current-pos)))
      (setf current-pos (mod (+ current-pos len skip-size) 256))
      (incf skip-size))
    (* (first hash-list) (second hash-list))))

(day10-p1)
;; 38415
