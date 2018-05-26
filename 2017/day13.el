;; -*- lexical-binding: t -*-

(defstruct scanner
  (depth 0)
  (range 0))

(defun score (scanner &optional delay)
  (unless delay (setf delay 0))
  (if (= 0 (mod (+ (scanner-depth scanner) delay)
                (* 2 (1- (scanner-range scanner)))))
      (* (scanner-depth scanner)
         (scanner-range scanner))
    0))

(defun hits (scanner &optional delay)
  (unless delay (setf delay 0))
  (if (= 0 (mod (+ (scanner-depth scanner) delay)
                (* 2 (1- (scanner-range scanner)))))
      1
    0))

(defun build-scanner (s)
  (let* ((tokens (split-string s ": " t))
         (numbers (mapcar #'string-to-number tokens)))
    (make-scanner :depth (first numbers)
                  :range (second numbers))))

(defun get-scanners (input)
  (let ((lines (split-string input "\n" t)))
    (mapcar #'build-scanner lines)))

(defun get-score (scanners &optional delay)
  (apply #'+ (mapcar (lambda (s) (score s delay)) scanners)))

(defun get-hits (scanners &optional delay)
  (apply #'+ (mapcar (lambda (s) (hits s delay)) scanners)))

(defun part1 ()
  (get-score (get-scanners input)))

(defun part2 ()
  (let ((scanners (get-scanners input))
        (delay 0)
        result
        found)
    (while (not found)
      (setf result (get-hits scanners delay))
      (if (= 0 result)
          (setf found t)
        (incf delay)))
    delay))

(setf input (with-current-buffer "day13input" (buffer-string)))
