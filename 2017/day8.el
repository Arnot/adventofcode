(setq registers (make-hash-table :test 'equal))
(setq highest-value-throughout-program 0)

(defun get-lines-from-input (buffername)
  (with-current-buffer buffername
    (split-string (buffer-substring (point-min) (point-max)) "\n" t)))

(defun process-single-line (line)
  (multiple-value-bind (destination operation amount _ source comparator threshold)
      (split-string line " ")
    (when (compare source comparator threshold)
      (perform operation destination amount)))
  (setf highest-value-throughout-program (max highest-value-throughout-program
                                              (apply #'max (hash-table-values registers)))))

(defun perform (operation destination amount)
  (let ((amnt (string-to-number amount)))
    (if (string= operation "inc")
        (incf (gethash destination registers 0) amnt)
      (assert (string= operation "dec"))
      (decf (gethash destination registers 0) amnt))))

(defun compare (source comparator threshold)
  (let* ((src (gethash source registers 0))
         (thr (string-to-number threshold)))
    (funcall (get-comparison comparator) src thr)))

(defun != (&rest args)
  (not (apply #'= args)))

(defun get-comparison (comparator)
  (cond
   ((string= comparator "<") #'<)
   ((string= comparator "<=") #'<=)
   ((string= comparator "==") #'=)
   ((string= comparator ">=") #'>=)
   ((string= comparator ">") #'>)
   ((string= comparator "!=") #'!=)
   (t (assert nil nil (format "invalid comparator: %s" comparator)))))

(defun day8-process-input ()
  (dolist (line (get-lines-from-input "day8input"))
    (process-single-line line)))

(day8-process-input)
(message (format "Part 1: %s" (apply #'max (hash-table-values registers))))
(message (format "Part 2: %s" highest-value-throughout-program))
