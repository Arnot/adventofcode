;; -*- lexical-binding: t -*-
(defun process-line (line)
  (let* ((pipe (split-string line "[ <>-]" t))
         (source (string-to-number (car pipe)))
         (target (mapcar #'string-to-number
                         (cdr pipe))))
    (list source target)))

(defun process-all-lines ()
  (with-current-buffer "day12input"
    (let ((lines (make-vector (count-lines (point-min) (point-max)) nil)))
      (beginning-of-buffer)
      (while (not (eobp))
        (let ((pipe (process-line (buffer-substring (line-beginning-position)
                                                    (line-end-position)))))
          (setf (elt lines (first pipe)) (second pipe)))
        (forward-line))
      lines)))

(defun friends-of (elt graph)
  (let (visited
        (queue (list elt)))
    (while queue
      (let* ((current (pop queue))
             (children (elt graph current)))
        (push current visited)
        (dolist (child children)
          (unless (or (member child visited)
                      (member child queue))
            (push child queue)))))
    visited))

(defun generate-range (size)
  (let (range)
    (dotimes (i size)
      (push i range))
    (reverse range)))

(defun find-all-groups ()
  (let* ((pipes (process-all-lines))
         (candidates (generate-range (length pipes)))
         groups)
    (dolist (candidate candidates)
      (let ((connections (friends-of candidate pipes)))
        (push connections groups)
        (dolist (connection connections)
          (delete connection candidates))))
    groups))
