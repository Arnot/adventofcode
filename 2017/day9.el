(defstruct state
  (sum 0) (garbage-sum 0) ; Answers to the 2 problems
  (group-depth 0) ; How many groups deep are we?
  in-garbage ; Are we inside garbage?
  after-!) ; Is the current character after a ! (and therefore escaped)

(defun process-character (s character)
  (if (state-in-garbage s)
      (if (state-after-! s)
          (setf (state-after-! s) nil)
        ;; Process stuff inside garbage
        (case character
          (?! (setf (state-after-! s) t))
          (?> (setf (state-in-garbage s) nil))
          (t (incf (state-garbage-sum s)))))
    ;; Process stuff outside of garbage
    (case character
      (?! (setf (state-after-! s) t))
      (?< (setf (state-in-garbage s) t))
      (?> (setf (state-in-garbage s) nil))
      (?{ (unless (state-in-garbage s)
            (incf (state-group-depth s))))
      (?} (unless (state-in-garbage s)
            (incf (state-sum s)
                  (state-group-depth s))
            (decf (state-group-depth s))))))
  s)

(defun process-input ()
  (with-current-buffer "day9input"
    (goto-char (point-min))
    (let ((state (make-state)))
      (while (not (eobp))
        (process-character state
                           (char-after))
        (forward-char))
      state)))

(let ((result (process-input)))
  (print (format "Part 1: %s" (state-sum result)))
  (print (format "Part 2: %s" (state-garbage-sum result))))
