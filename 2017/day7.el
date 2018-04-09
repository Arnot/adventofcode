(defstruct node weight children)

(defun get-input-lines (buffer-name)
  (with-current-buffer buffer-name
    (split-string (buffer-string)
                  "\n")))

(defun get-named-node (line)
  "Return a list of (node-name, node)"
  ;; node-list contains (name, "(weight)", children...)
  (let ((node-list (split-string line "[->, ]" t)))
    (list (first node-list)
          (make-node :weight (string-to-number (first (split-string (second node-list) "[()]" t)))
                     :children (cddr node-list)))))

(defun add-to-map (node-map named-node)
  (let ((name (first named-node))
        (node (second named-node)))
    (setf (gethash name node-map) node)))

(defun build-node-map ()
  "Build a map of name -> node"
  (let ((node-map (make-hash-table :test 'equal)))
    (dolist (line (get-input-lines "day7input"))
      (add-to-map node-map
                  (get-named-node line)))
    node-map))

(defun get-nodes-with-parents (node-map)
  (let ((nodes-with-parents '()))
    (apply #'append
           (mapcar #'node-children
                   (hash-table-values node-map)))))

(defun get-node-without-parents (node-map)
  (let ((nodes-without-parents (copy-hash-table node-map)))
    (dolist (node-with-parent (get-nodes-with-parents node-map))
      (remhash node-with-parent nodes-without-parents))
    (first (hash-table-keys nodes-without-parents))))

(defun get-total-weight (node-map node-name)
  (let* ((node (gethash node-name node-map))
         (weight (node-weight node)))
    (dolist (child-name (node-children node))
      (let ((child-weight (get-total-weight node-map child-name)))
        (incf weight child-weight)))
    weight))

(defun children-unbalanced? (node-map node-name)
  (let* ((node (gethash node-name node-map))
         (children (node-children node)))
    (if (null children)
        nil
      (let ((sub-weights (mapcar (lambda (child) (get-total-weight node-map child)) children)))
        (if (apply #'= sub-weights)
            nil
          (list children sub-weights))))))

(defun heaviest-child (balance-stats)
  "wow hacky"
  (let* ((names (first balance-stats))
         (weights (second balance-stats))
         (heaviest nil)
         (max-weight (apply #'max weights))
         (zipped (mapcar* #'cons names weights)))
    (dolist (pair zipped)
      (when (= max-weight (cdr pair))
        (setf heaviest (car pair))))
    heaviest))

(defun find-heavy-node (node-map node-name)
    (princ node-name)
    (let ((balance-stats (children-unbalanced? node-map node-name)))
      (print balance-stats)
      (if balance-stats
          (find-heavy-node node-map (heaviest-child balance-stats))
        (node-weight (gethash node-name node-map)))))

;; Part 1
(let ((nodes (build-node-map)))
  (message (format "Part 1: %s" (get-node-without-parents nodes))))

;; Part 2
(find-heavy-node (build-node-map) "dgoocsw")
