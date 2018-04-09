(defstruct node weight children)

(defun get-input-lines (buffer-name)
  (with-current-buffer buffer-name
    (split-string (buffer-string) "\n")))

(defun get-named-node (line)
  "Return a list of (node-name, node)"
  (let ((node-list (split-string line "[->, ]" t)))
    (list (first node-list)
          (make-node :weight (string-to-number (second node-list))
                     :children (cddr node-list)))))

(defun add-to-map (node-map named-node)
  (let ((name (first named-node))
        (node (second named-node)))
    (setf (gethash name node-map) node)))

(defun build-node-map ()
  "Build a map of name -> node"
  (let ((node-map (make-hash-table :test 'equal)))
    (dolist (l (get-input-lines "day7input"))
      (add-to-map node-map (get-named-node l)))
    node-map))

(defun get-nodes-with-parents (node-map)
  (let ((nodes-with-parents '()))
    (apply #'append (mapcar #'node-children (hash-table-values node-map)))))

(defun get-node-without-parents (node-map)
  (let ((nodes-without-parents (copy-hash-table node-map)))
    (dolist (node-with-parent (get-nodes-with-parents node-map))
      (remhash node-with-parent nodes-without-parents))
    (assert (= 1 (hash-table-count nodes-without-parents)))
    (first (hash-table-keys nodes-without-parents))))


;; Part 1
(let ((nodes (build-node-map)))
  (message (format "Part 1: %s" (get-node-without-parents nodes))))
