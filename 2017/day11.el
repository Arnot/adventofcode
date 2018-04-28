;; --+      +--+      +--
;;    \    /    \    /
;;     +--+ 0,-1 +--+  2,-1
;;    /    \    /    \
;; --+ -1,0 +--+  1,0 +--
;;    \    /    \    /
;;     +--+  0,0 +--+  2,0
;;    /    \    /    \
;; --+ -1,1 +--+  1,1 +--
;;    \    /    \    /
;;     +--+  0,1 +--+  2,1

(defun process-input (string)
  (split-string string "," t))

(defun get-direction-symbol (direction)
  (cond
   ((string= direction "n") 'n)
   ((string= direction "ne") 'ne)
   ((string= direction "se") 'se)
   ((string= direction "s") 's)
   ((string= direction "sw") 'sw)
   ((string= direction "nw") 'nw)))

(defun move (x y direction)
  (let ((dir (get-direction-symbol direction)))
    (if (evenp x)
        (case dir
          ('n  (list  x     (1- y)))
          ('s  (list  x     (1+ y)))
          ('ne (list (1+ x)  y))
          ('se (list (1+ x) (1+ y)))
          ('sw (list (1- x) (1+ y)))
          ('nw (list (1- x)  y)))
      (case dir
        ('n  (list  x     (1- y)))
        ('s  (list  x     (1+ y)))
        ('ne (list (1+ x) (1- y)))
        ('se (list (1+ x) y))
        ('sw (list (1- x) y))
        ('nw (list (1- x) (1- y))))
      )))

(defun do-steps (input)
  (let ((steps (mapcar #'get-direction-symbol (process-input input)))
        (posx 0)
        (posy 0)
        (maxposx 0)
        (maxposy 0))
    (dolist (step steps)
      (let ((newpos (move posx posy step)))
        (setf posx (first newpos)
              posy (second newpos)
              maxposx (max maxposx (abs posx))
              maxposy (max maxposy (abs posy)))))
    (print (format "Distance from (0,0): %s" (max (abs posx) (abs posy))))
    (print (format "Max distance from (0,0): %s" (max maxposx maxposy)))))

(setf input (concat "s,ne,ne,ne,se,se,se,s,s,s,s,s,s,s,sw,s,s,sw,sw,sw,sw,sw,sw,sw,n,s,ne,nw,nw,nw,"
                    "nw,nw,se,nw,nw,nw,nw,ne,n,n,nw,nw,n,nw,n,n,nw,n,ne,nw,nw,n,n,n,n,n,s,n,n,n,n,"
                    "n,n,n,ne,nw,n,nw,n,n,nw,n,n,n,n,se,n,nw,n,n,ne,s,nw,ne,ne,ne,n,ne,ne,ne,nw,sw,"
                    "n,se,ne,n,ne,ne,nw,ne,ne,sw,se,ne,ne,ne,nw,ne,sw,ne,se,ne,ne,nw,se,ne,ne,ne,ne,"
                    "ne,se,ne,ne,s,se,ne,s,ne,sw,ne,se,se,nw,se,ne,ne,se,ne,se,ne,se,se,se,se,se,se,"
                    "se,s,ne,n,s,se,se,se,se,ne,se,se,se,nw,s,s,s,s,se,se,se,se,s,nw,s,se,s,s,s,s,"
                    "s,sw,s,s,se,se,se,se,se,se,ne,ne,ne,s,ne,se,se,ne,s,ne,s,se,n,s,n,s,n,s,s,se,"
                    "s,s,sw,s,s,sw,s,s,s,s,s,s,nw,s,s,s,ne,ne,s,n,ne,s,sw,s,sw,s,sw,s,s,s,sw,s,s,s,"
                    "sw,s,s,sw,sw,s,s,sw,s,s,nw,sw,sw,sw,sw,ne,nw,s,s,nw,s,sw,s,sw,s,sw,sw,sw,sw,sw,"
                    "s,s,sw,sw,n,s,s,sw,sw,s,s,sw,s,sw,sw,sw,s,s,sw,sw,s,sw,sw,sw,n,sw,ne,nw,s,sw,"
                    "sw,s,sw,sw,nw,sw,sw,se,sw,ne,sw,sw,sw,sw,sw,sw,ne,n,s,sw,sw,sw,sw,nw,sw,sw,n,"
                    "sw,sw,sw,sw,sw,sw,nw,sw,sw,sw,sw,sw,sw,sw,ne,ne,sw,s,sw,nw,nw,sw,nw,sw,ne,sw,"
                    "sw,nw,sw,ne,nw,se,sw,sw,ne,sw,nw,sw,n,sw,nw,sw,nw,nw,sw,sw,sw,nw,ne,sw,nw,nw,"
                    "sw,sw,sw,sw,n,sw,nw,sw,nw,sw,sw,sw,sw,n,nw,nw,nw,nw,nw,sw,sw,nw,se,se,nw,sw,nw,"
                    "nw,nw,sw,nw,sw,ne,nw,nw,nw,nw,s,nw,sw,s,sw,se,sw,nw,sw,nw,sw,nw,nw,s,s,n,nw,sw,"
                    "sw,nw,nw,sw,sw,nw,ne,sw,nw,se,nw,se,s,nw,nw,nw,n,nw,nw,sw,nw,ne,ne,nw,nw,nw,sw,"
                    "nw,nw,se,se,nw,nw,nw,nw,sw,nw,nw,nw,nw,s,nw,nw,nw,nw,n,s,nw,nw,nw,n,nw,nw,nw,"
                    "n,n,ne,nw,n,nw,nw,nw,nw,n,nw,nw,nw,nw,nw,ne,n,sw,sw,nw,nw,sw,se,nw,n,nw,sw,nw,"
                    "s,nw,n,ne,nw,nw,nw,n,nw,s,n,nw,n,n,nw,n,se,n,nw,nw,n,nw,nw,ne,n,n,se,se,nw,nw,"
                    "nw,sw,n,nw,sw,sw,nw,nw,s,n,nw,n,s,n,n,n,nw,nw,n,nw,sw,sw,n,sw,n,n,n,n,n,nw,nw,"
                    "n,ne,n,ne,sw,n,n,n,nw,n,s,n,n,s,n,n,nw,nw,n,n,n,nw,nw,nw,n,nw,n,n,nw,se,n,nw,"
                    "n,n,sw,n,n,n,n,nw,n,nw,n,n,se,n,n,n,n,n,n,nw,s,nw,n,n,nw,n,n,n,sw,n,se,nw,n,nw,"
                    "n,n,n,n,n,n,n,sw,n,nw,n,n,n,n,n,n,se,n,se,n,n,n,n,n,n,nw,n,n,ne,n,n,n,n,sw,n,"
                    "se,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,ne,n,n,n,n,n,n,n,n,n,ne,n,s,ne,nw,s,n,nw,"
                    "n,n,n,sw,n,n,n,sw,se,ne,n,n,n,s,nw,n,se,n,n,s,sw,n,n,se,n,n,sw,n,n,n,ne,ne,ne,"
                    "ne,n,n,se,sw,n,n,n,se,n,se,sw,nw,ne,n,n,n,n,nw,n,n,n,n,n,n,ne,n,nw,n,s,n,nw,nw,"
                    "ne,ne,n,n,n,n,n,n,n,ne,n,n,n,n,n,ne,ne,n,ne,ne,n,n,n,ne,n,ne,n,n,n,n,ne,ne,n,"
                    "s,ne,ne,n,n,s,n,ne,ne,ne,n,ne,nw,n,ne,n,ne,n,ne,ne,se,ne,s,ne,ne,n,n,ne,s,n,ne,"
                    "s,n,ne,ne,ne,n,ne,s,se,se,ne,s,sw,ne,se,ne,ne,ne,n,ne,n,ne,n,nw,sw,ne,n,n,ne,"
                    "ne,ne,ne,ne,se,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,s,ne,ne,ne,ne,s,ne,ne,"
                    "sw,ne,ne,n,n,ne,ne,ne,ne,ne,n,ne,n,ne,ne,ne,ne,se,ne,ne,n,ne,ne,ne,n,sw,ne,ne,"
                    "n,ne,ne,ne,ne,n,ne,ne,ne,ne,ne,nw,ne,s,ne,ne,ne,ne,ne,ne,ne,sw,n,sw,ne,ne,ne,"
                    "ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,n,ne,ne,ne,ne,n,ne,ne,ne,ne,ne,ne,ne,ne,se,sw,"
                    "ne,ne,ne,ne,se,ne,ne,s,ne,s,ne,se,nw,s,n,ne,ne,ne,ne,sw,ne,ne,se,ne,ne,ne,ne,"
                    "ne,ne,se,ne,se,ne,ne,sw,nw,se,ne,ne,ne,ne,s,ne,n,ne,ne,nw,sw,ne,n,nw,ne,ne,se,"
                    "ne,ne,ne,ne,se,ne,ne,sw,ne,s,s,se,n,ne,ne,ne,n,s,nw,ne,se,se,s,ne,se,sw,ne,ne,"
                    "se,ne,ne,ne,ne,ne,ne,se,se,ne,ne,ne,se,ne,nw,ne,s,se,ne,sw,ne,ne,ne,ne,ne,ne,"
                    "ne,nw,se,ne,n,s,ne,ne,ne,ne,ne,ne,ne,ne,se,se,ne,nw,ne,ne,sw,sw,ne,ne,sw,ne,se,"
                    "ne,ne,ne,se,se,ne,ne,ne,ne,se,se,ne,ne,ne,ne,ne,se,ne,ne,s,se,ne,ne,se,ne,ne,"
                    "ne,se,s,ne,se,se,ne,ne,ne,ne,sw,se,se,s,se,se,ne,ne,se,s,se,se,ne,ne,se,s,ne,"
                    "se,ne,nw,se,ne,ne,se,se,se,sw,se,ne,se,se,se,ne,ne,se,se,ne,ne,ne,n,ne,ne,ne,"
                    "nw,se,s,se,s,n,ne,ne,nw,ne,se,ne,ne,s,ne,ne,se,nw,ne,se,se,se,se,se,se,se,ne,"
                    "se,se,ne,ne,se,se,se,se,n,ne,se,se,se,s,se,ne,ne,sw,se,ne,s,se,n,se,se,ne,ne,"
                    "se,se,se,n,sw,ne,ne,se,ne,se,se,se,se,se,ne,sw,sw,se,se,ne,ne,se,se,se,se,se,"
                    "nw,se,se,se,se,sw,se,s,se,sw,se,se,se,se,n,se,ne,s,se,ne,se,se,se,se,se,ne,ne,"
                    "se,ne,ne,n,se,se,sw,se,se,nw,se,ne,n,se,n,se,sw,se,sw,se,se,se,nw,se,se,se,se,"
                    "s,nw,sw,nw,se,se,se,ne,se,n,ne,se,se,se,se,se,se,se,se,se,se,nw,se,ne,se,se,se,"
                    "nw,se,se,se,se,se,se,s,se,se,se,s,se,sw,nw,se,se,sw,se,se,se,se,se,n,se,se,se,"
                    "se,ne,sw,se,se,sw,se,nw,n,ne,se,s,s,n,s,s,nw,se,ne,se,se,s,se,se,se,se,sw,se,"
                    "se,se,se,s,se,se,se,se,se,se,se,se,se,se,se,se,s,se,n,n,se,se,se,se,ne,se,se,"
                    "se,s,se,se,se,se,se,se,se,se,se,se,se,se,s,nw,se,se,s,se,s,se,s,se,se,se,s,se,"
                    "se,se,se,se,se,n,ne,se,n,s,se,se,sw,nw,nw,se,se,se,se,se,s,se,ne,se,s,se,se,n,"
                    "s,se,s,se,s,se,s,se,se,s,s,s,se,se,se,s,se,se,s,se,nw,ne,s,s,se,se,se,ne,se,se,"
                    "s,se,se,se,se,se,nw,se,se,se,ne,se,s,se,se,se,se,se,se,s,s,se,s,nw,s,n,se,s,se,"
                    "se,se,se,nw,s,se,s,s,se,se,se,ne,s,s,se,s,s,s,se,se,sw,s,se,se,sw,nw,s,s,se,se,"
                    "se,se,se,ne,s,s,se,s,s,se,s,sw,sw,s,se,s,se,se,s,se,s,n,s,s,nw,nw,s,s,se,s,s,"
                    "se,se,se,s,s,ne,se,se,n,ne,se,s,nw,s,se,s,s,n,se,se,ne,ne,se,s,s,se,se,se,se,"
                    "se,se,se,s,se,s,se,s,se,se,s,sw,s,se,sw,se,s,s,n,s,s,s,s,nw,n,s,s,s,se,s,s,s,"
                    "s,s,se,nw,s,se,s,s,nw,s,se,s,s,s,se,se,s,s,se,se,se,s,s,se,s,s,sw,se,se,se,s,"
                    "se,se,n,s,s,s,se,s,s,nw,s,ne,nw,se,s,s,se,nw,se,ne,se,se,se,s,s,se,se,nw,se,n,"
                    "se,s,sw,se,sw,s,se,s,s,se,s,se,s,s,se,s,s,n,s,se,se,s,s,s,s,s,s,sw,ne,s,s,s,s,"
                    "se,s,se,s,se,s,se,s,s,se,se,s,s,s,s,s,se,nw,se,se,s,se,ne,s,s,nw,nw,s,s,s,se,"
                    "ne,s,ne,s,s,s,sw,s,se,nw,s,nw,s,n,s,s,se,s,s,ne,se,s,s,s,se,s,s,se,s,nw,s,s,s,"
                    "se,ne,n,se,s,s,s,n,s,nw,s,ne,s,s,nw,s,s,n,s,s,se,s,se,se,s,s,s,s,s,s,s,se,ne,"
                    "s,ne,s,s,sw,s,s,s,s,n,s,s,s,s,sw,se,s,nw,ne,ne,s,s,se,s,s,s,s,s,ne,s,s,nw,sw,"
                    "s,s,s,n,s,ne,se,s,se,s,s,s,s,s,sw,se,nw,s,s,s,s,s,s,s,s,ne,n,s,ne,s,s,s,s,nw,"
                    "s,se,s,s,s,s,s,se,s,s,s,s,s,s,s,n,s,sw,s,s,s,s,s,se,s,s,sw,se,s,s,s,s,s,s,sw,"
                    "s,s,s,s,s,s,s,s,s,ne,nw,nw,s,n,s,s,s,s,s,s,s,s,sw,s,s,n,s,s,nw,s,s,s,nw,s,sw,"
                    "s,nw,s,s,s,s,sw,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,sw,s,sw,s,s,s,nw,"
                    "sw,s,s,nw,s,s,se,nw,s,s,s,se,s,se,nw,ne,s,s,s,s,s,s,nw,s,s,s,sw,s,sw,s,s,s,s,"
                    "s,s,sw,ne,sw,s,n,s,s,s,s,s,s,s,s,s,s,sw,s,s,s,s,s,s,s,sw,s,s,ne,s,s,s,s,s,sw,"
                    "sw,s,s,s,s,s,s,n,sw,n,ne,s,s,s,nw,nw,s,se,ne,s,se,s,s,s,sw,n,s,s,s,nw,sw,s,sw,"
                    "ne,s,s,s,s,sw,ne,s,s,s,s,s,sw,s,s,sw,s,s,s,s,s,s,s,sw,s,n,s,s,s,sw,s,s,n,sw,s,"
                    "n,ne,nw,sw,s,s,sw,s,s,sw,s,n,s,s,sw,s,s,sw,s,se,s,sw,s,s,s,sw,s,s,s,s,sw,s,n,"
                    "sw,s,s,s,sw,s,sw,s,sw,s,sw,s,s,sw,s,s,s,s,n,s,s,s,s,s,s,sw,s,s,sw,s,sw,s,s,s,"
                    "s,n,s,sw,s,s,n,nw,s,s,nw,sw,s,sw,sw,s,sw,sw,s,s,sw,sw,nw,sw,nw,s,sw,se,sw,s,s,"
                    "sw,sw,s,nw,s,s,se,s,s,s,sw,sw,sw,n,s,sw,s,sw,s,se,s,s,s,sw,sw,s,sw,nw,sw,s,s,"
                    "sw,sw,s,sw,s,s,s,sw,sw,sw,sw,sw,s,s,ne,ne,s,s,sw,se,se,sw,se,nw,sw,sw,s,sw,ne,"
                    "sw,sw,s,s,sw,nw,sw,s,sw,s,s,sw,s,sw,s,nw,se,s,s,s,sw,s,se,se,ne,sw,sw,sw,s,nw,"
                    "s,s,n,sw,sw,s,sw,sw,s,sw,sw,s,s,sw,s,s,ne,s,sw,s,sw,sw,sw,n,s,sw,s,s,sw,sw,nw,"
                    "n,sw,s,nw,s,s,sw,sw,sw,sw,sw,ne,sw,sw,sw,sw,s,s,sw,sw,sw,nw,s,s,s,sw,sw,se,sw,"
                    "sw,s,s,sw,sw,ne,se,sw,se,sw,s,nw,n,ne,s,sw,se,s,sw,s,sw,s,sw,s,sw,ne,s,sw,n,sw,"
                    "s,s,nw,sw,se,s,sw,sw,sw,sw,sw,sw,sw,ne,s,s,se,sw,sw,sw,sw,s,se,sw,sw,s,s,s,sw,"
                    "nw,n,sw,sw,s,ne,s,sw,se,sw,sw,se,sw,sw,sw,s,s,s,s,sw,s,s,sw,se,s,sw,s,nw,s,sw,"
                    "sw,s,sw,sw,sw,s,se,sw,sw,sw,sw,ne,sw,sw,sw,se,s,n,sw,se,s,sw,n,sw,sw,se,sw,s,"
                    "s,sw,sw,s,sw,ne,nw,sw,s,sw,sw,s,s,sw,sw,s,sw,sw,sw,sw,sw,sw,s,sw,sw,se,sw,sw,"
                    "s,sw,sw,se,sw,sw,s,ne,s,sw,ne,s,s,sw,sw,sw,s,sw,sw,sw,sw,sw,sw,sw,s,sw,ne,s,sw,"
                    "sw,sw,sw,sw,sw,sw,s,n,ne,s,sw,sw,sw,sw,sw,sw,sw,s,nw,sw,sw,n,sw,sw,sw,s,sw,sw,"
                    "sw,sw,sw,se,ne,sw,s,sw,ne,s,sw,sw,ne,sw,sw,s,n,sw,nw,sw,sw,ne,sw,sw,sw,sw,sw,"
                    "s,sw,n,sw,sw,s,sw,n,s,s,s,sw,sw,s,sw,se,sw,ne,n,sw,sw,n,sw,s,sw,sw,sw,sw,sw,sw,"
                    "s,sw,sw,s,s,sw,sw,sw,se,sw,nw,sw,sw,sw,sw,sw,se,sw,sw,sw,sw,sw,s,ne,sw,sw,sw,"
                    "se,ne,sw,sw,s,sw,sw,ne,nw,s,se,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,se,sw,sw,"
                    "ne,se,sw,sw,se,sw,ne,sw,se,sw,sw,sw,sw,sw,sw,sw,se,sw,sw,sw,sw,sw,sw,sw,sw,sw,"
                    "sw,sw,sw,se,sw,sw,sw,sw,sw,sw,sw,sw,sw,se,sw,sw,sw,ne,sw,sw,sw,nw,sw,n,sw,nw,"
                    "s,n,n,se,sw,se,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,s,nw,sw,sw,se,sw,sw,sw,"
                    "ne,sw,sw,sw,sw,sw,nw,nw,sw,sw,sw,sw,sw,sw,sw,n,sw,sw,sw,sw,sw,sw,nw,sw,ne,sw,"
                    "ne,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,n,sw,nw,n,sw,sw,sw,sw,nw,ne,sw,sw,sw,sw,sw,n,"
                    "sw,sw,sw,nw,nw,sw,sw,sw,ne,sw,sw,sw,nw,sw,sw,sw,ne,sw,ne,nw,sw,nw,s,sw,sw,sw,"
                    "ne,nw,sw,s,nw,sw,sw,sw,sw,sw,sw,sw,sw,sw,ne,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,ne,"
                    "n,sw,sw,sw,ne,sw,nw,se,nw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,s,sw,sw,se,sw,sw,sw,nw,"
                    "sw,sw,s,nw,ne,se,sw,sw,s,sw,sw,sw,se,sw,sw,nw,sw,sw,sw,sw,sw,sw,nw,sw,sw,n,sw,"
                    "sw,sw,sw,nw,sw,se,sw,sw,sw,s,ne,n,sw,sw,sw,nw,sw,ne,sw,sw,sw,sw,sw,sw,sw,nw,sw,"
                    "nw,sw,nw,s,sw,sw,sw,nw,nw,nw,sw,nw,sw,se,se,sw,sw,sw,sw,sw,sw,ne,nw,sw,sw,nw,"
                    "nw,sw,sw,ne,sw,se,sw,sw,sw,sw,sw,nw,nw,s,nw,nw,sw,sw,sw,nw,sw,n,ne,s,sw,sw,sw,"
                    "sw,sw,sw,sw,sw,sw,n,n,sw,sw,sw,sw,sw,n,nw,ne,nw,nw,ne,nw,nw,sw,sw,sw,sw,ne,nw,"
                    "sw,sw,s,nw,nw,sw,nw,sw,se,sw,sw,nw,ne,sw,nw,nw,sw,sw,sw,sw,s,s,sw,sw,sw,sw,n,"
                    "sw,nw,sw,sw,nw,sw,sw,ne,sw,sw,sw,ne,n,sw,sw,sw,n,nw,nw,sw,nw,nw,sw,sw,sw,sw,nw,"
                    "sw,sw,sw,nw,sw,sw,sw,nw,nw,nw,nw,ne,s,sw,sw,sw,sw,nw,sw,sw,sw,sw,nw,nw,sw,sw,"
                    "nw,nw,nw,sw,n,sw,nw,se,nw,s,sw,nw,sw,sw,sw,sw,nw,sw,n,nw,nw,nw,se,se,s,nw,sw,"
                    "ne,sw,sw,nw,ne,s,sw,sw,nw,nw,nw,nw,nw,nw,nw,nw,sw,sw,nw,sw,nw,nw,sw,sw,sw,sw,"
                    "nw,n,nw,sw,nw,sw,nw,ne,sw,se,nw,sw,n,sw,nw,sw,nw,sw,se,nw,nw,nw,sw,ne,nw,sw,s,"
                    "nw,s,s,nw,nw,ne,ne,nw,sw,nw,sw,nw,sw,n,se,sw,sw,nw,sw,sw,s,nw,ne,ne,nw,sw,nw,"
                    "nw,s,ne,nw,sw,sw,sw,sw,sw,sw,nw,ne,nw,nw,sw,nw,sw,se,sw,ne,sw,s,ne,sw,sw,nw,sw,"
                    "nw,ne,sw,nw,nw,sw,nw,nw,se,se,s,nw,sw,sw,nw,nw,nw,nw,nw,nw,sw,sw,sw,ne,s,sw,sw,"
                    "ne,nw,n,nw,se,nw,sw,se,nw,nw,sw,s,sw,s,sw,sw,sw,sw,sw,se,s,nw,sw,sw,sw,sw,nw,"
                    "nw,sw,nw,sw,n,sw,nw,n,se,sw,sw,sw,nw,se,sw,s,nw,nw,nw,sw,sw,nw,s,n,nw,sw,sw,nw,"
                    "sw,nw,nw,nw,se,nw,nw,s,nw,se,nw,nw,sw,sw,nw,sw,sw,nw,nw,nw,sw,nw,sw,s,sw,sw,sw,"
                    "nw,nw,nw,nw,sw,nw,nw,sw,s,nw,nw,nw,sw,sw,nw,sw,nw,se,ne,nw,nw,sw,nw,sw,nw,sw,"
                    "nw,nw,sw,sw,nw,sw,nw,sw,nw,sw,sw,nw,sw,nw,sw,sw,sw,nw,nw,nw,n,ne,nw,nw,sw,nw,"
                    "sw,nw,s,nw,nw,sw,sw,sw,sw,sw,nw,sw,n,nw,sw,nw,sw,sw,sw,nw,sw,nw,nw,sw,sw,sw,sw,"
                    "nw,nw,nw,nw,sw,sw,sw,nw,sw,sw,nw,n,sw,n,nw,sw,nw,nw,nw,s,nw,sw,nw,nw,nw,nw,nw,"
                    "sw,sw,nw,nw,n,n,sw,nw,nw,sw,sw,s,nw,nw,nw,nw,nw,sw,nw,sw,nw,sw,nw,sw,nw,nw,nw,"
                    "n,nw,sw,nw,nw,s,nw,nw,nw,sw,sw,nw,sw,nw,nw,ne,sw,se,nw,nw,sw,nw,nw,n,sw,sw,sw,"
                    "nw,nw,nw,nw,nw,nw,n,sw,ne,nw,nw,nw,sw,sw,sw,sw,ne,nw,nw,sw,nw,nw,sw,nw,nw,nw,"
                    "sw,nw,nw,sw,nw,nw,se,s,s,nw,nw,nw,nw,nw,sw,nw,nw,sw,sw,nw,nw,se,nw,nw,nw,nw,sw,"
                    "nw,nw,nw,sw,s,sw,se,nw,sw,nw,nw,nw,s,nw,nw,nw,sw,sw,nw,sw,sw,n,nw,nw,sw,nw,nw,"
                    "sw,nw,s,se,nw,nw,nw,nw,se,ne,nw,se,nw,ne,nw,nw,sw,nw,nw,nw,nw,nw,nw,sw,ne,nw,"
                    "nw,se,nw,sw,nw,nw,se,ne,nw,nw,nw,nw,s,sw,nw,sw,sw,nw,nw,ne,nw,nw,nw,nw,nw,se,"
                    "nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,s,ne,nw,sw,nw,sw,nw,nw,n,sw,nw,nw,nw,ne,sw,nw,"
                    "nw,nw,nw,sw,nw,nw,nw,se,nw,nw,s,nw,sw,se,sw,nw,nw,nw,s,sw,s,nw,n,se,sw,se,nw,"
                    "nw,nw,sw,sw,sw,nw,se,nw,nw,nw,nw,nw,nw,sw,ne,nw,nw,nw,nw,nw,sw,nw,sw,s,nw,nw,"
                    "nw,ne,nw,sw,se,se,nw,sw,sw,nw,n,s,s,nw,nw,nw,nw,nw,nw,nw,nw,nw,s,nw,nw,ne,n,se,"
                    "ne,nw,nw,nw,sw,sw,sw,nw,nw,nw,nw,se,nw,se,nw,nw,ne,n,nw,nw,nw,nw,nw,nw,nw,nw,"
                    "se,nw,ne,nw,nw,sw,nw,nw,nw,nw,nw,sw,se,nw,nw,nw,s,n,nw,nw,nw,s,s,nw,sw,s,nw,nw,"
                    "se,nw,nw,nw,n,nw,n,nw,nw,nw,nw,nw,nw,ne,sw,nw,nw,nw,s,nw,nw,nw,nw,se,nw,nw,nw,"
                    "nw,nw,nw,nw,ne,nw,nw,nw,nw,ne,sw,nw,nw,sw,nw,nw,nw,nw,n,nw,nw,se,nw,nw,sw,s,nw,"
                    "se,nw,n,nw,nw,ne,nw,ne,nw,sw,s,nw,nw,nw,nw,nw,ne,nw,s,nw,nw,nw,nw,nw,nw,nw,nw,"
                    "nw,nw,nw,nw,sw,n,ne,se,nw,nw,nw,nw,nw,nw,nw,nw,nw,ne,nw,n,nw,nw,nw,n,nw,nw,s,"
                    "ne,nw,nw,nw,n,nw,nw,ne,nw,nw,nw,nw,nw,nw,nw,s,nw,se,nw,nw,nw,nw,se,se,nw,nw,nw,"
                    "nw,nw,nw,s,nw,nw,nw,nw,ne,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,sw,sw,nw,se,nw,"
                    "nw,nw,sw,nw,nw,n,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,sw,s,nw,n,nw,sw,s,s,n,nw,n,"
                    "nw,nw,nw,nw,nw,nw,se,nw,nw,nw,nw,nw,nw,nw,nw,nw,n,ne,sw,nw,s,nw,ne,nw,nw,nw,nw,"
                    "nw,sw,nw,nw,nw,ne,nw,nw,sw,sw,s,nw,nw,ne,nw,s,nw,nw,n,nw,nw,n,n,nw,nw,nw,nw,nw,"
                    "nw,se,nw,n,nw,se,nw,s,nw,nw,sw,n,nw,nw,sw,nw,nw,n,nw,ne,nw,se,nw,n,se,nw,nw,nw,"
                    "nw,nw,nw,n,nw,nw,nw,nw,n,sw,nw,nw,nw,nw,ne,nw,nw,nw,n,n,nw,nw,nw,nw,nw,sw,sw,"
                    "n,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,se,nw,nw,nw,nw,nw,ne,nw,sw,n,s,sw,s,nw,se,nw,"
                    "nw,s,sw,sw,se,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,s,ne,se,nw,nw,s,nw,nw,n,nw,nw,nw,"
                    "nw,nw,n,n,nw,n,nw,nw,nw,se,nw,nw,nw,nw,n,nw,sw,nw,n,nw,nw,nw,se,nw,s,nw,s,nw,"
                    "nw,n,nw,n,n,nw,se,n,nw,sw,nw,nw,nw,nw,nw,nw,nw,nw,nw,s,n,nw,nw,ne,nw,n,nw,n,n,"
                    "nw,nw,n,nw,ne,ne,se,se,nw,n,nw,nw,nw,n,nw,se,n,s,nw,nw,se,n,nw,nw,sw,s,nw,nw,"
                    "ne,sw,nw,n,nw,nw,n,nw,ne,ne,sw,n,ne,s,nw,nw,se,nw,nw,nw,nw,n,nw,sw,n,ne,nw,se,"
                    "nw,nw,nw,se,sw,n,nw,nw,nw,nw,nw,s,n,nw,s,n,nw,nw,nw,ne,se,nw,nw,nw,nw,s,n,nw,"
                    "nw,n,nw,nw,nw,n,nw,nw,nw,nw,nw,s,nw,nw,s,nw,nw,nw,nw,nw,sw,nw,nw,ne,nw,ne,nw,"
                    "nw,sw,n,nw,nw,ne,nw,s,nw,n,se,sw,n,sw,s,nw,se,nw,nw,n,ne,nw,nw,nw,nw,nw,nw,se,"
                    "nw,ne,n,nw,nw,sw,ne,nw,n,n,n,nw,nw,nw,n,nw,n,nw,nw,n,s,nw,nw,nw,nw,ne,nw,s,ne,"
                    "ne,n,nw,se,nw,n,s,nw,se,n,nw,nw,nw,nw,nw,n,n,nw,nw,nw,sw,n,n,n,n,nw,nw,nw,nw,"
                    "nw,nw,n,nw,nw,nw,nw,sw,nw,n,nw,n,nw,nw,nw,n,nw,s,nw,se,n,n,nw,n,nw,n,nw,n,n,n,"
                    "nw,sw,nw,nw,s,n,nw,nw,ne,nw,se,n,nw,nw,n,n,nw,nw,nw,n,se,n,nw,nw,nw,sw,n,nw,nw,"
                    "nw,n,nw,nw,n,nw,s,n,nw,n,nw,n,nw,n,nw,nw,n,nw,n,nw,n,nw,nw,n,s,sw,nw,nw,nw,nw,"
                    "nw,n,ne,n,nw,nw,nw,n,s,nw,nw,nw,se,nw,nw,n,se,nw,ne,nw,n,n,nw,nw,se,nw,nw,se,"
                    "n,n,nw,n,n,nw,sw,ne,nw,nw,nw,sw,nw,ne,n,se,n,ne,nw,sw,nw,n,nw,nw,nw,n,ne,s,nw,"
                    "nw,sw,nw,se,nw,n,sw,sw,nw,n,n,n,nw,nw,sw,nw,n,nw,ne,n,nw,n,nw,n,nw,se,nw,nw,n,"
                    "s,se,nw,n,nw,nw,nw,n,n,s,n,nw,nw,nw,sw,s,n,ne,se,nw,ne,n,nw,nw,sw,nw,n,nw,n,nw,"
                    "nw,s,n,nw,nw,n,n,n,nw,nw,n,ne,n,nw,n,nw,n,ne,s,nw,n,n,nw,nw,n,nw,nw,sw,n,n,nw,"
                    "nw,n,nw,sw,nw,n,nw,nw,nw,n,n,nw,n,sw,n,n,n,nw,s,se,n,ne,n,nw,n,n,nw,n,n,nw,nw,"
                    "nw,n,nw,se,nw,sw,n,nw,nw,n,n,nw,n,n,s,nw,nw,n,sw,nw,nw,n,nw,nw,n,ne,s,sw,nw,nw,"
                    "n,n,n,nw,sw,n,ne,n,nw,nw,ne,sw,n,n,sw,nw,n,n,n,nw,sw,n,nw,nw,n,se,nw,nw,nw,n,"
                    "nw,nw,n,n,n,se,nw,nw,n,n,sw,n,nw,n,nw,n,n,nw,nw,n,nw,n,s,n,n,n,nw,nw,se,n,nw,"
                    "n,n,s,nw,n,n,nw,nw,n,s,n,n,nw,n,nw,n,n,n,nw,s,sw,n,sw,nw,ne,nw,nw,nw,se,n,nw,"
                    "nw,nw,nw,n,nw,nw,n,n,n,nw,nw,nw,n,n,nw,n,n,s,nw,ne,n,se,nw,nw,nw,n,n,nw,n,nw,"
                    "n,n,nw,n,nw,nw,nw,n,n,s,n,s,n,n,n,n,nw,se,n,nw,n,n,se,sw,n,n,n,nw,nw,nw,nw,nw,"
                    "s,s,s,nw,ne,nw,n,ne,n,n,n,nw,n,se,n,n,n,n,sw,nw,n,n,n,n,nw,n,s,se,n,n,n,nw,n,"
                    "n,n,nw,n,se,n,nw,s,se,nw,n,nw,nw,n,nw,n,n,ne,n,n,n,n,n,se,n,n,s,nw,n,nw,sw,s,"
                    "n,se,ne,n,sw,s,s,nw,ne,nw,sw,n,n,n,n,n,n,s,n,n,nw,nw,n,nw,n,n,sw,ne,nw,n,n,n,"
                    "se,se,n,n,n,s,n,n,ne,n,nw,nw,n,n,n,n,se,nw,n,n,n,n,n,s,n,nw,s,n,n,n,se,n,n,n,"
                    "n,nw,n,n,se,n,sw,n,n,n,n,n,n,s,nw,n,n,nw,nw,n,nw,nw,n,n,nw,n,n,nw,ne,n,n,n,nw,"
                    "nw,n,n,n,n,n,n,n,ne,sw,nw,n,n,n,n,nw,n,n,nw,n,ne,n,n,n,s,se,nw,n,se,n,nw,n,n,"
                    "se,n,n,nw,n,s,se,n,sw,n,ne,n,nw,n,n,ne,ne,n,n,n,n,n,n,nw,nw,n,n,se,n,nw,n,n,n,"
                    "ne,nw,ne,n,nw,n,nw,nw,n,n,n,nw,n,se,n,nw,se,n,nw,n,n,n,n,n,n,n,nw,se,n,n,n,nw,"
                    "n,se,nw,n,n,n,nw,n,sw,n,n,n,nw,nw,ne,n,n,nw,n,n,n,n,s,nw,n,sw,se,n,s,n,n,n,nw,"
                    "ne,nw,n,s,n,n,n,n,n,n,n,n,n,n,nw,n,nw,nw,n,n,n,n,s,n,n,nw,n,nw,n,n,n,sw,n,n,nw,"
                    "n,nw,n,n,se,s,n,se,sw,n,n,sw,n,n,n,nw,nw,n,nw,nw,nw,n,n,n,n,n,s,n,se,n,n,s,n,"
                    "n,nw,n,n,n,n,n,ne,n,n,n,n,se,n,n,n,nw,n,n,nw,n,n,s,n,sw,n,n,n,n,n,n,ne,n,se,nw,"
                    "nw,n,n,n,ne,n,n,n,n,nw,se,n,n,n,n,n,ne,n,n,n,sw,n,n,se,n,n,nw,n,n,n,ne,n,sw,nw,"
                    "nw,n,n,n,n,n,n,n,n,sw,n,s,n,n,nw,nw,s,n,n,n,n,n,n,n,nw,n,n,n,n,n,n,n,n,ne,n,ne,"
                    "n,n,n,n,n,n,se,s,s,n,n,n,ne,n,se,n,n,ne,ne,sw,sw,n,n,n,n,n,ne,s,n,nw,sw,n,s,n,"
                    "sw,sw,n,ne,ne,n,sw,n,n,sw,ne,n,n,n,n,n,ne,sw,n,n,sw,ne,n,s,ne,nw,n,n,n,n,n,n,"
                    "n,n,n,n,n,nw,n,n,s,n,n,ne,sw,n,n,nw,sw,s,s,se,s,s,ne,se,se,se,ne,ne,se,ne,ne,"
                    "ne,ne,se,ne,ne,ne,ne,sw,ne,n,n,ne,nw,nw,n,sw,nw,n,n,n,n,ne,nw,n,sw,n,nw,ne,nw,"
                    "nw,nw,sw,n,nw,n,n,n,nw,nw,nw,nw,nw,nw,nw,nw,nw,s,ne,sw,nw,nw,nw,nw,nw,nw,nw,nw,"
                    "se,nw,ne,nw,sw,ne,nw,nw,sw,ne,nw,sw,nw,nw,ne,sw,se,nw,nw,nw,sw,se,sw,sw,sw,sw,"
                    "nw,sw,nw,sw,s,se,sw,sw,sw,sw,sw,sw,sw,sw,sw,s,sw,s,s,n,s,sw,s,s,se,sw,n,s,sw,"
                    "s,s,s,ne,sw,sw,sw,s,s,se,s,s,s,s,s,s,s,s,s,s,s,ne,se,s,s,nw,s,ne,sw,s,s,s,se,"
                    "s,sw,s,s,s,s,ne,s,sw,s,s,ne,s,se,s,s,s,se,ne,se,se,se,se,se,s,ne,ne,sw,s,se,n,"
                    "ne,s,se,s,se,s,se,n,s,se,s,nw,se,se,s,se,sw,se,ne,se,nw,se,se,se,se,nw,se,se,"
                    "se,se,se,se,se,se,n,nw,se,n,se,ne,n,sw,se,sw,se,se,se,se,ne,se,se,n,se,sw,se,"
                    "ne,se,ne,nw,ne,se,se,ne,se,sw,se,se,sw,se,n,se,n,ne,n,ne,ne,ne,se,se,ne,se,se,"
                    "nw,se,se,ne,se,n,s,se,ne,ne,n,ne,ne,ne,ne,nw,s,ne,sw,n,ne,ne,ne,nw,se,ne,ne,ne,"
                    "ne,ne,ne,ne,se,nw,ne,ne,ne,ne,se,ne,ne,se,ne,ne,se,se,se,sw,ne,ne,ne,ne,ne,ne,"
                    "ne,ne,ne,ne,ne,ne,ne,s,ne,n,ne,ne,ne,s,n,ne,se,n,n,s,se,n,ne,ne,nw,ne,ne,ne,ne,"
                    "ne,ne,nw,ne,ne,ne,n,n,ne,sw,ne,n,se,sw,sw,n,n,ne,n,se,n,ne,ne,ne,nw,ne,s,n,se,"
                    "sw,ne,se,n,ne,se,ne,ne,n,n,n,n,n,n,ne,n,n,n,n,ne,ne,n,ne,s,n,sw,ne,s,ne,s,ne,"
                    "ne,n,n,n,nw,n,n,ne,ne,ne,n,n,n,n,ne,ne,ne,n,n,n,ne,s,s,n,ne,n,se,n,n,ne,n,n,ne,"
                    "ne,n,ne,nw,sw,n,n,n,n,n,n,nw,n,n,ne,se,se,nw,se,n,n,n,n,n,nw,n,n,n,n,n,se,nw,"
                    "ne,n,s,n,n,n,n,nw,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,sw,n,n,n,nw,n,n,n,nw,se,nw,"
                    "s,se,n,n,n,nw,s,n,nw,n,n,n,nw,se,n,nw,n,n,n,n,nw,nw,sw,n,n,nw,n,nw,se,n,n,nw,"
                    "n,n,se,n,n,n,nw,n,nw,n,nw,nw,nw,s,n,n,n,nw,nw,nw,n,n,n,n,n,nw,nw,nw,n,nw,n,nw,"
                    "nw,nw,ne,nw,nw,n,nw,nw,n,n,n,n,n,n,n,n,nw,n,s,ne,nw,n,nw,nw,nw,s,n,nw,sw,sw,nw,"
                    "nw,n,nw,nw,nw,nw,se,nw,nw,n,nw,n,sw,nw,nw,ne,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,"
                    "nw,nw,nw,se,nw,s,nw,n,sw,nw,n,ne,ne,nw,nw,nw,nw,nw,nw,nw,nw,ne,nw,nw,nw,nw,nw,"
                    "nw,ne,nw,sw,nw,nw,nw,nw,nw,sw,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,sw,nw,nw,nw,nw,s,"
                    "nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,sw,ne,sw,sw,sw,nw,nw,se,nw,s,s,nw,nw,se,s,se,s,"
                    "sw,nw,nw,nw,nw,nw,se,sw,se,nw,nw,nw,nw,nw,nw,nw,nw,nw,ne,nw,sw,nw,nw,nw,sw,sw,"
                    "n,n,n,nw,nw,sw,sw,nw,nw,nw,se,sw,sw,sw,sw,nw,sw,nw,se,nw,sw,nw,s,sw,nw,sw,nw,"
                    "nw,nw,n,sw,se,s,nw,sw,sw,nw,sw,nw,nw,nw,n,se,sw,nw,nw,sw,ne,nw,sw,sw,sw,sw,nw,"
                    "nw,ne,ne,nw,s,nw,sw,nw,sw,nw,nw,sw,n,nw,sw,nw,nw,nw,nw,sw,n,n,nw,nw,nw,nw,sw,"
                    "nw,nw,nw,sw,sw,sw,nw,se,sw,sw,sw,sw,nw,nw,sw,nw,sw,se,s,sw,ne,nw,ne,sw,ne,sw,"
                    "nw,sw,sw,sw,sw,nw,sw,sw,nw,sw,sw,n,nw,sw,sw,sw,se,sw,sw,sw,n,n,sw,sw,sw,sw,sw,"
                    "sw,nw,sw,nw,sw,s,sw,ne,sw,s,nw,sw,sw,n,sw,sw,nw,sw,n,nw,nw,sw,nw,sw,s,sw,ne,sw,"
                    "n,s,sw,ne,sw,sw,sw,sw,sw,sw,se,sw,sw,sw,se,n,sw,sw,nw,s,sw,sw,sw,sw,sw,sw,sw,"
                    "sw,sw,sw,nw,sw,sw,sw,nw,sw,sw,s,sw,sw,nw,sw,ne,sw,n,sw,sw,sw,sw,n,s,s,sw,se,sw,"
                    "sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,s,sw,se,sw,s,sw,ne,"
                    "nw,n,s,ne,sw,se,nw,sw,sw,sw,sw,sw,s,sw,sw,sw,sw,sw,s,sw,sw,sw,sw,sw,sw,sw,sw,"
                    "sw,sw,sw,s,ne,sw,sw,sw,sw,sw,n,s,sw,sw,sw,ne,sw,sw,sw,se,sw,nw,sw,sw,sw,se,se,"
                    "sw,s,sw,sw,sw,n,sw,sw,sw,sw,sw,sw,sw,sw,se,n,s,s,s,s,nw,sw,se,ne,sw,sw,n,nw,sw,"
                    "s,s,s,nw,sw,s,sw,sw,sw,sw,sw,ne,s,s,sw,sw,s,s,sw,s,s,n,s,sw,s,nw,s,sw,sw,sw,n,"
                    "s,sw,sw,sw,sw,sw,sw,s,sw,n,s,sw,sw,sw,s,sw,sw,sw,s,sw,s,sw,s,s,sw,sw,s,nw,sw,"
                    "sw,sw,sw,s,s,sw,se,s,n,sw,s,sw,sw,sw,s,sw,s,sw,s,n,sw,s,s,sw,sw,sw,sw,sw,s,sw,"
                    "nw,sw,sw,s,s,sw,sw,sw,sw,ne,sw,sw,s,sw,n,sw,sw,sw,s,nw,nw,sw,s,sw,s,s,s,s,sw,"
                    "ne,s,sw,s,s,sw,sw,sw,nw,s,s,s,sw,nw,n,nw,sw,sw,s,s,sw,sw,se,s,se,sw,nw,s,s,sw,"
                    "s,s,s,s,sw,s,ne,sw,s,nw,sw,s,s,ne,sw,s,sw,sw,sw,s,sw,s,sw,s,se,s,nw,sw,s,s,sw,"
                    "s,sw,s,se,s,n,s,ne,s,sw,s,s,sw,nw,s,s,s,sw,s,s,s,s,sw,s,sw,sw,s,n,s,s,s,s,s,sw,"
                    "ne,nw,se,s,s,s,s,s,s,sw,n,s,nw,n,s,s,s,n,s,s,nw,s,s,s,nw,s,s,nw,s,se,n,n,s,s,"
                    "n,s,s,sw,sw,s,nw,s,s,s,nw,s,s,s,n,s,s,s,nw,s,s,s,se,s,s,s,ne,se,s,s,s,s,s,sw,"
                    "n,s,ne,s,s,se,nw,s,s,s,s,s,n,s,s,sw,nw,nw,s,s,n,s,nw,n,s,s,s,ne,sw,s,s,s,s,s,"
                    "sw,s,s,s,s,s,s,s,s,s,sw,nw,ne,s,s,s,s,s,s,nw,s,s,s,se,s,n,ne,nw,se,ne,s,s,s,n,"
                    "s,nw,s,s,s,sw,sw,ne,s,se,se,s,s,s,sw,s,s,se,s,s,s,ne,s,s,se,s,s,s,se,n,s,s,s,"
                    "s,s,ne,s,s,s,s,s,s,s,s,n,s,s,s,s,s,s,s,se,s,s,s,se,s,s,s,s,se,se,s,sw,nw,s,nw,"
                    "s,s,se,s,n,s,se,n,sw,s,sw,sw,se,s,se,s,s,s,s,s,s,s,s,s,s,se,s,se,sw,s,s,se,s,"
                    "s,s,s,se,s,s,nw,s,s,n,n,s,s,se,se,se,s,se,nw,s,s,se,s,se,se,s,s,s,nw,nw,se,n,"
                    "se,n,se,se,s,s,n,s,sw,s,s,s,s,s,se,n,s,se,ne,s,s,s,sw,se,n,s,s,s,s,s,s,s,n,se,"
                    "s,s,s,s,s,s,s,s,s,se,s,sw,s,se,ne,sw,nw,s,s,nw,s,s,s,se,s,s,sw,se,s,s,s,ne,se,"
                    "s,s,s,s,s,s,sw,se,nw,se,se,s,sw,s,ne,se,s,se,se,se,s,se,se,se,n,se,sw,s,s,s,se,"
                    "s,s,se,s,s,s,s,s,se,se,se,s,se,se,se,s,se,se,s,s,s,se,s,s,s,s,se,se,s,s,s,se,"
                    "se,s,s,n,se,s,s,nw,se,se,se,se,nw,se,s,se,s,s,se,se,n,s,sw,nw,s,s,se,se,se,s,"
                    "se,n,s,se,s,se,n,se,se,se,se,s,se,nw,s,se,s,n,s,se,se,s,se,se,ne,s,ne,se,se,se,"
                    "s,nw,se,nw,s,se,s,s,se,se,nw,s,se,s,se,se,sw,se,s,s,se,se,se,ne,se,s,se,s,se,"
                    "se,se,se,se,se,se,s,se,s,se,se,s,se,s,nw,ne,se,nw,se,se,s,se,se,s,ne,se,se,se,"
                    "s,se,se,se,se,s,se,se,se,se,s,se,se,se,se,se,se,se,se,s,se,se,se,nw,s,se,s,n,"
                    "s,se,se,se,se,s,s,se,se,nw,se,n,ne,se,nw,se,s,s,se,se,n,sw,ne,s,se,se,sw,s,s,"
                    "se,n,se,se,s,ne,s,se,se,se,se,se,sw,se,se,se,se,se,ne,s,ne,se,s,sw,se,se,se,se,"
                    "se,se,s,se,s,s,se,sw,se,se,se,se,nw,se,se,nw,se,ne,s,se,sw,se,se,se,s,se,sw,se,"
                    "se,se,se,sw,se,se,sw,se,se,se,se,se,nw,ne,sw,ne,se,s,se,sw,se,se,se,se,se,se,"
                    "ne,se,sw,se,se,se,se,se,sw,se,se,se,se,se,se,se,se,se,ne,se,se,n,se,n,s,se,se,"
                    "se,se,se,se,se,nw,se,se,se,sw,se,se,se,se,ne,se,se,se,se,se,se,s,se,se,ne,s,ne,"
                    "se,s,se,se,nw,se,se,sw,se,se,se,se,se,se,se,se,se,se,nw,ne,se,se,ne,se,se,se,"
                    "se,se,se,n,se,se,se,se,ne,sw,se,se,ne,se,ne,se,nw,se,se,se,n,se,se,sw,se,s,se,"
                    "se,se,se,se,se,se,se,ne,se,se,se,s,se,se,se,ne,se,nw,n,se,se,nw,ne,se,se,se,se,"
                    "se,se,ne,ne,nw,se,se,se,ne,se,se,ne,n,se,se,se,se,ne,se,ne,ne,se,n,nw,se,se,s,"
                    "ne,se,se,se,ne,se,se,se,se,se,s,ne,se,se,nw,se,se,se,sw,se,n,se,se,se,s,ne,se,"
                    "se,se,se,se,n,n,se,se,se,se,ne,nw,se,se,ne,n,se,ne,se,se,ne,ne,se,ne,n,s,ne,se,"
                    "se,se,se,ne,s,s,se,se,se,se,se,ne,se,ne,ne,ne,se,sw,s,se,s,ne,se,ne,ne,se,ne,"
                    "se,se,se,se,ne,se,se,se,ne,sw,nw,se,se,s,se,nw,se,se,nw,s,se,se,se,se,se,se,nw,"
                    "se,se,se,nw,ne,ne,se,se,n,se,se,se,nw,se,s,nw,ne,ne,ne,se,ne,se,se,se,ne,se,se,"
                    "ne,ne,ne,nw,se,se,nw,ne,se,ne,n,se,ne,ne,ne,ne,ne,se,sw,ne,se,s,sw,se,nw,ne,se,"
                    "se,se,se,se,sw,se,se,s,ne,se,se,ne,se,se,ne,se,nw,ne,se,se,se,se,se,ne,ne,se,"
                    "se,se,ne,se,se,ne,ne,se,se,n,ne,ne,se,ne,se,se,ne,ne,n,ne,se,ne,se,ne,s,se,ne,"
                    "se,sw,ne,ne,s,ne,n,ne,se,se,ne,ne,ne,se,se,se,se,sw,se,s,ne,nw,ne,ne,ne,se,se,"
                    "se,ne,ne,nw,se,se,ne,se,nw,se,nw,se,se,nw,se,se,ne,ne,se,se,se,se,ne,ne,ne,ne,"
                    "sw,se,ne,ne,se,ne,se,ne,se,se,ne,ne,ne,ne,ne,se,se,se,se,se,sw,ne,se,n,se,n,nw,"
                    "se,se,sw,se,ne,se,ne,ne,ne,ne,nw,ne,se,se,ne,ne,se,ne,ne,se,ne,ne,ne,ne,se,ne,"
                    "sw,se,se,n,ne,se,ne,ne,sw,ne,ne,ne,se,se,se,se,se,ne,ne,se,n,se,n,sw,se,se,se,"
                    "se,ne,se,se,se,se,ne,se,se,se,ne,n,n,ne,se,se,ne,ne,ne,nw,ne,se,ne,se,ne,n,se,"
                    "ne,nw,ne,se,ne,s,ne,ne,ne,se,ne,sw,ne,se,se,se,ne,ne,ne,se,s,se,ne,n,n,ne,se,"
                    "se,ne,ne,ne,se,ne,n,ne,s,ne,ne,s,ne,se,ne,se,ne,ne,nw,se,ne,ne,se,nw,ne,ne,se,"
                    "sw,ne,se,ne,ne,s,se,ne,ne,ne,ne,se,se,ne,n,s,se,ne,s,ne,ne,ne,ne,ne,ne,ne,ne,"
                    "ne,ne,ne,ne,nw,n,ne,ne,n,se,ne,ne,ne,ne,ne,n,ne,se,ne,ne,se,ne,ne,nw,ne,se,ne,"
                    "se,s,se,ne,ne,ne,ne,ne,ne,ne,s,ne,ne,ne,ne,ne,ne,se,ne,ne,ne,nw,ne,se,ne,ne,ne,"
                    "ne,ne,nw,ne,nw,ne,ne,s,se,n,ne,ne,ne,se,ne,ne,ne,ne,ne,ne,nw,ne,s,sw,ne,ne,ne,"
                    "ne,ne,ne,ne,se,se,se,ne,n,ne,ne,ne,ne,ne,n,ne,ne,ne,ne,ne,ne,ne,s,ne,ne,ne,ne,"
                    "ne,ne,ne,ne,ne,ne,se,ne,se,ne,n,ne,se,ne,ne,ne,se,n,ne,ne,ne,ne,se,ne,ne,ne,ne,"
                    "se,sw,ne,ne,se,se,ne,ne,n,nw,ne,se,ne,ne,ne,ne,ne,ne,nw,ne,ne,ne,ne,ne,ne,ne,"
                    "ne,ne,ne,ne,se,ne,ne,ne,sw,nw,ne,nw,se,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,"
                    "ne,ne,ne,ne,ne,ne,ne,ne,ne,se"))
