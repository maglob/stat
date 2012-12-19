;;;; Statistics package 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :vecto)
    (ql:quickload "vecto")))

(defstruct axis                         
  (start)
  (end)
  (tick-delta))

(defvar +palette+ #((0 0 0) (0 0 1) (0 1 0) (0 1 1)
                    (1 0 0) (1 0 1) (1 1 0) (1 1 1)))

(defun create-axis (min max &optional (ntick 5))
  "Create axis with nice labels for range MIN..MAX with NTICK desired ticks.
See Graphics Gems: Nice Numbers for Graph Labels"
  (flet ((nicenum (x round)
           (let* ((exp (floor (log x 10)))
                  (f (/ x (expt 10 exp))))
             (* (if round
                    (cond
                      ((< f 1.5) 1)
                      ((< f 3) 2)
                      ((< f 7) 5)
                      (t 10))
                    (cond
                      ((<= f 1) 1)
                      ((<= f 2) 2)
                      ((<= f 5) 5)
                      (t 10)))
                (expt 10 exp)))))
    (let* ((range (nicenum (- max min) nil))
           (d (nicenum (/ range (1- ntick)) t)))
      (make-axis :start (* (floor (/ min d)) d) 
                 :end (* (ceiling (/ max d)) d)
                 :tick-delta d))))
  
(defun get-color (i)
  (elt +palette+ (mod i (length +palette+))))

(defun sum (seq)
  "Sum of SEQ"
  (reduce #'+ seq))

(defun product (seq)
  "Product of SEQ"
  (reduce #'* seq))

(defun mean (seq)
  "Arithmetic mean of SEQ"
  (/ (sum seq) (length seq)))
  
(defun median (seq)
  "Median of SEQ"
  (let* ((n (length seq))
	 (m (floor n 2))
	 (s (sort seq #'<)))
    (if (zerop (mod n 2))
	(/ (+ (elt s (1- m)) 
	      (elt s m))
	   2)
	(elt s m))))

(defun variance (seq)
  (let ((mean (mean seq)))
    (mean (map (type-of seq) (lambda (x) 
			       (expt (- mean x) 2))
	       seq))))

(defun covariance (a b)
  "Covariance between sequence A and B"
  (let ((mean-a (mean a))
        (mean-b (mean b))
        (sum 0))
    (map nil (lambda (x y)
               (incf sum (* (- x mean-a) (- y mean-b))))
         a b)
    (/ sum (1- (length a)))))

(defun stddev (seq)
  "Standard deviation of SEQ"
  (let ((mean (mean seq)))
    (sqrt (/ (sum (mapcar (lambda (x) 
                            (expt (- x mean) 2))
                          seq))
             (1- (length seq))))))

(defun ppmcc (a b)
  "Pearson Product-Mean Correlation Coefficient between sequences A and B"
  (/ (covariance a b)
     (* (stddev a) (stddev b))))

(defun range(n &optional (end nil end-p) (step 1))
  "List of numbers [0, END[ or [N, END[ with step size STEP"
  (if end-p
      (do ((i n (+ i step))
           (lst nil (cons i lst)))
	  ((>= i end) (nreverse lst)))
      (range 0 n)))
 
(defun hash-table-list (ht &aux res)
  "Convert hashtable HT into list of (key . value) pairs"
  (maphash (lambda (key value) 
	     (push (cons key value) res)) 
	   ht)
  res)

(defun frequence (seq &key (sort-value-fn nil) (sort-key-fn nil))
  "Frequence of elements in SEQ as a list of (element . count) pairs"
  (let ((ht (make-hash-table :test #'equal)))
    (map nil (lambda (x) 
               (incf (gethash x ht 0)))
         seq)
    (if sort-value-fn
        (sort (hash-table-list ht) (lambda (a b) (funcall sort-value-fn (cdr a) (cdr b))))
        (if sort-key-fn
            (sort (hash-table-list ht) (lambda (a b) (funcall sort-key-fn (car a) (car b))))
            (hash-table-list ht)))))

(defun head (seq &optional (n 10))
  "Sequence of first N elements in SEQ"
  (if (>= n (length seq))
      seq
      (subseq seq 0 n)))

(defun tail (seq &optional (n 10))
  "Sequence of last N elmenets in SEQ"
  (if (>= n (length seq))
      seq
      (subseq seq (- (length seq) n))))

(defun enumerate (seq &optional (start 0))
  "Enumerate elements in SEQ starting from START. Returns sequence of cons cells (n . element)"
  (map (type-of seq)
       (lambda (x) (cons (1- (incf start)) x))
       seq))

(defun seq-max (seq)
  "Maximum element in SEQ"
  (reduce #'max seq))

(defun seq-min (seq)
  "Minimum element in SEQ"
  (reduce #'min seq))

(defun scale (seq &optional (start 0) (end 1))
  "Scale values in SEQ into [START, END["
  (let* ((min (seq-min seq))
	 (max (seq-max seq))
	 (delta-start (coerce (- start min) 'double-float))
	 (delta-scale (coerce (/ (- (- end double-float-epsilon) start) (- max min)) 'double-float)))
    (map (type-of seq)
	 (lambda (x) 
	   (+ (* x delta-scale) 
	      delta-start))
	 seq)))

(defun demo()
  (let* ((width 400)
         (height 300)
         (margin 40)
         (title "Demo chart")
         (frame-width (- width (* 2 margin)))
         (frame-height (- height (* 2 margin)))
         (data-1 '(1 2 3 5 8)) 
         (data-2 #(10 4 28 15 20 10))
         (data-3 '(.1 .5 .3 .4 .25))
         (data-4 '(67 72 77 74 69))
         (data-5 '(155 220 240 195 175))
         (data-6 '(14.2 16.4 11.9 15.2 18.5 22.1 19.4 25.1 23.4 18.1 22.6 17.2))
         (data-7 '(215 325 185 332 406 522 412 614 544 421 445 408)))
    (vecto:with-canvas (:width width :height height)
      (vecto:translate .5 .5)  ; Anti-alias fix for 1-pixel lines
      (vecto:set-rgb-fill 1 1 1)
      (vecto:rectangle 0 0  width height)
      (vecto:fill-path)
      (vecto:set-rgb-stroke 0 0 0)
      (vecto:rectangle margin margin frame-width frame-height)
      (vecto:stroke)
      (vecto:set-font (vecto:get-font "/Library/Fonts/Times New Roman.ttf") 40)
      (vecto:set-rgb-fill 0 0 0 )
      (vecto:draw-centered-string (/ width 2) (- height 36) title)
      (vecto:set-rgb-stroke 0 0 0)
      (vecto:translate margin margin)      
      ; (graph-bars-grouped frame-width frame-height (list data-1 data-2))
      (graph-scatter-plot frame-width frame-height data-6 data-7)
      (vecto:save-png "vecto.png"))))
    
(defun transformer-old (size &rest seqs)
  "Return TRANSFORMER function to scale values in SEQS in scale of SIZE"
  (let* ((min (seq-min (mapcar #'seq-min seqs)))
	 (max (seq-max (mapcar #'seq-max seqs)))
	 (scale (/ size (- max min))))
    (lambda (x) 
      (* scale (- x min)))))

(defun transformer (from-min from-max to-min to-max)
  "Maps values from [FROM-MIN, FROM-MAX] into [TO-MIN, TO-MAX]"
  (let ((scale (/ (- to-max to-min) (- from-max from-min))))
    (lambda (x)
      (+ to-min (* scale (- x from-min))))))

(defun draw-y-axis (width height axis &key (left t) (tick-size 10))
  "Draw y-axis AXIS to the left of frame"
  (vecto:set-line-width 1)
  (let ((x (round (if left (- tick-size) width))))    
    (do* ((d (* (/ height (- (axis-end axis) (axis-start axis))) 
                (axis-tick-delta axis)))
          (l (axis-start axis) (+ l (axis-tick-delta axis)))
          (y 0 (+ y d)))
         ((> y height))
      (vecto:move-to x (round y))
      (vecto:line-to (+ x tick-size) (round y))
      (vecto:draw-string (if left (- 30) (+ width +5)) y (format nil "~A" l) )))
  (vecto:stroke))

(defun draw-x-axis (width axis &key (tick-size 10))
  "Draw x-axis AXIS to the bottom of frame"
  (vecto:set-line-width 1)
  (do ((d (* (/ width (- (axis-end axis) (axis-start axis)))
             (axis-tick-delta axis)))
       (l (axis-start axis) (+ l (axis-tick-delta axis)))
       (x 0 (+ x d)))
      ((> x width))
    (vecto:move-to (round x) 0)
    (vecto:line-to (round x) (- tick-size))
    (vecto:draw-string x (- tick-size 30) (format nil "~A" l)))
  (vecto:stroke))


(defun graph-scatter-plot (width height seq-a seq-b)
  "Scatter plot of sequences SEQ-A SEQ-B"
  (let* ((min-a (seq-min seq-a))
         (max-a (seq-max seq-a))
         (min-b (seq-min seq-b))
         (max-b (seq-max seq-b))
         (axis-x (create-axis min-a max-a))
         (axis-y (create-axis min-b max-b))
         (trans-x (transformer (axis-start axis-x) (axis-end axis-x) 0 width))
         (trans-y (transformer (axis-start axis-y) (axis-end axis-y) 0 height))
         (font (vecto:get-font "/Library/Fonts/Times New Roman.ttf")))
    (vecto:set-font font 10)
    (print axis-x)
    (print axis-y)
    (draw-y-axis width height axis-y)
    (draw-x-axis width axis-x)
    (map nil (lambda (x y)
               (vecto:centered-circle-path (funcall trans-x x) (funcall trans-y y) 2))
         seq-a seq-b))
  (vecto:fill-path))

(defun graph-lines (width height &rest seqs)
  "Draw line chart from SEQS into view size of WIDTH and HEIGHT"
  (vecto:set-line-width 3)
  (let ((trans-x (transformer 0 (seq-max (mapcar #'length seqs)) 0 width))
	(trans-y (transformer 0 (seq-max (mapcar #'seq-max seqs)) 0 height)))
    (dolist (s seqs)      
      (vecto:move-to 0 (funcall trans-y (elt s 0)))
      (map nil (lambda (x y) 
		 (vecto:line-to (funcall trans-x x) (funcall trans-y y)))
	   (range (length s)) s)
      (vecto:stroke))))

(defun graph-bars-grouped (width height seqs &key (category-gutter 8))
  "Draw grouped bars chart from SEQS into view size of WIDTH and HEIGHT"
  (let* ((ngroups (length seqs))
	 (ncategories (seq-max (mapcar #'length seqs)))
	 (category-width (/ width ncategories))
	 (bar-width (/ (- category-width category-gutter) ngroups))
         (min (seq-min (mapcar #'seq-min seqs)))
         (max (seq-max (mapcar #'seq-max seqs)))
         (y-axis (create-axis min max))
	 (trans-x (transformer 0 ncategories 0 (- width category-width)))
	 (trans-y (transformer (axis-start y-axis) (axis-end y-axis) 0 height))
         (group-delta 0)
         (font (vecto:get-font "/Library/Fonts/Times New Roman.ttf")))
    (vecto:set-font font 10)
    (print y-axis)
    (draw-y-axis width height y-axis)
    (draw-y-axis width height y-axis :left nil :tick-size 5)
    (dolist (s seqs)
      (map nil (lambda (x y)
                 (vecto:rectangle (+ (funcall trans-x x) group-delta) 0
                                  bar-width (funcall trans-y y)))
           (range (length s)) s)
      (incf group-delta bar-width)
      (apply #'vecto:set-rgb-fill (get-color (1+ (position s seqs))))
      (vecto:fill-path))))
