                                        ; Task:
                                        ; 1) Find bounding box.
                                        ; 2) Establish function that takes (x,y) and returns nearest point.
                                        ; 3) Establish function that returns list of infinite points.
                                        ; 4) Iterate through bounding box, keeping track of point sums in hash map.
                                        ; 5) Remove infinite keys from hash map.
                                        ; 6) Get key with largest remaining value.

(defun parse-coordinate (line id)
  (list
   :x (parse-integer (subseq line 0 (position #\, line)))
   :y (parse-integer (subseq line (1+ (position #\, line))))
   :id id))

(defun get-coordinates ()
  (loop
    with id = #\A
    for line in (uiop:read-file-lines "input_day6.txt")
    collect (parse-coordinate line id)
    do (setf id (code-char (+ (char-code id) 1)))))

(defun get-bounding-box (coordinates)
  (list
   (list :x (- (reduce #'min (map 'list #'(lambda (coord) (getf coord :x)) coordinates)) 1)
         :y (- (reduce #'min (map 'list #'(lambda (coord) (getf coord :y)) coordinates))) 1)
   (list :x (+ 100 (reduce #'max (map 'list #'(lambda (coord) (getf coord :x)) coordinates)))
         :y (+ 100 (reduce #'max (map 'list #'(lambda (coord) (getf coord :y)) coordinates))))))

(defun manhattan-distance (c1 c2)
  (+
   (abs (- (getf c1 :x) (getf c2 :x)))
   (abs (- (getf c1 :y) (getf c2 :y)))))

(defun get-nearest-point (coordinates x y)
  (loop
    with min-cost = (manhattan-distance (elt coordinates 0)
                                        (list :x x :y y))
    with point = (list :x x :y y)
    with min-pnt = (getf (car coordinates) :id)
    with min-cnt = 1
    for coord in (cdr coordinates)
    do (when (equalp (manhattan-distance coord point) min-cost)
         (setf min-cnt (1+ min-cnt)))
       (when (< (manhattan-distance coord point) min-cost)
         (setf min-cost (manhattan-distance coord point))
         (setf min-cnt 1)
         (setf min-pnt (getf coord :id)))
    finally (return (if (= min-cnt 1)
                 min-pnt
                 #\.))))

(defun get-infinite-ids (coordinates bounding-box)
  (remove-duplicates
   (concatenate 'list
                (loop ; Top row
                  with top-y = (getf (elt bounding-box 0) :y)
                  with x1 = (getf (elt bounding-box 0) :x)
                  with x2 = (getf (elt bounding-box 1) :x)
                  for x from x1 to x2
                  collect (get-nearest-point coordinates x top-y))
                (loop ; Bottom row
                  with bottom-y = (getf (elt bounding-box 1) :y)
                  with x1 = (getf (elt bounding-box 0) :x)
                  with x2 = (getf (elt bounding-box 1) :x)
                  for x from x1 to x2
                  collect (get-nearest-point coordinates x bottom-y))
                (loop ; Left row
                  with left-x = (getf (elt bounding-box 0) :x)
                  with y1 = (getf (elt bounding-box 0) :y)
                  with y2 = (getf (elt bounding-box 1) :y)
                  for y from y1 to y2
                  collect (get-nearest-point coordinates left-x y))
                (loop ; Right row
                  with right-x = (getf (elt bounding-box 1) :x)
                  with y1 = (getf (elt bounding-box 0) :y)
                  with y2 = (getf (elt bounding-box 1) :y)
                  for y from y1 to y2
                  collect (get-nearest-point coordinates right-x y)))))

(defun get-solution-one ()
  (let (
        (coordinates (get-coordinates))
        (bounding-box (get-bounding-box (get-coordinates)))
        (counts (make-hash-table)))
    
    (loop
      with start-x = (getf (elt bounding-box 0) :x)
      with end-x = (getf (elt bounding-box 1) :x)
      for x from start-x to end-x
      do (loop
           with start-y = (getf (elt bounding-box 0) :y)
           with end-y = (getf (elt bounding-box 1) :y)
           with nearest-point = nil
           for y from start-y to end-y
           do (setf nearest-point (get-nearest-point coordinates x y))
              (setf (gethash nearest-point counts) (+ 1 (gethash nearest-point counts 0)))))
    (loop
      for id in (get-infinite-ids coordinates bounding-box)
      do (remhash id counts))
    (loop for k being the hash-keys in counts using (hash-value v)
          do (format t "K: ~a V: ~a ~%" k v))))

(defun print-solution (coordinates bounding-box)
  (loop
    with y1 = (getf (elt bounding-box 0) :y)
    with y2 = (getf (elt bounding-box 1) :y)
    for y from y1 to y2
    do (loop
         with x1 = (getf (elt bounding-box 0) :x)
         with x2 = (getf (elt bounding-box 1) :x)
         for x from x1 to x2
         do (format t "~a" (get-nearest-point coordinates x y)))
       (format t "~%")))

(defun is-safe-region (coordinates x y safe-distance)
  (< (reduce #'+
             (map 'list #'manhattan-distance
                  coordinates
                  (make-array (length coordinates) :initial-element (list :x x :y y))))
     safe-distance))

(defun get-solution-two (coordinates bounding-box)
  (loop
    with region-count = 0
    with start-x = (getf (elt bounding-box 0) :x)
    with end-x = (getf (elt bounding-box 1) :x)
    for x from start-x to end-x
    do (loop
         with start-y = (getf (elt bounding-box 0) :y)
         with end-y = (getf (elt bounding-box 1) :y)
         for y from start-y to end-y
         do (when (is-safe-region coordinates x y 10000)
              (setf region-count (1+ region-count))))
    finally (return region-count)))
