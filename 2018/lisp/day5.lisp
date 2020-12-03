(defun get-input ()
  (let (
        (f (uiop:read-file-string "input_day5.txt")))
    (subseq f 0 (- (length f) 1))))

(defun does-react (x y)
  "Returns T if x and y react with each other."
  (if (and(equalp (char-downcase x) (char-downcase y))
          (or (equalp (upper-case-p x) (lower-case-p y))
              (equalp (lower-case-p x) (upper-case-p y))))
      T))

(defun reduction (polymers)
  "Takes a list of polymers and performs a reduction based on the reaction rules. Returns a modified list."
  (let 
   ((p (loop
         with l = (length polymers)
         for i from 0 below l
         if (and (< (1+ i) l)
                 (does-react (aref polymers i) (aref polymers (1+ i))))
         do (setf i (1+ i))             ; Skip next val
         else
         collect (aref polymers i))))
    (make-array (length p) :initial-contents p)))

(defun complete-reduction (polymers)
  "Returns a completed reduction for a polymer, wherein no more reactions are possible."
  (let (
        (reduced-polymers (reduction polymers)))
    ;(format t "~a~%" (length polymers))
    (if (equalp (length polymers) (length reduced-polymers))
        reduced-polymers
        (complete-reduction reduced-polymers))))

(defun get-distinct-elements (polymers)
  "Returns a list of the distinct elements in a polymer."
  (let (
        (h (make-hash-table)))
    (loop
      with l = (length polymers)
      for i from 0 below l
      do (setf (gethash  (char-downcase (aref polymers i)) h) 1))
    (loop for key being the hash-keys of h collect key)))

(defun remove-polymers (polymers p)
  "Returns a new array with polymers of type p removed."
  (let (
        (remainder (remove-if #'(lambda (x) (equalp (char-downcase p) (char-downcase x))) polymers)))
    (make-array (length remainder) :initial-contents remainder)))

(defun get-shortest-improved-polymer (input)
  (let (
        (min -1)
        (min-polymer nil)
        (polymers (get-distinct-elements input))
        )
    (loop
      for p in polymers
      do
      (when (not (equalp min-polymer nil))
        (let ((new-length (length (complete-reduction (remove-polymers input p)))))
          (format t "min: ~a polymer: ~a~%" new-length p)
          (when (< new-length min)
           
            (setf min new-length)
            (setf min-polymer p))))
      (when (equalp min-polymer nil)
           (setf min (length (complete-reduction (remove-polymers input p))))
           (setf min-polymer p))
        
      )
    min))

(defun get-solution-one ()
  (length (complete-reduction (get-input))))
