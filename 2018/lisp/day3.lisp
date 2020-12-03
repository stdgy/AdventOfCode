(defparameter *a* (make-array '(1001 1001) :initial-element nil))
(defparameter *input* (uiop:read-file-lines "input_day3.txt"))
(defparameter *claims* (create-claims *input*))

; Claims look like ((:id 1)(:left-padding 1)(:top-padding 1)(:width 1)(:height 1))
(defun create-claims (input)
  (loop
    with id = 0
    with left-padding = 0
    with top-padding = 0
    with width = 0
    with height = 0
    for c in input
    do
    (setf id (parse-integer c :start (+ (position #\# c) 1) :end (position #\@ c)))
    (setf left-padding (parse-integer c :start (+ 1 (position #\@ c))
                                        :end (position #\, c)))
    (setf top-padding (parse-integer c :start (+ 1 (position #\, c))
                                       :end (position #\: c)))
    (setf width (parse-integer c :start (+ 1 (position #\: c))
                                 :end (position #\x c)))
    (setf height (parse-integer c :start (+ 1 (position #\x c))))
    collect (list
             (list :id id)
             (list :left-padding left-padding)
             (list :top-padding top-padding)
             (list :width width)
             (list :height height))
    ))

(defun does-claim-overlap (claim fabric)
  (loop
    with claim-id = (get-key claim :id)
    for x from (get-key claim :left-padding) below (+ (get-key claim :left-padding) (get-key claim :width))
    do (if (loop
             for y from (get-key claim :top-padding) below (+ (get-key claim :top-padding) (get-key claim :height))
             do
             ;(format t "~a ~a ~%" claim-id (aref fabric y x))
             (if (not (equalp (aref fabric y x) claim-id))
                 (return T))
             finally (return nil))
           (return T))
    finally (return nil)))

(defun get-non-overlapping-claim (claims fabric)
  (loop
    for claim in claims
    do (if (not (does-claim-overlap claim fabric))
           (return claim))))

(defun get-key (claim key)
  (elt (find-if #'(lambda (item) (equalp (first item) key)) claim) 1))

(defun process-claim (claim fabric)
  "Fills in the fabric array using the claim details. It inserts the claim's id or 0 for spots with multiple claims."
  (loop
    for x from (get-key claim :left-padding) below (+ (get-key claim :left-padding) (get-key claim :width))
    do (loop
         for y from (get-key claim :top-padding) below (+ (get-key claim :top-padding) (get-key claim :height))
         do
         (if (equalp (aref fabric y x) nil)
             (setf (aref fabric y x) (get-key claim :id))
             (setf (aref fabric y x) 0))))
    
    ))

(defun process-claims (claims fabric)
  (loop
    for claim in claims
    do
    (process-claim claim fabric)
    )
  )

(defun count-overlap (fabric)
  (loop
    with total-overlap = 0
    for x from 0 to 1000
    do (loop
         for y from 0 to 1000
         do (if (equalp (aref fabric x y) 0)
                (setf total-overlap (1+ total-overlap)))
            
         )

      
    finally (return total-overlap)))

(defun get-solution-one ()
  (let (
        (fabric (make-array '(1001 1001) :initial-element nil))
        (claims (create-claims (uiop:read-file-lines "input_day3.txt")))
        )
    (process-claims claims fabric)
    (count-overlap fabric)))

(defun get-solution-two ()
  (let (
        (fabric (make-array '(1001 1001) :initial-element nil))
        (claims (create-claims (uiop:read-file-lines "input_day3.txt")))
        )
    (process-claims claims fabric)
    (get-non-overlapping-claim claims fabric)
    )
  )
