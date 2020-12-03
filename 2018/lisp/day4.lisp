                                        ; Design of struct for storing data types.
                                        ; (
                                        ;  (:date yyyy mm dd hh mi)
                                        ;  (:type "falls asleep" "wakes up" "begins shift")
                                        ;  (:guard number)
                                        ; )

(defun parse-year (line)
  (subseq line 1 5))

(defun parse-month (line)
  (subseq line 6 8))

(defun parse-day (line)
  (subseq line 9 11))

(defun parse-hour (line)
  (subseq line 12 14))

(defun parse-minute (line)
  (subseq line 15 17))

(defun parse-date (line)
  (parse-integer (concatenate 'string
                              (parse-year line)
                              (parse-month line)
                              (parse-day line)
                              (parse-hour line)
                              (parse-minute line))))

(defun parse-type (line)
  (case (aref line 19)
    (#\w :wakes-up)
    (#\f :falls-asleep)
    (#\G :begins-shift)))

(defun parse-guard (line)
  (if (equalp (aref line 19) #\G)
      (parse-integer line
                     :start 26
                     :end (position #\space line :start 26))))

(defun parse-line (line)
  (list
   :date (parse-date line)
   :type (parse-type line)
   :guard (parse-guard line)))

(defun parse-input ()
  (let (
        (input (uiop:read-file-lines "input_day4.txt"))
        )
    (loop
      for line in input
      collect (parse-line line))
    ))

(defun get-sorted-input ()
  (Let (
        (input (parse-input)))
    (sort input #'(lambda (x y) (< (getf x :date) (getf y :date))))))


(defun assign-guards (input)
  (loop
    with last-guard = nil
    for line in input
    do (if  (equalp (getf line :guard) nil)
            (setf (getf line :guard) last-guard)
            (setf last-guard (getf line :guard)))
    finally (return input)))

(defun get-guard-asleep-most (input)
  (let (
        (guard-to-minutes (make-hash-table))
        )
    (loop
      with last-fell-asleep = nil
      for line in input
      do (if (equalp (getf line :type) :FALLS-ASLEEP)
             (setf last-fell-asleep (getf line :date)))
         (if (equalp (getf line :type) :WAKES-UP)
             (setf (gethash
                    (getf line :guard)
                    guard-to-minutes)
                    (+ (gethash (getf line :guard) guard-to-minutes 0) (- (getf line :date) last-fell-asleep)))))
    (loop
      with max = 0
      with guard = nil
      for k being the hash-keys in guard-to-minutes using (hash-value v)
      Do 
         (when (> v max)
           (setf guard k)
           (setf max v))
      finally (return guard))))

                                        ; (get-guard-asleep-most (assign-guards (get-sorted-input)))

(defun get-minute-most-asleep (guard-id input)
  (let (
        (time (make-array 60))
        (g-inputs (remove-if-not #'(lambda (l) (equalp (getf l :guard) guard-id)) input))
        )
    (loop
      with fell-asleep = nil
      for i in g-inputs
      do (if (equalp (getf i :type) :FALLS-ASLEEP)
             (setf fell-asleep (getf i :date)))
         (if (equalp (getf i :type) :WAKES-UP)
             (loop
               with start = (mod fell-asleep 100)
               with end = (mod (getf i :date) 100)
               for x from start below end
               do (setf (elt time x) (1+ (elt time x))))))
    (loop
      with max-time = 0
      for i from 0 to 59
      do (if (> (elt time i) (elt time max-time))
             (setf max-time i))
      finally (return max-time))
    ))


(defun get-solution-one ()
  (let (
        (input (assign-guards (get-sorted-input)))
        (guard-id (get-guard-asleep-most (assign-guards (get-sorted-input))))
        )
    (* (get-minute-most-asleep guard-id input) guard-id)))

(defun record-mins-slept (time start end)
  (loop
    for s from (mod start 100) below (mod end 100)
    do (setf (elt time s)
             (1+ (elt time s)))
    finally (return time))
)

(defun get-solution-two ()
  (let (
        (input (assign-guards (get-sorted-input)))
        (h (make-hash-table))
        )
    (loop
      with fell-asleep = 0
      with woke-up = 0
      with guard-id = 0
      for line in input
      do (if (equalp (getf line :type) :FALLS-ASLEEP)
             (setf fell-asleep (getf line :date)))
         (when (equalp (getf line :type) :WAKES-UP)
           (setf woke-up (getf line :date))
           (setf guard-id (getf line :guard))
           (setf (gethash guard-id h) (record-mins-slept
             (gethash guard-id h (make-array 60))
             fell-asleep
             woke-up))
           ))
    (loop
      with guard-id = 0
      with max = 0
      with minute = 0
      for k being the hash-keys in h using (hash-value v)
      do (loop
           for i from 0 to 59
           do (when (> (aref v i) max)
                (setf max (aref v i))
                (setf guard-id k)
                (setf minute i)))
      finally (return (* guard-id minute)))))
