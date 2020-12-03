(defparameter *input* (uiop:read-file-lines "input_day1.txt"))

(setf *input* (map 'list #'parse-integer *input*))

(defun get-first-solution (frequencies)
  (reduce #'+ frequencies))

(defun test-me ()
  (let ((x "test") (y "this"))
    (format t "~a ~a ~%" x y)))


(defun get-second-solution (frequencies)
  (let
      (
       (recorded-frequencies (make-hash-table))
       (current-frequency 0)
       (input-index 0)
       )
    (loop
      while (equal (gethash current-frequency recorded-frequencies) nil)
      do
      (setf (gethash current-frequency recorded-frequencies) 1)
                                        ;(format t "Current Frequency: ~a~%" current-frequency)
      (setf current-frequency (+ (elt frequencies input-index) current-frequency))
      (setf input-index (+ input-index 1))
                                        ;(format t "Index: ~a~%" *input-index*)
      (if (= input-index (length frequencies))
          (setf input-index 0))
      finally (return current-frequency)
      )
    )
  )


(defun print-hash (hash)
  (maphash #'(lambda (key val)
               (format t "Key: ~a Val: ~a ~%" key val)) hash))
