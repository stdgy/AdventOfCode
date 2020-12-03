
(defun parse (l)
  (let ((num-children (first l))
        (num-metadata (second l)))
    (loop
      with rest = (nthcdr 2 l)
      with metadata = (list)
      for i from 1 to num-children
      do (multiple-value-bind (remaining m)
             (parse rest)
           (setq rest remaining)
           (setq metadata (append metadata m)))
      finally (return (values (nthcdr num-metadata rest) (append metadata (subseq rest 0 num-metadata)))))))

(defvar *l* (list 2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2))

(defun file-to-list (filename)
   (string-to-integers (uiop:read-file-string filename) 0))

(defun string-to-integers (s start)
  (if start
    (cons
     (parse-integer s :start start :junk-allowed T)
     (string-to-integers s (position #\Space s :start (1+ start))))
    nil))

(defun get-solution-one ()
  (reduce #'+ (second (multiple-value-list
                       (parse (file-to-list "input_day8.txtt"))))))

(defun double-numbers (numbers)
  (if numbers
      (cons (* (first numbers) 2) (double-numbers (rest numbers)))
      nil))

(defun (test)
    ((adfasdf)))
