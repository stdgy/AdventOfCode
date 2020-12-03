(defparameter *input* (uiop:read-file-lines "input_day2.txt"))

(defun has-n-of-same-letter (word n)
  (loop
    for c across word
    do (if (= (count c word) n)
           (return T))))

(defun get-solution-one (input)
  (*
   (length (remove-if-not #'(lambda (x) (has-n-of-same-letter x 2)) input))
   (length (remove-if-not #'(lambda (x) (has-n-of-same-letter x 3)) input))))

(defun remove-nth-char (input n)
  "Takes a list of strings and returns them with their nth character removed."
  (map 'list #'(lambda (x) (concatenate 'string (subseq x 0 n) (subseq x (+ n 1)))) input))

(defun get-common-string (input)
  "Takes a list of strings as input and returns a list of unique strings that were in the input list more than once."
  (remove-duplicates (remove-if #'(lambda (x) (= (count x input :test #'equalp) 1)) input)
                     :test #'equalp))


(defun get-solution-two (input)
  (loop
    with common-string = ()
    for i from 0 to (length input)
    do
    (setq common-string (get-common-string (remove-nth-char input i)))
    (if (= (length common-string) 1)
        (return common-string))))
