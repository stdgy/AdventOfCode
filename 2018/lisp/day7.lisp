(defun parse-line (line)
  (list :step (elt line 36)
        :prev (elt line 5)))


(defun parse-input ()
  (map 'list #'parse-line (uiop:read-file-lines "day7_input.txt")))

(defun parse-required-steps ()
  (let ((h (make-hash-table :test 'equalp)))
    (loop
      with requirements = (parse-input)
      with step = nil
      with prev = nil
      for requirement in requirements
      do (setq step (getf requirement :step))
         (setq prev (getf requirement :prev))
         
         (if (last (gethash step h))
             (setf (gethash step h) (append (gethash step h) (list prev)))
             (setf (gethash step h) (list prev)))
      finally (return h))))

(defun get-nodes ()
  "Returns all of the nodes."
  (remove-duplicates
   (concatenate 'list
                (map 'list #'(lambda (requirement) (getf requirement :prev)) (parse-input))
                (map 'list #'(lambda (requirement) (getf requirement :step)) (parse-input)))
   :test 'equalp))

(defun get-remaining-nodes (nodes visited-nodes)
  "Returns all of the nodes not yet visited."
  (remove-if #'(lambda (node) (position node visited-nodes)) nodes))

(defun is-visitable (node visited-nodes requirements)
  "Returns whether all of a node's requirements have been visited."
  (every #'(lambda (required-node) (position required-node visited-nodes))
         (gethash node requirements)))

(defun get-allowed-nodes (nodes visited-nodes requirements)
  "Returns a list of nodes we are allowed to visit"
  (remove-if-not #'(lambda (node) (is-visitable node visited-nodes requirements)) (get-remaining-nodes nodes visited-nodes)))

(defun initialize-allowed-nodes (nodes requirements)
  (remove-if #'(lambda (node) (last (gethash node requirements))) nodes))

(defun get-solution-one ()
  (let (
        (nodes (get-nodes))
        (requirements (parse-required-steps))
        (allowed-nodes nil)
        (visited-nodes (list))
        (min-node nil))
    (setq allowed-nodes (initialize-allowed-nodes nodes requirements))
    (loop
      while (> (length allowed-nodes) 0)
      ; Move minimum allowed node to visited nodes
      do (setf min-node (reduce #'(lambda (c1 c2) (if (char< c1 c2)
                                                      c1
                                                      c2)) allowed-nodes))
         (push min-node visited-nodes)
                                        ; Remove minimum node from allowed nodes
         (setf allowed-nodes (remove-if #'(lambda (node) (equalp node min-node)) allowed-nodes))
                                        ; Re-calculate allowed nodes 
         (setf allowed-nodes (remove-duplicates (concatenate 'list allowed-nodes (get-allowed-nodes nodes visited-nodes requirements)) :test 'equalp))
         ;(print min-node)
         ;(print allowed-nodes)
         ;(print visited-nodes)
      )
    (format nil "~{~a~}" (reverse visited-nodes))))
