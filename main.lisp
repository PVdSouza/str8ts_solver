(defun itop (index)
    (cons
        (multiple-value-bind (q r) (floor index 6) r)
        (* (- index 6) (multiple-value-bind (q r) (floor index 6) r))
    )
)


(defun ptoi (x y)
    (+ (* x 6) y)
)


(defun isValid(index board)
    (if (= (nth index board) Nil)
        isValid (+ index 1)
        index
    )
)
