(set 'numberBoard '(3 0 0 4 0 0
                    0 0 0 0 0 0
                    0 0 0 0 0 2
                    4 0 0 0 0 3
                    0 0 0 0 0 5
                    0 0 3 0 1 0))


(set 'colorBoard '(NIL NIL T  T  T  NIL
                   NIL T   T  T  T  NIL
                   T   T  NIL NIL T  T
                   T   T  NIL NIL T  T
                   NIL T  T  T  T   NIL
                   NIL T  T  T  NIL NIL))


(defun itop (index)
    (cons
        (new-div index 6)
        (- index (* (new-div index 6) 6))
    )
)

(defun new-div (x y)
    (if (>=(- x y) 0)
        (+ 1 (new-div (- x y) y))
        0
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

(defun getRow(index board)
    (mapcar (lambda (x) (nth (ptoi x index) board)) '(0 1 2 3 4 5))
)

(defun getColumn(index board)
    (mapcar (lambda (y) (nth (ptoi index y) board)) '(0 1 2 3 4 5))
)

(defun getSequences (board color index)
    (if (= index 6)
        (cons (cons (getSeqColumn board color index) (getSeqLine board color index)) (getSequences board color (+ index 1)))
    )
)

(defun filter-nil (lista)
    (remove-if #'(lambda (x) (not x)) lista)
)

(defun splitOn (lista)
    (if (null lista)
        ()
        (filter-nil (cons (take-while-second lista) (splitOn (drop-while-not (drop-while lista)))))
    )
)

(defun drop-while-not (lista)
    (if (null lista)
        ()
        (if (not (nth 1 (car lista)))
            (drop-while-not (cdr lista))
            lista
        )
    )
)

(defun drop-while (lista)
    (if (null lista)
        ()
        (if (nth 1 (car lista))
            (drop-while(cdr lista))
            lista
        )
    )
)

(defun take-while-second(lista)
    (loop for item in lista
          while (nth 1 item)
          collect (nth 0 item))
    ;; (collect-list (list i) (for i in lista) (take-while ()))
)

(defun take-while (pred list)
    (loop for x in list
        while (funcall pred x)
        collect x)
)

(defun getSeqColumn ()
)

(defun getSeqLine()
)

(defun isFinished()
)

(defun isSequence()
)
