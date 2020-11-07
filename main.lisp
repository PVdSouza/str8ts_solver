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
    (mapcar (lambda (x) (nth (ptoi index x) board)) '(0 1 2 3 4 5))
)

(defun getColumn(index board)
    (mapcar (lambda (y) (nth (ptoi y index) board)) '(0 1 2 3 4 5))
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

(defun nextBlank (index board color)
    (if (= index (length board))
        (if (nth index color)
            (- (length board) 1)
            (length board)
        )
        
        (if (= (nth (+ index 1) board) 0)
            (if (nth (+ index 1) color)
                (+ index 1)
                (nextBlank (+ index 1) board color)
            )
        )
    )
)

(defun try (index board value)
    (setf (nth index board) value)
    (return-from try board)
)

(defun take-while-second(lista)
    (loop for item in lista
          while (nth 1 item)
          collect (nth 0 item))
)

(defun take-while (pred list)
    (loop for x in list
        while (funcall pred x)
        collect x)
)

(defun getSeqColumn (board color column)
    (splitOn (zip (getColumn column board) (getColumn column color)))
)

(defun zip (board color)
    (if (null board)
        ()
        (if (null color)
            ()
            (cons
                (list (car board) (car color))
                (zip (cdr board) (cdr color))
            )
        )
    )
)

(defun getSeqLine(board color line)
    (splitOn (zip (getRow line board) (getRow line color)))
)

(defun isFinished (color board)
    (set 'sequences (getSeqColumn board color 0))
    (actFinished sequences)
)


(defun actFinished(list)
    (if (null list)
        'TRUE)
    (if (not (isSequence (first list)))
        (nil)
    (isFinished (rest list))
    )
)

(defun isSequence(list)
    (= (- (nth (- (lentght list) 1) (sort 'list)) (nth  0 (sort 'list)))) ( - (length list) 1)
)