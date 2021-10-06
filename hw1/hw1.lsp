;SEQ
(defun SEQ(N)
  (if (or (= N 0) (= N 1) (= N 2))
    1
    (+ (SEQ(- N 1)) (SEQ(- N 2)) (SEQ(- N 3)))
  )
)

;SUMS
(defun SUMS(N)
  (if (or (= N 0) (= N 1) (= N 2))
    0
    (+ 2 (SUMS(- N 1)) (SUMS(- N 2)) (SUMS(- N 3))) 
  )
)

;ANON
(defun ANON(TREE)
  (cond ((not TREE) nil)
        ((atom TREE) '0)
        (t (cons(ANON(car TREE)) (ANON(cdr TREE))))
  )
)
