;SEQ
(defun SEQ(N)
  (if (or (= N 0) (= N 1) (= N 2))
    1
    (+ (SEQ(- N 1)) (SEQ(- N 2)) (SEQ(- N 3)))
  )
)


