;SEQ
#|
This function input an non-negative number N, and returns Nth padovan number. 
Explanation:
By definition,for n>=3, SEQ(n) = SEQ(n-1) + SEQ(n-2) + SEQ(n-3) with SEQ(0) = SEQ(1) = SEQ(2) = 1. Hence for n<3, we output 1 by definition; for n>=3, we create recursionfuctions that compute SEQ with input n-1, n-2, and n-3, and add them together as the result.
|#
(defun SEQ(N)
  (if (or (= N 0) (= N 1) (= N 2)) ;base
    1
    (+ (SEQ(- N 1)) (SEQ(- N 2)) (SEQ(- N 3))) ;recursion
  )
)

;SUMS
#|
This function input an non-negative number N, and returns the number of addition required by SEQ to compute Nth padovan number. 
Explanation:
We know that for base case N=1, N=2, and N=3, we do not need addtion. For SEQ(N-1) + SEQ(N-2) + SEQ(N-3), two addition processes are needed. Hence we add 2 onto the result for every level of recursion. 
|#
(defun SUMS(N)
  (if (or (= N 0) (= N 1) (= N 2)) ;base
    0
    (+ 2 (SUMS(- N 1)) (SUMS(- N 2)) (SUMS(- N 3))) ;recursion
  )
)

;ANON
#|
For ANON, we input a TREE, output an anonymized tree with the same structure, but where all symbols and numbers in the tree are replaced by 0. 

Explanation:
Base case: 
If the TREE does not exist, we return NIL; else if TREE is a leaf node, we return 0 as the leaf of our reulting tree; 

Recursion:
Else we compute recursive functions for the first subtree and the remaining list of subtrees corresspondingly. 
|#
(defun ANON(TREE)
  (cond ((not TREE) nil) ;the TREE is empty
        ((atom TREE) '0) ;TREE is a leaf
        (t (cons(ANON(car TREE)) (ANON(cdr TREE)))) ;recursion
  )
)
