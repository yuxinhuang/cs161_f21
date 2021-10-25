;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

;helper function: check if the box is in the row
;input a row
;output a boolean: true if we find a box in the row, nil otherwise
(defun isBoxRow(row)
	(cond
		;return nil if the row is null
		((null row) nil)
		;return true if we find a box at front of the row
		((isBox (car row)) t)
		;otherwise recusively find the box in the rest of the row
		(t (isBoxRow(cdr row)))
	)
)
; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.

;input s: a list of lists
;iterate through every row of s
; return true if no box (2) exists
;otherwise return nil
(defun goal-test (s)
	(cond
		;base: s is null
		((null s) t)
		;check the first row of s
		;if a box (2) exists, it cannot be a goal state
		((isBoxRow (car s)) nil)
		;recursively check the rest of s
		(t (goal-test (cdr s)))

	)
 );end defun

;get-square
;s: the current state s
;r: a row number
;c: a column number
;return: the integer content of state S at (r,c).
;If it is outside scope, return WALL (1)

(defun get-square (s r c)
	(cond
		;base case: (r,c) out of range
		((or (null s) (null (car s))) 1)
		;or there is no wall on the edge of s
		((or (< r 0) (< c 0)) 1)
		;we found (r,c)
		((and (= r 0) (= c 0)) (car (car s)))
		;if not found, and row number larger than 0, minus 1 on row number
		((> r 0) (get-square (cdr s) (- r 1) c))
		;iterate through the row recursively by minus 1 on col number 
		((> c 0) (get-square (cons (cdr (car s)) (cdr s)) r (- c 1)))
	)
	
)

;set-square
;s: the current state s
;r: a row number
;c: a column number
;v: a square content
;return: a new state s’ that is obtained by setting the square (r,c) to value v.
(defun set-square (s r c v)
	(cond
		;base case: (r,c) out of range, return the same state
		((or (null s) (null (car s))) s)
		;at (r,c) setting the square (r,c) to v, and attach the rest of s
		((and (= r 0) (= c 0)) (cons (cons v (cdr (car s))) (cdr s)))
		;if not found in the current row, and row number larger than 0, minus 1 on row number
		;cons the current row and recursively process the rest of rows
		((> r 0) (cons (car s) (set-square (cdr s) (- r 1) c v)))
		;if it is at the current row
		;iterate through the row recursively by minus 1 on col number 
		;cons the current col and recursively process the rest of cols
		((> c 0) (let ((res (set-square (cons (cdr (car s)) (cdr s)) r (- c 1) v)))
			(cons (cons (car (car s)) (car res)) (cdr res))
		))



	)
)

;try-move-keeper
;helper function for try-move
;s: the current state s
;r: the keeper's row
;c: the keeper's column
;v: the keeper's content
;v1: the square content one cell away from the keeper on the direction
;v2: the square content two cells away from the keeper on the direction
;output the state after keeper's movement

(defun try-move-keeper (s r c v v1 v2)
	(cond
		;the keeper moves into a wall, return nil
		((isWall v1) NIL)
		;the keeper is pushing a box, but the box is not moving to the empty or star square
	    ;return nil
	    ((and (isBox v1) (not (or (isBlank v2) (isStar v2)))) NIL)	
	    ;otherwise, move the keeper
	    ;keeper only (3), change square (r,c) to blank
	    ((equal v keeper) (set-square s r c blank))
	    ;keeper and star(6),change square (r,c) to star
	    ((equal v keeperstar) (set-square s r c star)) 
	)

)

;try-move
;to check whether the move is legal
;s: the current state s
;D: a move direction
;return the state that is the result of moving the keeper in state s in direction d
;if the move is invalid, return nil

(defun try-move (s D)
	;r: the keeper's row
	;c: the keeper's column
	;v: the keeper's value
	(let* 
		(
			(pos (getKeeperPosition s 0))
			(r (car (cdr pos)))
			(c (car pos))
			(v (get-square s r c))

		)
		(cond
			;direction1, direction2: square contents of two cells on the direction
			;temp: state after the keeper is moved
			;up
			((equal D 'u) (let* ((up1 (get-square s (- r 1) c)) (up2 (get-square s (- r 2) c)) (temp (try-move-keeper s r c v up1 up2)))
				(cond
					;if the upper square is blank, move keeper
					((isBlank up1) (set-square temp (- r 1) c keeper))
					;if the upper square is a box, and the square above the box is blank
					;move the box to upper, change the state
					((and (isBox up1) (isBlank up2)) (set-square (set-square temp (- r 1) c keeper) (- r 2) c box))
					;if the upper square is a box, and the square above the box is the goal
					;move the box to upper, change the state
					((and (isBox up1) (isStar up2)) (set-square (set-square temp (- r 1) c keeper) (- r 2) c boxstar))
					;if the upper square is star
					((isStar up1) (set-square temp (- r 1) c keeperstar))
					;if the upper square is star, and the square above the box is blank
					((and (isBoxStar up1) (isBlank up2)) (set-square (set-square temp (- r 1) c keeperstar) (- r 2) c box))
					;if the upper square is boxstar, and the square above the box is star
					((and (isBoxStar up1) (isStar up2)) (set-square (set-square temp (- r 1) c keeperstar) (- r 2) c boxstar))
				)
			))

			; down 
			((equal D 'd) (let* ((down1 (get-square s (+ r 1) c)) (down2 (get-square s (+ r 2) c)) (temp (try-move-keeper s r c v down1 down2)))
				(cond
					((isBlank down1) (set-square temp (+ r 1) c keeper))
					((and (isBox down1) (isBlank down2)) (set-square (set-square temp (+ r 1) c keeper) (+ r 2) c box))
					((and (isBox down1) (isStar down2)) (set-square (set-square temp (+ r 1) c keeper) (+ r 2) c boxstar))
					((isStar down1) (set-square temp (+ r 1) c keeperstar))
					((and (isBoxStar down1) (isBlank down2)) (set-square (set-square temp (+ r 1) c keeperstar) (+ r 2) c box))
					((and (isBoxStar down1) (isStar down2)) (set-square (set-square temp (+ r 1) c keeperstar) (+ r 2) c boxstar))
				)
			))

			;left
			((equal D 'l) (let* ((left1 (get-square s r (- c 1))) (left2 (get-square s r (- c 2))) (temp (try-move-keeper s r c v left1 left2)))
				(cond 
					((isBlank left1) (set-square temp r (- c 1) keeper))
					((and (isBox left1) (isBlank left2)) (set-square (set-square temp r (- c 1) keeper) r (- c 2) box))
					((and (isBox left1) (isStar left2)) (set-square (set-square temp r (- c 1) keeper) r (- c 2) boxstar))
					((isStar left1) (set-square temp r (- c 1) keeperstar))
					((and (isBoxStar left1) (isBlank left2)) (set-square (set-square temp r (- c 1) keeperstar) r (- c 2) box))
					((and (isBoxStar left1) (isStar left2)) (set-square (set-square temp r (- c 1) keeperstar) r (- c 2) boxstar))
				)
			))

			;right
			((equal D 'r) (let* ((right1 (get-square s r (+ c 1))) (right2 (get-square s r (+ c 2))) (temp (try-move-keeper s r c v right1 right2)))
				(cond
					((isBlank right1) (set-square temp r (+ c 1) keeper))
					((and (isBox right1) (isBlank right2)) (set-square (set-square temp r (+ c 1) keeper) r (+ c 2) box))
					((and (isBox right1) (isStar right2)) (set-square (set-square temp r (+ c 1) keeper) r (+ c 2) boxstar))
					((isStar right1) (set-square temp r (+ c 1) keeperstar))
					((and (isBoxStar right1) (isBlank right2)) (set-square (set-square temp r (+ c 1) keeperstar) r (+ c 2) box))
					((and (isBoxStar right1) (isStar right2)) (set-square (set-square temp r (+ c 1) keeperstar) r (+ c 2) boxstar))
				)
			))
		)
	)

)

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;
(defun next-states (s)
;  (let* ((pos (getKeeperPosition s 0))
;	 (x (car pos))
;	 (y (cadr pos))
;	 ;x and y are now the coordinate of the keeper in s.
;	 (result nil)
;	 )
 ;   (cleanUpList result);end
  ; );end let
  (cleanUpList (list (try-move s 'u) (try-move s 'd) (try-move s 'l) (try-move s 'r)))
);

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0
 )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;h1
;s: the current state
;returns the number of boxes which are not on goal positions in the given state
(defun h1 (s)
	(cond
		;base case: s is null, return zero
		((null s) 0)

		;recusively process the rest of the row if the first row is null
		((null (car s)) (h1 (cdr s)))

		;in the middle of the row
		;if a box is found, +1 to h1
		;otherwise continue searching 
		((list (car s)) 
			(if 
				(isBox (caar S)) 
				(+ 1 (h1 (cons (cdar S) (cdr S)))) 
				;else
				(h1 (cons (cdar S) (cdr S)))
		    )
		)
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;helper function for h105711853;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;findKeeperPosition
;find the keeper's position
;s: the current state
;r: row number
;c: column number
;return (r,c)
(defun findKeeperPosition(s r c)
	(cond
		;no keeper
		((null s) '())
		;end of row
		((null (car s)) (findKeeperPosition (cdr s) (+ r 1) 0))
		;finding in the row
		((list (car s))
			;if we find the keeper, return (r,c) as a list
			(if (or (isKeeper (caar s)) (isKeeperStar(caar s)))
				(list r c)
				;else keep searching
				(findKeeperPosition (cons (cdar s) (cdr s)) r (+ c 1))
			)
		)
	)
)


;findGoalPositions
;find all goals
;s: current state
;r: row number
;c: column number
;return a list of goals' location tuple as a list
(defun findGoalPositions (s r c)
	(cond
		;no goal
		((null s) '())
		;end of row
		((null (car s)) (findGoalPositions (cdr s) (+ r 1) 0))
		;finding in the row
		((list (car s))
			;if we find the goal, return (r,c) as a list
			(if (or (isStar (caar s)) (isKeeperStar(caar s)))
				(append (list (list r c)) (findGoalPositions (cons (cdar s) (cdr s)) r (+ c 1)))
				;else keep searching
				(findGoalPositions (cons (cdar s) (cdr s)) r (+ c 1))
			)
		)
	)
)

;findBoxPositions
;find all boxes
;s: current state
;r: row number
;c: column number
;return a list of boxes' location tuple as a list
(defun findBoxPositions (s r c)
	(cond
		;no box
		((null s) '())
		;end of row
		((null (car s)) (findBoxPositions (cdr s) (+ r 1) 0))
		;finding in the row
		((list (car s))
			;if we find the box, return (r,c) as a list
			(if (isBox (caar s))
				(append (list (list r c)) (findBoxPositions (cons (cdar s) (cdr s)) r (+ c 1)))
				;else keep searching
				(findBoxPositions (cons (cdar s) (cdr s)) r (+ c 1))
			)
		)

	)
)

;minEle
;min element of a list
;list: a list
;x: large
(defun minEle (list x)
	(cond
		;return x if list is null
		((null list) x)
		; if the first element is smaller than x, x = the first element of the list, and search the rest of the list
		((< (car list) x) (minEle (cdr list) (car list)))
		;else contnue searching without renewing x
		(t (minEle (cdr list) x))
	)
)

;L1-helper
;L1 distance between two tuples
;source: the source tuple
;dest: the destination tuple
;return the L1 distance of source and dest
(defun L1-helper (source dest)
	(cond
		;if one of the tuple is null
		((or (null source) (null dest)) 0)
		;else
		(t (+ (abs (- (car dest) (car source))) (abs (- (cadr dest) (cadr source)))))
	)
)

;L1
;L1 distance between a source and a list of tuples
;source: the source tuple
;list: a list of dest
;return a list of L1 distance
(defun L1 (source list)
	(cond
		; if the dest list is null
		((null list) '())
		;else
		(t (let ((dest (car list)))
			(append (list (L1-helper source dest)) (L1 source (cdr list)))
			)
		)
	)
)

;sum-of-list
;sum of the list's elements
;list: a list
;return the sum
(defun sum-of-list (list)
	(cond
		;if the list in null
		((null list) 0)
		;else
		(t (+ (car list) (sum-of-list (cdr list))))
	)
)

;opt-goal
;source: the source tuple(box)
;dest-list: a list of destinations (goals)
;dest: the goal
;returns a tuple of optimal goal with respect to the source box

(defun opt-goal (source dest-list dest)
	(cond
		;if dest-list is null
		((null dest-list) dest) 
		;else if the first element of dest-list is closer to soure than current dest
		;renew dest to the first element of dest list, and continue finding
		((< (L1-helper source (car dest-list)) (L1-helper source dest)) (opt-goal source (cdr dest-list) (car dest-list)))
		(t (opt-goal source (cdr dest-list) dest))
	)
)

;sum-of-opt-goal-L1
;box-list: a list of box positions
;goal-list: a list a goal positions
;return the sum of minimum L1 distance between a box and its closest goal

(defun sum-of-opt-goal-L1 (box-list goal-list)
	(cond
		;if the box-list is null
		((null box-list) 0)
		;else
		(t (let ((source (car box-list)))
			(+ (minEle (L1 source goal-list) most-positive-fixnum) (sum-of-opt-goal-L1 (cdr box-list) goal-list))
			)
		)
	)


)

;isBoxBlocked
;if the box is stuck
;s: the current state
;box: a list 
;return boolean
(defun isBoxBlocked (s box)
	(let ((x (car box)) (y (cadr box)))
		(cond
			((and (isWall (get-square s (- x 1) y)) (isWall (get-square s x (- y 1)))) t)
			((and (isWall (get-square s (+ x 1) y)) (isWall (get-square s x (- y 1)))) t)
			((and (isWall (get-square s (- x 1) y)) (isWall (get-square s x (+ y 1)))) t)
			((and (isWall (get-square s (+ x 1) y)) (isWall (get-square s x (+ y 1)))) t)
		)
	)
)

;boxBlocked
;if any box is blocked in a list
(defun boxBlocked (s box-list)
	(cond
		((null box-list) 0)
		(t (if (isBoxBlocked s (car box-list))
			1000 
			(boxBlocked s (cdr box-list))
			)
		)
	)

)
; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
(defun h105711853 (s)
	(let ((k-pos (findKeeperPosition s 0 0)) (box-list (cleanUpList(findBoxPositions s 0 0))) (goal-list (cleanUpList (findGoalPositions s 0 0))))
		(+ (sum-of-opt-goal-L1 box-list goal-list) (sum-of-list (L1 k-pos box-list)) (boxBlocked s box-list))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
