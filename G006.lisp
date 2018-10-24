(in-package :user)

(defconstant *goal-state* (make-array '(4 4)
							:initial-contents '((1 2 3 4)
							(5 6 7 8)
							(9 10 11 12)
							(13 14 15 nil))))


(defun get-position (state)
	"Get the empty position.
	 Arguments:
	 * state -- An array representing the state.
	 Return:
	 * A pair corresponding to the position of the player."
	(let ((n-rows (car (array-dimensions state)))
		  (n-columns (car (cdr (array-dimensions state)))))
		(dotimes (i n-rows) ;go through each position (i, j) to check if it's nil
			(dotimes (j n-columns)
				(if (null (aref state i j))
					(return-from get-position (cons i j)))))))

(defun goalp (state)
	"Check if state is the goal state.
	 Arguments:
	 * state -- An array representing the state.
	 Return:
	 * T if it is, NIL otherwise."
	(equalp state *goal-state*))


(defun operator (state)
	"Apply up, down, left and right operators on the current state.
	 Arguments:
	 * state -- An array representing the state.
	 Return:
	 * A list of state arrays, corresponding to the result of applying each operator."
	(let ((result)
		  (row (car (get-position state)))
	 	  (col (cdr (get-position state)))
	 	  (n-rows (car (array-dimensions state)))
		  (n-columns (car (cdr (array-dimensions state)))))
		;each element of the list represents a move up, down, left or right
		(dolist (move '((1 . 0) (0 . 1) (-1 . 0) (0 . -1)))
			(let ((aux-state (copy-array state))
				  (new-row (+ row (car move)))
				  (new-col (+ col (cdr move))))
				(if (and (and (>= new-row 0) (< new-row n-rows))
						 (and (>= new-col 0) (< new-col n-columns)))
					(progn
						(setf (aref aux-state row col) (aref aux-state new-row new-col))
						(setf (aref aux-state new-row new-col) nil)
						(setf result (cons aux-state result))
					))))
		result))

(defun manhattan-block-distance-h (state)
	"Heuristic that reflects the sum of manhattan distances of each tile when compared to the goal state.
	 Arguments:
	 * state -- An array representing the state.
	 Return:
	 * The sum of manhattan distances of mismatched tiles."	
	(let ((n-rows (car (array-dimensions state)))
		  (n-columns (car (cdr (array-dimensions state))))
		  (sum 0))
		(flet ((manhattan-distance (pos1 pos2) ;pos1 and pos2 are pairs
				(+ (abs (- (car pos1) (car pos2))) 
				   (abs (- (cdr pos1) (cdr pos2)))))
			   (correct-pos (piece-index)
				(if (null piece-index)
					(cons (1- n-rows) (1- n-columns))
					(cons (floor (1- piece-index) n-rows) (mod (1- piece-index) n-columns)))))
		(dotimes (i n-rows)
			(dotimes (j n-columns)
				(setf sum (+ sum (manhattan-distance (cons i j) (correct-pos (aref state i j))))))))
		sum))

(defun solve-problem (state strategy)
	"Solve a problem using a given strategy and an initial state.
	 Arguments:
	 * state -- An array representing the state.
	 * strategy -- a string that indicates the strategy to use.
	 Return:
	 * A list of states from the initial state to the goal state."
	(let ((result (procura (cria-problema state 
							(list #'operator)
							:estado-final *goal-state*
							:objectivo? #'goalp
							:custo nil
							:heuristica #'manhattan-block-distance-h
							:estado= #'equalp)
						strategy
		 				:espaco-em-arvore? T)))

		(car result))) 


