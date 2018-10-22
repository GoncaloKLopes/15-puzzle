(in-package :user)
(defconstant +goal-state+ (make-array '(4 4)
							:initial-contents '((1 2 3 4)
							(5 6 7 8)
							(9 10 11 12)
							(13 14 15 nil))))


(defun get-position (state)
	"Get the empty position.
	 Arguments:
	 * state -- A state.
	 Return:
	 * A pair corresponding to the position of the player."
	(let ((n-rows (car (array-dimensions state)))
		  (n-columns (car (cdr (array-dimensions state)))))
		(dotimes (i n-rows) ;go through each position (i, j) to check if it's nil
			(dotimes (j n-columns)
				(if (null (aref state i j))
					(return-from get-position (cons i j)))))))

(defun copy-array (array)
	"Copy an array.
	 Arguments:
	 * array -- The array to be copied.
	 Return:
	 * A copy of array."
	 (let ((new-array (make-array (array-dimensions array))))
	 	(dotimes (i (car (array-dimensions array)))
	 			(dotimes (j (car (cdr (array-dimensions array))))
	 				(setf (aref new-array i j) (aref array i j))))
	 	new-array))

(defun goalp (state)
	"Check if state is the goal state.
	 Arguments:
	 * state -- The state being tested.
	 Return:
	 * T if it is, NIL otherwise."
	(equalp state +goal-state+))


(defun operator-move-up (state)
	"Apply the move-up operator.
	 Arguments:
	 * state -- The state being changed.
	 Return:
	 * The new resulting state or state if already at the top row."
	 (let ((row (car (get-position state)))
	 	   (col (cdr (get-position state))))
	 	(if (equalp row 0) ;if top row
			state 
			(let ((result (copy-array state))) ;new var, don't change original state
			(progn
				(setf (aref result row col) (aref state (- row 1) col))
				(setf (aref result (- row 1) col) nil)
				result)))))

(defun operator-move-down (state)
	"Apply the move-down operator.
	 Arguments:
	 * state -- The state being changed.
	 Return:
	 * The new resulting state or state if already at the bottom row."
	 (let ((row (car (get-position state)))
	 	   (col (cdr (get-position state))))
	 	(if (equalp row (- (car (cdr (array-dimensions state))) 1)) ;if bottom row
			state 
			(let ((result (copy-array state))) ;new var, don't change original state
			(progn
				(setf (aref result row col) (aref state (+ row 1) col))
				(setf (aref result (+ row 1) col) nil)
				result)))))

(defun operator-move-left (state)
	"Apply the move-left operator.
	 Arguments:
	 * state -- The state being changed.
	 Return:
	 * The new resulting state or state if already at left-est column."
	 (let ((row (car (get-position state)))
	 	   (col (cdr (get-position state))))
	 	(if (equalp col 0) ;if left-est column
			state 
			(let ((result (copy-array state))) ;new var, don't change original state
			(progn
				(setf (aref result row col) (aref state row (- col 1)))
				(setf (aref result row (- col 1)) nil)
				result)))))

(defun operator-move-right (state)
	"Apply the move-right operator.
	 Arguments:
	 * state -- The state being changed.
	 Return:
	 * The new resulting state or state if already at right-est column."
	 (let ((row (car (get-position state)))
	 	   (col (cdr (get-position state))))
	 	(if (equalp col (- (car (cdr (array-dimensions state))) 1)) ;if right-est column
			state 
			(let ((result (copy-array state))) ;new var, don't change original state
			(progn
				(setf (aref result row col) (aref state row (+ col 1)))
				(setf (aref result row (+ col 1)) nil)
				result)))))

(defun n-mismatched-tiles-h (state)
	"Heuristic that reflects the number of mismatched tiles when compared to the goal state.
	 Arguments:
	 * state -- The state being evaluated.
	 Return:
	 * The number of mismatched tiles."
	(let ((count 0)
		  (n-rows (car (array-dimensions state)))
		  (n-columns (car (cdr (array-dimensions state)))))
		(dotimes (i n-rows) ;go through each position (i, j) to check if it's nil
			(dotimes (j n-columns)
				(if (not (equalp (aref state i j) (aref +goal-state+ i j)))
					(setf count (1+ count)))))
		count))