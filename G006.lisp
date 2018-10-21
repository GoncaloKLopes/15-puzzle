(in-package :user)
(defconstant +goal-state+ (make-array '(4 4)
							:initial-contents '((1 2 3 4)
							(5 6 7 8)
							(9 10 11 12)
							(13 14 15 nil))))

;;Check if state is the goal state.
;;
;;Arguments:
;;
;;state -- The state being tested.
;;
;;Return:
;;T if it is, NIL otherwise.
(defun goalp (state)
	(equalp state +goal-state+))

