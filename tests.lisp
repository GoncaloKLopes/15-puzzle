(in-package :user)

(setf a1 (make-array '(4 4)
			:initial-contents '((1 2 3 4)
			(5 6 7 8)
			(13 9 10 11)
			(14 nil 15 12))))

;;same as +goal-state+
(setf a2 (make-array '(4 4)
			:initial-contents '((1 2 3 4)
			(5 6 7 8)
			(9 10 11 12)
			(13 14 15 nil))))

;;;goalp
(let((res ""))
	(if (goalp a2)
		(setf res "passed")
		(setf res "failed"))
	(format t "goalp same test ~A!~%" res))

(let((res ""))
	(if (goalp a1)
		(setf res "failed")
		(setf res "passed"))
	(format t "goalp different test ~A!~%" res))

