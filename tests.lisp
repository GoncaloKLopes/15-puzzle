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

(setf a3 (make-array '(4 4)
			:initial-contents '((nil 2 3 4)
			(5 6 7 8)
			(9 10 11 12)
			(13 14 15 1))))

(setf a4 (make-array '(4 4)
			:initial-contents '((1 2 3 4)
			(5 6 7 8)
			(13 nil 10 11)
			(14 9 15 12))))

(setf a5 (make-array '(4 4)
			:initial-contents '((1 2 3 4)
			(5 6 7 8)
			(nil 13 10 11)
			(14 9 15 12))))

(setf a6 (make-array '(4 4)
			:initial-contents '((nil 15 14 13)
			(12 11 10 9)
			(8 7 6 5)
			(4 3 2 1))))

;;;get-position
(let ((res "")
	 (test (get-position a1)))
	(if (equalp test '(3 . 1))
		(setf res "passed")
		(setf res "failed"))
	(format t "get-position test ~A!~%" res))

;;;copy-array
(let ((res "")
	 (test (copy-array a1)))
	(if (and (equalp test a1) (not (eq test a1)))
		(setf res "passed")
		(setf res "failed"))
	(format t "copy-array45 test ~A!~%" res))
;;;goalp
(let ((res ""))
	(if (goalp a2)
		(setf res "passed")
		(setf res "failed"))
	(format t "goalp same test ~A!~%" res))

(let ((res ""))
	(if (goalp a1)
		(setf res "failed")
		(setf res "passed"))
	(format t "goalp different test ~A!~%" res))

;;operator-move-up
(let ((res "")
	  (test (operator-move-up a3)))
	(if (equalp test a3)
		(setf res "passed")
		(setf res "failed"))
	(format t "operator-move-up no move test ~A!~%" res))

(let ((res "")
	  (test (operator-move-up a1)))
	(if (equalp test a4)
		(setf res "passed")
		(setf res "failed"))
	(format t "operator-move-up yes move test ~A!~%" res))


;;operator-move-down
(let ((res "")
	  (test (operator-move-down a1)))
	(if (equalp test a1)
		(setf res "passed")
		(setf res "failed"))
	(format t "operator-move-down no move test ~A!~%" res))

(let ((res "")
	  (test (operator-move-down a4)))
	(if (equalp test a1)
		(setf res "passed")
		(setf res "failed"))
	(format t "operator-move-down yes move test ~A!~%" res))


;;operator-move-left
(let ((res "")
	  (test (operator-move-left a5)))
	(if (equalp test a5)
		(setf res "passed")
		(setf res "failed"))
	(format t "operator-move-left no move test ~A!~%" res))

(let ((res "")
	  (test (operator-move-left a4)))
	(if (equalp test a5)
		(setf res "passed")
		(setf res "failed"))
	(format t "operator-move-left yes move test ~A!~%" res))


;;operator-move-right
(let ((res "")
	  (test (operator-move-right a2)))
	(if (equalp test a2)
		(setf res "passed")
		(setf res "failed"))
	(format t "operator-move-right no move test ~A!~%" res))

(let ((res "")
	  (test (operator-move-right a5)))
	(if (equalp test a4)
		(setf res "passed")
		(setf res "failed"))
	(format t "operator-move-right yes move test ~A!~%" res))


;;n-mismatched-tiles-h
(let ((res "")
	  (test (n-mismatched-tiles-h a1)))
	(if (equalp test 7)
		(setf res "passed")
		(setf res "failed"))
	(format t "n-mismatched-tiles-h simple test ~A!~%" res))

(let ((res "")
	  (test (n-mismatched-tiles-h a2)))
	(if (equalp test 0)
		(setf res "passed")
		(setf res "failed"))
	(format t "n-mismatched-tiles-h zero test ~A!~%" res))

(let ((res "")
	  (test (n-mismatched-tiles-h a6)))
	(if (equalp test 16)
		(setf res "passed")
		(setf res "failed"))
	(format t "n-mismatched-tiles-h full wrong test ~A!~%" res))