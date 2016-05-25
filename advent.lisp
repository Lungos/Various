;;day 1 A

(print (with-open-file (stream "input1.txt")
		       (loop for char = (read-char stream nil)
			     until (null char)
			sum (case char
				  ((#\() 1)
				  ((#\)) -1)
				  (t 0)))))
		     
;; B

(let ((i 0)
      (floor 0))
  (with-open-file (stream "input1.txt")
		  (loop for char = (read-char stream nil)
			until (eq floor -1)
			do
			(incf i)
			(case char
			      ((#\() (incf floor))
			      ((#\)) (decf floor))
			      (t 0))))
  (print i))

;;day 2 A

(let ((total 0))
  (with-open-file (stream "input2.txt")
		  (loop for line = (read-line stream nil)
			until (null line) do
			(let* ((value (read-from-string
				       (concatenate 'string "("
						    (substitute #\Space #\x line)
						    ")")))
			       (l (car value))
			       (w (cadr value))
			       (h (caddr value)))
			  (setf total (+ (eval (list 'min (* l w) (* l h) (* w h)))
					 (* 2 l h)
					 (* 2 l w)
					 (* 2 w h)
					 total)))))
  (print total))

;; B

(let ((total 0))
  (with-open-file (stream "input2.txt")
		  (loop for line = (read-line stream nil)
			until (null line) do
			(let* ((value (read-from-string
				       (concatenate 'string "("
						    (substitute #\Space #\x line)
						    ")")))
			       (l (car value))
			       (w (cadr value))
			       (h (caddr value)))
			  (setf total (+ (eval (list 'min (* 2 (+ l w)) (* 2 (+ l h)) (* 2 (+ w h))))
					 (* w l h)
					 total)))))
  (print total))

;; Day 3 A

(let ((total 1)
      (db (make-hash-table :test #'equal))
      (x 0)
      (y 0))
  (setf (gethash (cons x y) db) t)
  (with-open-file (stream "input3.txt")
		  (loop for char = (read-char stream nil)
			until (null char) do
			(case char
			      ((#\^) (incf y))
			      ((#\v) (decf y))
			      ((#\<) (decf x))
			      ((#\>) (incf x))
			      (t 0) (print char))
			(unless (gethash (cons x y) db)
			  (progn (setf (gethash (cons x y) db) t)
				 (incf total)))))
  (print total))

;; B

(let ((total 1)
      (db (make-hash-table :test #'equal))
      (x (list 0 0))
      (y (list 0 0)))
  (setf (gethash (cons 0 0) db) t)
  (with-open-file (stream "input3.txt")
		  (loop for char = (read-char stream nil)
			for i
			until (null char) do
			(case char
			      ((#\^) (incf (nth (mod i 2) y)))
			      ((#\v) (decf (nth (mod i 2) y)))
			      ((#\<) (decf (nth (mod i 2) x)))
			      ((#\>) (incf (nth (mod i 2) x)))
			      (t 0) (print char))
			(unless (gethash (cons (nth (mod i 2) x) (nth (mod i 2) y)) db)
			  (progn (setf (gethash (cons (nth (mod i 2) x) (nth (mod i 2) y)) db) t)
				 (incf total)))))
  (print total))

;; Day 4

;;(need to read up on md5)

;; Day 5 A
