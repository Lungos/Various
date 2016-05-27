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
(defun 3-vowels-p (string)
  (let ((i 0))
    (map 'string #'(lambda (char)
		     (when (find char "aeiou" :test #'equal)
		       (incf i))
		     char)
	 string)
    (>= i 3)))

(defun same-twice-p (string)
  (let ((b nil))
    (with-input-from-string (stream string)
			    (loop until (null (peek-char nil stream nil 'nil)) do
				  (when (equal (read-char stream)
					       (peek-char nil stream nil))
				    (setf b t))))
    b))

(defun not-allowed-p (string)
  (not (or (search "ab" string)
	   (search "cd" string)
	   (search "pq" string)
	   (search "xy" string))))

(let ((total 0))
  (with-open-file (stream "input5.txt")
		  (loop for line = (read-line stream nil)
			until (null line) do
			(when (and (3-vowels-p line)
				   (same-twice-p line)
				   (not-allowed-p line))
			  (incf total))))
  (format t "Day 5 A: ~a~%" total))

;; B

(defun part (pair line)
  (cond ((search pair (subseq line 1))  t)
	((> (length line) 3) (part (subseq line 0 2) (subseq line 1)))
	(t nil)))

(defun one-between (string)
  (loop for i
	when (equal (char string i) (char string (+ 2 i)))
	return t
	when (eq i (- (length string) 3))
	return nil))

(let ((total 0))
  (with-open-file (stream "input5.txt")
		  (loop for line = (read-line stream nil)
			until (null line)
			do
			(when (and (part (subseq line 0 2) (subseq line 1))
				   (one-between line))
			  (incf total))))
  
  (format t "Day 5 B: ~a~%" total))
  
  ;; Day 6 A
  
  (defvar lights (make-array '(1000 1000)))
(defconstant through 0)

(defun turn-off (x1 y1 tt x2 y2)
  (loop for x from x1 upto x2 do
	(loop for y from y1 upto y2 do
	      (setf (aref lights x y) nil))))

(defun turn-on (x1 y1 tt x2 y2)
  (loop for x from x1 upto x2 do
	(loop for y from y1 upto y2 do
	      (setf (aref lights x y) t))))

(defun togg-e (x1 y1 tt x2 y2)
  (loop for x from x1 upto x2 do
	(loop for y from y1 upto y2 do
	      (if (aref lights x y) 
		  (setf (aref lights x y) nil)
		(setf (aref lights x y) t)))))

(with-open-file (stream "input6.txt")
		(loop for line = (read-line stream nil)
		      until (null line)
		      do
		      (setf (char line 4) #\-)
		      (eval (read-from-string
			     (concatenate 'string "("
					  (substitute #\Space #\, line)
					  ")")))))


(format t "Day 6 A: ~a~%" (loop for x below 1000
			        sum (loop for y below 1000 sum
					  (if (aref lights x y)
					      1
					    0))))

;; B

(defvar lights (make-array '(1000 1000) :initial-element 0))
(defconstant through 0)

(defun turn-off (x1 y1 tt x2 y2)
  (loop for x from x1 upto x2 do
	(loop for y from y1 upto y2 do
	      (unless (eq 0 (aref lights x y))
		(decf (aref lights x y))))))

(defun turn-on (x1 y1 tt x2 y2)
  (loop for x from x1 upto x2 do
	(loop for y from y1 upto y2 do
	      (incf (aref lights x y)))))

(defun togg-e (x1 y1 tt x2 y2)
  (loop for x from x1 upto x2 do
	(loop for y from y1 upto y2 do
	      (setf (aref lights x y) (+ 2 (aref lights x y))))))

(with-open-file (stream "input6.txt")
		(loop for line = (read-line stream nil)
		      until (null line)
		      do
		      (setf (char line 4) #\-)
		      (eval (read-from-string
			     (concatenate 'string "("
					  (substitute #\Space #\, line)
					  ")")))))


(format t "Day 6 B: ~a~%" (loop for x below 1000
			        sum (loop for y below 1000 sum
					  (aref lights x y))))

;; Day 7 A
(defconstant GB (make-hash-table :test #'equal))

(defun 16bit (x)
  (let ((bv (copy-seq #*0000000000000000)))
    (loop for i downfrom (length bv)
	  until (eq x 0) do
	  (when (eq (floor (/ x (expt 2 i))) 1)
	    (setf (bit bv i) 1)
	    (setf x (- x (expt 2 i)))))
    bv))

(defun 16dec (x)
  (loop for i across x
	for j  sum
	(* i (expt 2 j))))

(defun bitnot (a)
  (let ((x (val a)))
    (16dec (bit-not (16bit x)))))
    
(defun bitor (a b)
  (let ((x (val a))
	(y (val b)))
    (16dec (bit-ior (16bit x) (16bit y)))))

(defun bitand (a b)
  (let ((x (val a))
	(y (val b)))
    (16dec (bit-and (16bit x) (16bit y)))))

(defun bitlshift (a i)
  (let ((x (val a)))
    (ash x i)))

(defun bitrshift (a i)
  (let ((x (val a)))
    (ash x (- 0 i))))

(defun val (a)
  (cond ((numberp (gethash a GB)) (gethash a GB))
	((numberp a) a)
	(t (setf (gethash a GB) (eval (gethash a GB))))))

(with-open-file (stream "input7.txt")
		(loop for line = (read-line stream nil)
		      until (null line)
		      do	  
		      (let ((value (read-from-string (concatenate 'string "("
								  line
								  ")"))))
			(cond ((equal (car value) 'NOT) (setf (gethash (cadddr value) GB) `(bitnot (quote ,(cadr value)))))
			      ((equal (cadr value) 'OR) (setf (gethash (nth 4 value) GB) `(bitor (quote ,(car value)) (quote ,(caddr value)))))
			      ((equal (cadr value) 'AND) (setf (gethash (nth 4 value) GB) `(bitand (quote ,(car value)) (quote ,(caddr value)))))
			      ((equal (cadr value) 'LSHIFT) (setf (gethash (nth 4 value) GB) `(bitlshift (quote ,(car value)) ,(caddr value))))
			      ((equal (cadr value) 'RSHIFT) (setf (gethash (nth 4 value) GB) `(bitrshift (quote ,(car value)) ,(caddr value))))
			      (t (setf (gethash (caddr value) GB) (car value)))))))

(format t "Day 7 A: ~a~%" (val (gethash 'a GB)))

;; B

(with-open-file (stream "input7.txt")
		(loop for line = (read-line stream nil)
		      until (null line)
		      do
	  
		      (let ((value (read-from-string (concatenate 'string "("
								  line
								  ")"))))
			(cond ((equal (car value) 'NOT) (setf (gethash (cadddr value) GB) `(bitnot (quote ,(cadr value)))))
			      ((equal (cadr value) 'OR) (setf (gethash (nth 4 value) GB) `(bitor (quote ,(car value)) (quote ,(caddr value)))))
			      ((equal (cadr value) 'AND) (setf (gethash (nth 4 value) GB) `(bitand (quote ,(car value)) (quote ,(caddr value)))))
			      ((equal (cadr value) 'LSHIFT) (setf (gethash (nth 4 value) GB) `(bitlshift (quote ,(car value)) ,(caddr value))))
			      ((equal (cadr value) 'RSHIFT) (setf (gethash (nth 4 value) GB) `(bitrshift (quote ,(car value)) ,(caddr value))))
			      (t (setf (gethash (caddr value) GB) (car value)))))))

(setf (gethash 'b GB) 3176)
(format t "Day 7 B: ~a~%" (val (gethash 'a GB)))



;; Day 17 A

(setf total 0)


(defun run (list nbr sum)
  (if (> nbr 0)
      (loop for x in list
	    for pos upto (- (length list) nbr)
	    for l on list do
	    (run (nthcdr (1+ pos) list) (1- nbr) (+ x sum)))
    (when (eq sum 150)
      (incf total))))


(let ((input '(43 3 4 10 21 44 4 6 47 41 34 17 17 44 36 31 46 9 27 38)))
  (loop for i from 1 to (length input) do
	(run input i 0)))

(format t "Day 17 A: ~a~%" total)

;; B

(setf total 0)

(defun run (list nbr sum)
  (if (> nbr 0)
      (loop for x in list
	    for pos upto (- (length list) nbr)
	    for l on list do
	    (run (nthcdr (1+ pos) list) (1- nbr) (+ x sum)))
    (when (eq sum 150)
      (incf total))))


(let ((input '(43 3 4 10 21 44 4 6 47 41 34 17 17 44 36 31 46 9 27 38)))
  (loop for i from 1 to (length input)
	until (> total 0) do
	(run input i 0)))

(format t "Day 17 B: ~a~%" total)

;; Day 18 A

(defvar lights (make-array '(100 100) :initial-element 0))

(with-open-file (stream "input18.txt")
		(loop for line = (read-line stream nil)
		      for x
		      until (null line)
		      do
		      (loop for i across line
			    for y do
			    (when (equal i #\#)
			      (setf (aref lights x y) 1)))))

(loop for i below 100 do
      (let ((temp (make-array '(100 100) :initial-element 0)))
	(loop for x below 100 do
	      (loop for y below 100 do
		    (let ((val (loop for x1 from (1- x) upto (1+ x) sum
				     (loop for y1 from (1- y) upto (1+ y) sum
					   (if (or (< x1 0) (< y1 0)
						   (> x1 99) (> y1 99)
						   (and (eq x x1) (eq y y1)))
					       0
					     (aref lights x1 y1))))))
		      (if (eq 0 (aref lights x y))
			  (when (eq 3 val) (setf (aref temp x y) 1))
			(when (or (eq 3 val) (eq 2 val)) (setf (aref temp x y) 1))))))
	(setf lights temp))
      (print i))


(format t "Day 18 A: ~a~%" (loop for x below 100
			        sum (loop for y below 100 sum
					  (aref lights x y))))
					  
;; B

(defvar lights (make-array '(100 100 101) :initial-element 0))

(with-open-file (stream "input18.txt")
		(loop for line = (read-line stream nil)
		      for x
		      until (null line)
		      do
		      (loop for i across line
			    for y do
			    (if (or (and (eq x 0) (eq y 0))
				    (and (eq x 0) (eq y 99))
				    (and (eq x 99) (eq y 0))
				    (and (eq x 99) (eq y 99)))
				(setf (aref lights x y 0) 1)
			      (when (equal i #\#)
				(setf (aref lights x y 0) 1))))))

(loop for i below 100 do
	(loop for x below 100 do
	      (loop for y below 100 do
		    (if (or (and (eq x 0) (eq y 0))
			    (and (eq x 0) (eq y 99))
			    (and (eq x 99) (eq y 0))
			    (and (eq x 99) (eq y 99)))
			(setf (aref lights x y (1+ i)) 1)		      
		      (let ((val (loop for x1 from (1- x) upto (1+ x) sum
				       (loop for y1 from (1- y) upto (1+ y) sum
					     (if (or (< x1 0) (< y1 0)
						     (> x1 99) (> y1 99)
						     (and (eq x x1) (eq y y1)))
						 0
					       (aref lights x1 y1 i))))))
			(if (eq 0 (aref lights x y i))
			    (when (eq 3 val) (setf (aref lights x y (1+ i)) 1))
			  (when (or (eq 3 val) (eq 2 val)) (setf (aref lights x y (1+ i)) 1))))))))



(format t "Day 18 B: ~a~%" (loop for x below 100
			        sum (loop for y below 100 sum
					  (aref lights x y 100))))

;;
