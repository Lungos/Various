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

;; Day 8 A

(format t "Day 8 A: ~a" (with-open-file (stream "input8.txt")
					(loop for line = (read-line stream nil)
					      until (null line) sum
					      (- (length line)
						 (loop for i until (>= i (length line)) sum
						       (case (aref line i)
							     (#\" 0)
							     (#\\ (case (aref line (1+ i))
									(#\" (incf i) 1)
									(#\\ (incf i) 1)
									(#\x (incf i 3) 1)))
							     (t 1)))))))
							     
;; Day 8 B

(format t "Day 8 B: ~a" (with-open-file (stream "input8.txt")
					(loop for line = (read-line stream nil)
					      until (null line) sum
					      (- (+ (loop for i until (>= i (length line)) sum
							  (case (aref line i)
								(#\" 2)
								(#\\ 2)
								(t 1)))
						    2)
						 (length line)))))

;; Day 9 A

(defconstant *distances* (make-hash-table :test #'equal))
(setf AlphaCentauri 1)
(setf Snowdin 2)
(setf Tambi 3)
(setf Faerun 4)
(setf Norrath 5)
(setf Straylight 6)
(setf tristram 7)
(setf arbre 8)
(defstruct indiv fitness gene)
(setf *random-state* (make-random-state t))

(with-open-file (stream "input9.txt")
		(loop for line = (read-line stream nil)
		      until (null line) do
		      (let* ((value (read-from-string (concatenate 'string "(" line ")")))
			    (x (eval (car value)))
			    (y (eval (caddr value))))
			(setf (gethash (cons x y) *distances*) (nth 4 value))
			(setf (gethash (cons y x) *distances*) (nth 4 value)))))



(format t "Day 9 A: ~a" (let ((population nil)
			      (struct '(1 2 3 4 5 6 8 7 8)))
			  (labels ((fitness (gene)
					    (loop for i below (1- (length gene)) sum
						  (gethash (cons (nth i gene) (nth (1+ i) gene)) *distances*)))
				   (rng-gene (list)
					     (unless (null list)
					       (let ((x (nth (random (length list)) list)))
						 (cons x (rng-gene (remove x list)))))))
				  (loop repeat 60 do
					(let ((ind (make-indiv :gene (rng-gene struct))))
					  (setf (indiv-fitness ind) (fitness (indiv-gene ind)))
					  (push ind population)))
				  (setf population (subseq (sort population #'< :key #'indiv-fitness) 0 30))
				  (setf children nil)
				  (loop repeat 50 do
					(loop for i in population repeat 15 do
					      (let ((child (make-indiv :gene
								       (let* ((r1 (max (- (random (length struct)) 2) 0))
									      (r2 (+ r1 (random (- (length struct) r1)))))
									 (append (subseq (indiv-gene i) 0 r1)
										 (reverse (subseq (indiv-gene i) r1 r2))
										 (subseq (indiv-gene i) r2))))))
						(setf (indiv-fitness child) (fitness (indiv-gene child)))
						(push child children)))
					(setf population (subseq (sort (append population children) #'< :key #'indiv-fitness) 0 30)))
				  (indiv-fitness (car population)))))

;; Day 9 B

;; Same as A just sort for largest first instead of smallest first

;; Day 10 A

(defun elves-look (code nbr)
  (if (eq nbr 0)
      code
    (let ((val nil)
	  (i 0)
	  (next ""))
      (labels ((write-next (ch nbr) (concatenate 'string next (write-to-string nbr) (string ch))))
	      (setf val (aref code 0))
	      (loop for ch across code do
		    (if (equal ch val)
			(incf i)
		      (progn (setf next (write-next val i))
			     (setf i 1)
			     (setf val ch))))
	      (setf next (write-next val i)))
      (elves-look next (1- nbr)))))

(format t "Day 10 A: ~a" (length (elves-look (write-to-string 3113322113) 40)))

;; Day 11 A

(defun 3-incremental-p (string)
  (let ((next 0)
	(i 0))
    (loop for ch across string do
	  (if (eq next (char-code ch))
	      (progn (incf i)
		     (incf next))
	    (progn (setf i 0)
		   (setf next (1+ (char-code ch)))))
	 (when (eq i 2) (return t)))))

(defun same-twice-p (string)
  (let ((nbr 0)
	(pre-ch nil))
    (loop for ch across string
	  for i do
	  (if (equal pre-ch ch)
	      (progn (incf nbr)
		     (setf pre-ch nil))
	    (setf pre-ch ch)))
    (if (> nbr 1)
	t
      nil)))

(defun not-allowed-p (string)
  (not (or (find #\i string :test #'equalp)
	   (find #\o string :test #'equalp)
	   (find #\l string :test #'equalp))))


(format t "Day 11 A: ~a~%" (let ((string (copy-seq "hxbxwxba")))
			     (loop until (and (3-incremental-p string)
					      (same-twice-p string)
					      (not-allowed-p string))
				   do
				   (loop for i downfrom (1- (length string)) do
					 (if (eq 122 (char-code (aref string i)))
					     (setf (aref string i) #\a)
					   (setf (aref string i) (code-char (1+ (char-code (aref string i))))))
					 until (not (equal (aref string i) #\a))))
			     string))

;; Day 11 B

;; Same as A just change the string to be the next one after As result ie "hxbxxzaa"

;; Day 12 A

(format t "Day 12 A: ~a~%" (with-open-file (stream "input12.txt")
					   (loop for line = (read-line stream nil)
						 until (null line) sum
						 (let* ((value (read-from-string
								(concatenate 'string "("
									     (substitute-if #\Space #'(lambda (x)
													(or (equal x #\{)
													    (equal x #\})
													    (equal x #\[)
													    (equal x #\])
													    (equal x #\,)
													    (equal x #\:)))
											    line)
									     ")"))))
						   (loop for x in value sum
							 (if (numberp x)
							     x
							   0))))))
							   
;; Day 13 A

;;Very similar to 9, same as B but without the 0 in struct.

;; Day 13 B

(defconstant happiness (make-hash-table :test #'equal))
(setf Alice 1)
(setf Bob 2)
(setf Carol 3)
(setf David 4)
(setf Eric 5)
(setf Frank 6)
(setf George 7)
(setf Mallory 8)
(defstruct indiv fitness gene)
(setf *random-state* (make-random-state t))

(defun lose (x) (- x))
(defun gain (x) x)


(with-open-file (stream "input13.txt")
		(loop for line = (read-line stream nil)
		      until (null line) do
		      (let* ((value (read-from-string (concatenate 'string "(" (remove #\. line) ")")))
			     (x (eval (car value)))
			     (y (eval (nth 10 value)))
			     (z (eval (list (nth 2 value) (nth 3 value)))))
			(setf (gethash (cons x y) happiness) z))))

(loop for i from 1 to 8 do
      (setf (gethash (cons 0 i) happiness) 0)
      (setf (gethash (cons i 0) happiness) 0))


(format t "Day 13 A: ~a" (let ((population nil)
			      (struct '(0 1 2 3 4 5 6 8 7 8)))
			  (labels ((fitness (gene)
					    (loop for i below (length gene) sum
						  (+ (gethash (cons (nth i gene) (nth (mod (1+ i) (length gene)) gene)) happiness)
						     (gethash (cons (nth (mod (1+ i) (length gene)) gene) (nth i gene)) happiness))))
				   (rng-gene (list)
					     (unless (null list)
					       (let ((x (nth (random (length list)) list)))
						 (cons x (rng-gene (remove x list)))))))
				  (loop repeat 60 do
					(let ((ind (make-indiv :gene (rng-gene struct))))
					  (setf (indiv-fitness ind) (fitness (indiv-gene ind)))
					  (push ind population)))
				  (setf population (subseq (sort population #'> :key #'indiv-fitness) 0 30))
				  (setf children nil)
				  (loop repeat 50 do
					(loop for i in population repeat 15 do
					      (let ((child (make-indiv :gene
								       (let* ((r1 (max (- (random (length struct)) 2) 0))
									      (r2 (+ r1 (random (- (length struct) r1)))))
									 (append (subseq (indiv-gene i) 0 r1)
										 (reverse (subseq (indiv-gene i) r1 r2))
										 (subseq (indiv-gene i) r2))))))
						(setf (indiv-fitness child) (fitness (indiv-gene child)))
						(push child children)))
					(setf population (subseq (sort (append population children) #'> :key #'indiv-fitness) 0 30)))
				  (indiv-fitness (car population)))))
				  

;; Day 14 A
;; The code works for the examples but claims the answear i give is for someone ells, not me
(format t "Day 14 A: ~a" (let ((reindeers nil))
			   (with-open-file (stream "input14.txt")
					   (loop for line = (read-line stream nil)
						 until (null line) do
						 (let ((value (read-from-string (concatenate 'string "(" (remove #\, line) ")"))))
						   (push (list (car value) (cadddr value) (nth 6 value) (nth 13 value)) reindeers))))
			   (eval (cons 'max (mapcar #'(lambda (x) (+ (* (cadr x) (caddr x) (floor (/ 2503 (+ (caddr x) (cadddr x)))))
								     (* (caddr x) (min (cadr x) (mod (+ (caddr x) (cadddr x)) 2503)))))
						    reindeers)))))
						    
;; Day 15 A

;; structure more similar to 13 but with fitness more like 15B, but without counting calories
;; Probably time to turn it into a macro

;; Day 15 B

(defstruct indiv fitness gene)
(setf *random-state* (make-random-state t))


(defconstant ingredients (make-array 5))
(with-open-file (stream "input15.txt")
		(loop for line = (read-line stream nil)
		      for i
		      until (null line) do
		      (let* ((value (read-from-string (concatenate 'string "(" (remove-if #'(lambda (x) (or (equal x #\,)
													    (equal x #\:)))
											  line) ")"))))
			(loop for i below (length ingredients) do
			      (push (nth (+ 2 (* 2 i)) value) (aref ingredients i))))))

(format t "~%Day 15 B: ~a" #.(cons 'max (loop repeat 5 collect
						  (let ((population nil))
						    (labels ((fitness (gene)
								      (let* ((a 0)
									     (s (loop for i in gene sum i))
									     (g (loop for x in gene
										      for i collect
										      (if (eq i 3)
											  (- 100 a)
											(progn (incf a (floor (* 100 (/ x s))))
											       (floor (* 100 (/ x s)))))))
									     (c (loop for i in g
										      for x in (aref ingredients 4) sum
										      (* i x))))
									
									(/ (eval (cons '* (loop for i across ingredients
												repeat 4 collect
												(loop for x in g
												      for y in i sum
												      (* x y)))))
									   (expt 2 (abs (- 500 c)))))))
							    (loop repeat 60 do
								  (let ((ind (make-indiv :gene (loop repeat 4 collect (random 100)))))
								    (setf (indiv-fitness ind) (fitness (indiv-gene ind)))
								    (push ind population)))
							    (setf population (subseq (sort population #'> :key #'indiv-fitness) 0 50))
							    (setf children nil)
							    (let ((best 0)
								  (times 0))
							      (loop for i until (> i (+ times 200)) do
								    (loop for i in population repeat 30 do
									  (let* ((mother (nth (random (length population)) population))
										 (r (/ (random 100) 100))
										 (child (make-indiv :gene
												    (mapcar #'(lambda (x y) (floor (+ (* r x)
																      (* (- 1 r) y))))
													    (indiv-gene i)
													    (indiv-gene mother)))))
									    (setf (indiv-fitness child) (fitness (indiv-gene child)))
									    (push child children)))
								    (setf population (subseq (sort (append population children) #'> :key #'indiv-fitness) 0 30))
								    (setf children nil)
								    (loop for i in population
									  for k do
									  (when (and (> 5 (random 100)) (< 0 k))
									    (setf (nth (random 4) (indiv-gene i)) (random 100))
									    (setf (indiv-fitness i) (fitness (indiv-gene i)))))
								    (when (> (indiv-fitness (car population)) best)
								      (setf best (indiv-fitness (car population)))
								      (setf times i)))))
						    
						    
						    (indiv-fitness (car population))))))

;; Day 16 A

;;same as B, just with all functions using eq

;; Day 16 B

(defun children (x) (eq x 3))
(defun cats (x) (> x 7))
(defun samoyeds (x) (eq x 2))
(defun pomeranians (x) (< x 3))
(defun akitas (x) (eq x 0))
(defun vizslas (x) (eq x 0))
(defun goldfish (x) (< x 5))
(defun trees (x) (> x 3))
(defun cars (x) (eq x 2))
(defun perfumes (x) (eq x 1))

(format t "Day 16 B: ~a~%" (with-open-file (stream "input16.txt")
					   (loop for line = (read-line stream nil)
						 until (null line) do
						 (let* ((value (read-from-string
								(concatenate 'string "("
									     (substitute #\Space #\, (substitute #\Space #\: line))
									     ")"))))
						   (when (and (eval (list (nth 2 value) (nth 3 value)))
							      (eval (list (nth 4 value) (nth 5 value)))
							      (eval (list (nth 6 value) (nth 7 value))))
						     (return (cadr value)))))))

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

;; Day 19 A

(defconstant db (make-hash-table :test #'equal))

(let ((dict nil)
      (input nil)
      (total 0))
  (with-open-file (stream "input19.txt")
		  (loop for line = (read-line stream nil)
			for i
			until (null line) do
			(when (and (> (length line) 6) (< (length line) 20))
			  (let ((value `(,(remove #\Space (subseq line 0 2)) ,(remove #\Space (subseq line 5)))))
			    (push (cons (string (car value)) (string (cadr value))) dict)))
			(when (> (length line) 20)
			  (setf input line))))

  (labels ((replace (code start)
		    (let ((temp (search (car code) input :start2 start)))

		      (if temp
			  (let ((str (concatenate 'string (subseq input 0 temp) (cdr code) (subseq input (+ temp (length (car code)))))))

			    (unless (gethash str db)
			      (incf total)
			      (setf (gethash str db) t))
			    (replace code (1+ temp)))
			nil))))
			  
	  (loop for i in dict do
		(replace i 0)))
		        
(format t "~%Day 19 A: ~a" total))
