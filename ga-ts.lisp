;;Travel to everynode and return to the start
(load "svg.lisp") ;;made by Jeremiah LaRocco, jl2 on github

(defparameter *nodes* nil)
(defparameter *popSize* 50)
(defconstant *points* 200)
(defparameter *MaxX* 500)
(defparameter *MaxY* 500)
(defparameter *distances* (make-hash-table :test #'equal))
(defparameter *population* nil)
(setf *random-state* (make-random-state t))

;;;create a population of individuals
(defstruct individual fitness gene (reeval nil))

(defun gene-values (nbr)
  (loop for i
	from 1
	to nbr
	collect i))

(defun return2 (a b lis)
  (list (nth a lis) (nth b lis)))
  
(defun rng-gene (nbr)
  (let ((value (gene-values nbr)))
    (labels ((generate (lis)
		       (unless (null lis)
			 (let ((x  (nth (random (length lis)) lis)))
			   (cons x (generate (remove x lis)))))))
	    (generate value))))

(defun rng-ind (nbr)
  (let ((i individual))
    (setf (individual-gene i) (rng-gene nbr))
    (setf (individual-fitness i) (fitness (individual-gene i)))
    i))

;;create an array with nbr random points with max value x and y
(defun nodes (nbr x y)
  (setf *nodes* (make-array nbr))
  (nodes2 nbr x y))

(defun nodes2 (nbr x y)
  (let ((n (rng-node x y)))
    (if (> nbr 0)
	(if (find n *nodes* :test #'equal)
	    (nodes2 nbr x y)
	  (progn (setf (aref *nodes* (1- nbr)) n)
		 (nodes2 (1- nbr) x y))))))

(defun rng-node (x y)
  (cons (random x) (random y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;make sure a and b are ordered always same way
(defun dist-nodes (a b)
  (let ((val (gethash (list a b) *distances*)))
    (if val
	val
      (setf  (gethash (list a b) *distances*) (calc-dist a b)))))

(defun calc-dist (a b)
  (let ((x (- (car a) (car b)))
	(y (- (cdr a) (cdr b))))
    (sqrt (+ (* x x)
	     (* y y)))))

(defun dist-nbr (a b)
  (if (< a b)
      (dist-nodes (aref *nodes* (1- a)) (aref *nodes* (1- b)))
    (dist-nodes (aref *nodes* (1- b)) (aref *nodes* (1- a)))))

(defun fitness (gene)
  (loop for i below (length gene)
	sum (dist-nbr (aref gene i) (aref gene (mod (1+ i) (length gene))))))
		
(defun gen-ind (gene-length)
  (let* ((gene (rng-gene gene-length))
	 (x (make-individual :gene (make-array gene-length :initial-contents gene))))
    (setf (individual-fitness x) (fitness (individual-gene x)))
    x))
	

(defun gen-population (popSize gene-length)
  (setf *population* (sort (loop repeat popSize
				 collect (gen-ind gene-length))
			   #'< :key #'individual-fitness)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;reproduce

;; 100k runs give 48.5 per 100
(defun reprop (rank popSize)
  (< rank (random popSize)))

(defun reproduction (popSize)
  (labels ((f (rank pop)
	      (unless (null pop)
		(when (reprop rank popSize)
		  (push (make-individual :gene (reproduce (car pop)) :reeval t) *population*))
		(f (1+ rank) (cdr pop)))))
	  (f 1 *population*)))

(defun reproduce (ind)
  (let* ((father-gene (individual-gene ind))
	 (size (length father-gene))
	 (r1 (random size))
	 (r2 (labels ((f (n)
			 (if (eq n r1)
			     (f (random size))
			   n)))
		     (f (random size)))))
    (cutlist father-gene r1 r2 size)))
;    (setf (individual-gene x) gene)))

(defun cutlist (father-gene r1 r2 size)
  (let ((gene (make-array size))
	(swap nil))
    (loop for i below size do
	  (cond ((< i r1) (setf (aref gene i) (aref father-gene i)))
		((> i r2) (setf (aref gene i) (aref father-gene i)))
		(t (push (aref father-gene i) swap))))
    (loop for i in swap
	  for j to (- r2 r1) do
	  (setf (aref gene (+ j r1)) i))
    gene))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;mutate

(defun mutate (r gene-length)
  (labels ((f (pop)
	      (if (equal (individual-fitness (car pop)) nil)
		  (f (cdr pop))
		(cdr pop))))
	  (let ((mutate-pop (f *population*)))
	    (labels ((mu (pop)
			 (unless (null pop)
			   (let ((ind (car pop)))
			     (when (< (/ (random 100) 100.0) r) ;average 0.495
			       (let* ((r1 (random gene-length))
				      (r2 (labels ((t (n)
						     (if (eq n r1)
							 (t (random gene-length))
						       n)))
						  (t (random gene-length))))
				      (temp (aref (individual-gene ind) r1)))
				 (setf (aref (individual-gene ind) r1) (aref (individual-gene ind) r2))
				 (setf (aref (individual-gene ind) r2) temp)
				 (setf (individual-reeval ind) t))))			     
			   (mu (cdr pop)))))
		    (mu mutate-pop)))))

(defun evaluate ()
  (labels ((f (pop)
	      (unless (null pop)
		(let ((ind (car pop)))
		  (when (individual-reeval ind)
		    (setf (individual-fitness ind) (fitness (individual-gene ind)))
		    (setf (individual-reeval ind) nil)))
		(f (cdr pop)))))
	  (f *population*))
  (setf *population* (sort *population* #'< :key #'individual-fitness)))

(defun death (popSize)
  (let ((end (+ 1 (floor (/ popSize 25))))
	(leng (length *population*)))
    (setf *population* (append (subseq *population* 0 (- popSize end)) (subseq *population* (- leng end) leng)))))

(defparameter *best* 0)
(defparameter *bestlist* nil)

(defun main (popSize gene-length xMax yMax)
  (nodes gene-length xMax yMax)
;  (circle-nodes gene-length xMax yMax)
  (gen-population popSize gene-length)
  (let ((m 0))
    (loop for i
	  do
	  (reproduction popSize)
	  (mutate 0.04 gene-length)
	  (evaluate)
	  (death popSize)
	  (when (and (> (- i m) 200) (not (eq 0 (floor (- (individual-fitness (car *population*)) *best*)))))
	    (setf *best* (individual-fitness (car *population*)))
	    (format t "i-m=~a and i=~a best=~a~C" (- i m) i *best* #\newline)
	    (setf m i)
	    (results))
	  (when (eq (mod i 5) 0)
	    (push (individual-fitness (car *population*)) *bestlist*))
	  until (< (+ 1000 m) i))))

(defun add-line-list (img p1 p2)
  (add-line img (floor (car p1)) (floor (cdr p1)) (floor (car p2)) (floor (cdr p2))))

(defun add-line-gene (img a b)
  (add-line-list img (aref *nodes* (1- a)) (aref *nodes* (1- b))))

(defun add-point-list (img list x y)
  (add-point img (+ x (floor (car list))) (+ y (floor (cdr list)))))

(defun add-point-chift (img x y r d)
  (add-point img (+ x r) (+ y d)))

(defun results ()
  (setf bestlist (reverse *bestlist*))
  (let ((img (make-svg))
	(worst (car bestlist))
	(gene (individual-gene (car *population*))))
;    (print 1)
    (loop for y in bestlist
	  for x do
	  (add-point-chift img x (* (/ 500 worst) y) 0 500))
 ;   (print 2)
    (loop for p across *nodes* do
	  (add-point-list img p 0 0))
 ;   (print 3)
    (loop for i below (length gene) do
	  (add-line-gene img (aref gene i) (aref gene (mod (1+ i) (length gene)))))
    
    (with-open-file (stream "results.svg" :direction :output :if-exists :supersede)
		    (to_file img :stream stream))))
    

(defun circle-nodes (gene-length x y)
  (setf *nodes* (make-array gene-length))
  (let* ((mi 5)
	 (ma (- (min x y) 5))
	 (a (/ (- ma mi) (1- (ash gene-length -1)))))
    (loop for i below gene-length by 2 do
	  (let* ((x1 (+ (* a (ash i -1)) mi))
		 (r (ash (min x y) -1))
		 (y1 (+ r (sqrt (- (* r r) (* (- x1 r) (- x1 r))))))
		 (y2 (- r (sqrt (- (* r r) (* (- x1 r) (- x1 r)))))))
	    (setf (aref *nodes* i) (cons x1 y1))
	    (setf (aref *nodes* (1+ i)) (cons x1 y2))))))


(print (time (main *popSize* *points* *MaxX* *MaxY*)))
(results)
