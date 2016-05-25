
(load "svg.lisp") ;;made by Jeremiah LaRocco, jl2 on github
				        
(defconstant *temp* (list (cons 30 50) (cons 100 420) (cons 300 200)))
(defconstant *cos* (list (cons 0 1) (cons (/ PI 8) (cos (/ PI 8))) (cons (/ PI 16) (cos (/ PI 16)))
			 (cons (/ PI 12) (cos (/ PI 12))) (cons (/ PI 32) (cos (/ PI 32)))
			 (cons (/ PI 10) (cos (/ PI 10)))))
(defvar *id-nbr* 0)
(defconstant *points* *cos*) ;points to use
(defconstant *runtime* 1) ;;minutes
(defconstant *output-update* 200) ;generations between update
(defconstant *swap* 50) ;generations between movement between populations
(defconstant *popsize* 100) ;individuals in each population
(defconstant *populations* 7) ;nbr of populations
(defconstant *mutaterate* 0.1) ;% of individuals with 1 mutation each generation
(defconstant *reproductions* 30) ;nbr of couples who try to reproduce
(defconstant *reprofail* 0.1) ;% fail to reproduce
(defconstant *start* 6) ;elements in chromo1 to start with
(defconstant *unmutateable* 1) ;nbr individuals in each population that can't mutate
(defconstant *worstsurvival* 10) ;nbr individuals with the worst fitness to survive a generation
(defvar *update-t* (get-universal-time))

;elements (value, objects needed)
(defconstant *elements* (list (cons 'x 0) (cons 1 0) (cons 2 0) (cons 3 0) (cons 4 0) (cons 5 0) (cons 6 0) (cons 7 0) (cons 8 0) (cons 9 0) (cons '* 2) (cons '- 2) (cons '+ 2) (cons '/ 2) (cons '(* x x) 0) (cons 'x 0))) ;(cons 'exp 1) (cons 'sin 1) (cons 'cos 1) (cons 'tan 1)))
;leaves elements that are leaves
(defparameter *leaves* nil)
(mapc #'(lambda (x) (if (eq (cdr x) 0) (push x *leaves*))) *elements*)

(defstruct indiv id fitness chromo1 chromo2 formula)
(defparameter *population* (make-array *populations*))
(defparameter *children* (make-array *populations*))
(defparameter *DB* (make-hash-table :test #'equal))
(setf *random-state* (make-random-state t))

(defun main (points)
  (setup *popsize* points *populations*)
  (let ((start-t (get-universal-time)))
    (loop for i
	  until (and (> (- (get-universal-time) *update-t*) (* *runtime* 60)) (output-best i points));start-t) (* *runtime* 60))
	  do
	  (reproduce *popsize* *reproductions* *reprofail* points *populations*)
	  (mutate *mutaterate* *unmutateable* points *populations*)
	  (nextgen *popsize* *worstsurvival* *populations*)
	  (when (eq (mod i *swap*) 0)
	    (let ((a (car (aref *population* 0))))
	      (loop for j below *populations* do
		    (if (eq (+ 1 j) *populations*)
			(setf b a)
		      (setf b (car (aref *population* (mod (+ 1 j) *populations*)))))
		    (setf (aref *population* j) (cons b (cdr (aref *population* j)))))))
					;	(print *population*)
	  (when (eq 0 (mod i *output-update*))
	    (progn
	      (output-best i points))))
    (output-best 0 points)))

(let ((bestfit most-positive-double-float))
  (defun output-best (nbr points)
    (print 'i=)
    (princ nbr)
    (let ((best (car (sort (loop for i across *population* collect (car i)) #'< :key #'indiv-fitness))))
      (if (or (not (eq bestfit (indiv-fitness best))) (eq nbr 0))
	  (progn
	    (setf *update-t* (get-universal-time))
	    (setf bestfit (indiv-fitness best))
	    (print (indiv-fitness best))
	    (print (indiv-formula best))
	    (results (indiv-formula best) points)
	    nil)
	t))))
    

(defun translate(gene)
  (let ((nbr -1))
    (labels ((next ()
		   (nth (incf nbr) gene)))
	    (labels ((translate (a)
				(case (cdr a)
				      ((1) (let* ((b (next))
						  (x (if (eq 0 (cdr b))
							 (car b)
						       (translate b))))
					     `(,(car a) ,x)))
				      ((2) (let* ((b (next))
						  (c (next))
						  (x (if (eq 0 (cdr b))
							 (car b)
						       (translate b)))
						  (y (if (eq 0 (cdr c))
							 (car c)
						       (translate c))))
					     `(,(car a) ,x ,y)))
				      ((0) (car a))
				      (t 'x))))
		    (translate (next))))))

(defun evaluate (func a)
  (setq val func x a)
  (eval val))

(defun random-leaf ()
  (nth (random (length *leaves*)) *leaves*))

(defun random-element ()
  (nth (random (length *elements*)) *elements*))

(defun setup (size points pops)
  (loop for j across *population*
	for k do
	(loop repeat (* 1 size) do
	      (let ((i (make-indiv)))
		(setf (indiv-chromo1 i) (loop repeat *start* collect (random-element)))		
		(setf (indiv-chromo2 i) (loop repeat (1+ *start*) collect (random-leaf)))
		(setf (indiv-formula i) (translate (append (indiv-chromo1 i) (indiv-chromo2 i))))
		(setf (indiv-fitness i) (fitness (indiv-formula i) points))
		(setf (indiv-id i) (incf *id-nbr*))
		(push i j)))
	(setf (aref *population* k) (subseq (sort j #'< :key #'indiv-fitness) 0 size))))

(defun calc-fitness (form points)
  (let ((m (ignore-errors (loop for i in points sum
				(let* ((x (evaluate form (car i)))
				       (x2 (- x (cdr i))))
				  (* x2 x2))))))
    (if (equal m nil)
	most-positive-double-float
      m)))

(defun fitness (form points)
  (let ((val (gethash form *DB*)))
    (if val
	val
      (setf (gethash form *DB*) (calc-fitness form points)))))

;	(reproduce *popsize* new fail points)

(defun rand-not-i (nbr i)
  (let ((n (random nbr)))
    (if (eq i n)
	(rand-not-i nbr i)
      n)))

(defun reproduce (pop new fail points pops)
  (loop for j below pops do
	(loop repeat new for i do
	      (when (> (random 100)(* 100 fail))
		(child (nth i (aref *population* j)) (nth (rand-not-i pop i) (aref *population* j)) points j)))))
  ;(setf *population* (sort (append *population* *children*) #'< :key #'indiv-fitness))
  ;(setf *children* nil))

(defun child (pa ma points pop)
  (let ((c1m (random (length (indiv-chromo1 ma))))
	(c2m (random (length (indiv-chromo2 ma))))
	(c1p (random (length (indiv-chromo1 pa))))
	(c2p (random (length (indiv-chromo2 pa))))
	(new1 (make-indiv))
	(new2 (make-indiv)))
    (setf (indiv-id new1) (incf *id-nbr*))
    (setf (indiv-id new2) (incf *id-nbr*))
    (setf (indiv-chromo1 new1)
	  (append (loop for i in (indiv-chromo1 ma)
			for j upto c1m
			collect i)
		  (nthcdr c1p (indiv-chromo1 pa))))
    (setf (indiv-chromo1 new2)
	  (append (loop for i in (indiv-chromo1 pa)
			for j upto c1p
			collect i)
		  (nthcdr c1m (indiv-chromo1 ma))))
    (setf (indiv-chromo2 new2)
	  (append (loop for i in (indiv-chromo2 ma)
			for j upto c2m
			collect i)
		  (nthcdr c2p (indiv-chromo2 pa))))
    (setf (indiv-chromo2 new1)
	  (append (loop for i in (indiv-chromo2 pa)
			for j upto c2p
			collect i)
		  (nthcdr c2m (indiv-chromo2 ma))))
    ;removed due to being slow, but now some have values that's connected to the length and not its genes
;    (setf (indiv-chromo2 new1) (append (indiv-chromo2 new1)
;				       (loop repeat (1+ (- (length (indiv-chromo1 new1)) (length (indiv-chromo2 new1))))
;					     collect (random-leaf))))
 ;   (setf (indiv-chromo2 new2) (append (indiv-chromo2 new2)
;				       (loop repeat (1+ (- (length (indiv-chromo1 new2)) (length (indiv-chromo2 new2))))
;					     collect (random-leaf))))
    (setf (indiv-formula new1) (translate (append (indiv-chromo1 new1) (indiv-chromo2 new1))))
    (setf (indiv-fitness new1) (fitness (indiv-formula new1) points))
    (push new1 (aref *children* pop))
    (setf (indiv-formula new2) (translate (append (indiv-chromo1 new2) (indiv-chromo2 new2))))
    (setf (indiv-fitness new2) (fitness (indiv-formula new2) points))
    (push new2 (aref *children* pop))))
				   ;	(mutate *mutaterate* safe)

(defun mutate (rate safe points pops)
  (loop for pop below pops do
	(let ((i 0))
	  (mapc #'(lambda (x) (unless (<= (incf i) safe)
				(when (< (random 100) (* rate 100))
				  (progn (if (> (random 3) 0)
					     (let ((j (random (length (indiv-chromo1 x)))))
					       (setf (nth j (indiv-chromo1 x)) (random-element)))
					   (let ((j (random (length (indiv-chromo2 x)))))
					     (setf (nth j (indiv-chromo2 x)) (random-leaf))))
					 (setf (indiv-formula x) (translate (append (indiv-chromo1 x) (indiv-chromo2 x))))
					 (setf (indiv-fitness x) (fitness (indiv-formula x) points))
					 (setf (indiv-id x) (incf *id-nbr*))))))
		(aref *population* pop)))
	(setf (aref *population* pop) (sort (aref *population* pop)  #'< :key #'indiv-fitness))))

					;	(nextgen *popsize* survive)

(defun nextgen (size leave pops)
  (loop for pop below pops do
	(setf (aref *population* pop)
	      (sort (append (aref *children* pop)
			    (nthcdr (- (length (aref *population* pop)) leave) (aref *population* pop))
			    (loop for i in (aref *population* pop)
				  for j below (- (length (aref *population* pop)) (length (aref *children* pop)) leave)
				  collect i)) #'< :key #'indiv-fitness))
	(setf (aref *children* pop) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun results (form points)
  (let ((img (make-svg)))
    (range form points img)
    
    (with-open-file (stream "GP.svg" :direction :output :if-exists :supersede)
		    (to_file img :stream stream))))

(defun range (form points img)
  (let* ((xps (loop for p in points collect (car p)))
	 (xmi (min xps))
	 (xma (max xps))
	 (diff (- xma xmi))
	 (xmin (- xmi (* diff 0.1)))
	 (xmax (+ xma (* diff 0.1)))
	 (xpoints (loop for j upto 100 collect (+ xmin (* j (/ (- xmax xmin) 100)))))
	 (ypoints (loop for x in xpoints collect (evaluate form x)))
	 (ymi (min (append ypoints (mapcar #'cdr points))))
	 (yma (max (append ypoints (mapcar #'cdr points))))
	 (diffy (- ymi yma))
	 (ymin (- ymi (* diffy 0.1)))
	 (ymax (+ yma (* diffy 0.1))))


    (labels ((fitx (k) (- (/ (* 500 k) (- xmax xmin)) (/ (* xmin 500) (- xmax xmin))))
	     (fity (k) (+ 50 (+ (* -1 (- (/ (* 500 k) (- yma ymi)) (/ (* ymi 500) (- yma ymi)))) 500))))
	    (let ((xp (mapcar #'fitx xpoints))
		  (yp (mapcar #'fity ypoints))
		  (p1 (mapcar #'(lambda (k) (fitx (car k))) points))
		  (p2 (mapcar #'(lambda (k) (fity (cdr k))) points)))

	      (loop for x below (1- (length xp))
		    for y below (1- (length yp)) do
		    (add-line img (floor (nth x xp)) (floor (nth y yp)) (floor (nth (1+ x) xp)) (floor (nth (1+ y) yp))))

	      (loop for x in p1
		    for y in p2 do
		    (add-point img (floor x) (floor y)))))))
		    ;(print (format t "(~a,~a)" x y)))))))
(defun min (x)
  (let ((m (car x)))
    (loop for i in x do (when (< i m) (setf m i)))
    m))

(defun max (x)
  (let ((m (car x)))
    (loop for i in x do (when (> i m) (setf m i)))
    m))
    
    
						   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(main *points*)

;(print *population*)
