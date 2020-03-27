;********************************************************
;*         TEMPORAL BOUNDING OF EVENTS
;********************************************************


; ACL2 functions not defined in CMU CL 

(DEFUN MEMBER-EQUAL (X LST)
  (COND ((ENDP LST) NIL)
        ((EQUAL X (CAR LST)) LST)
        (T (MEMBER-EQUAL X (CDR LST)))))


(DEFUN NFIX (X) 
  (IF (AND (INTEGERP X) (>= X 0)) X 0))


; make list of all observation id's for given observation sequence obs.
; m is the index of obs in the evidential statement;
; n is the index of the first element of obs.

(defun allobs (obs m n)
  (if (atom obs)
    nil
    (cons (list (nfix m) (nfix n)) 
          (allobs (cdr obs) (nfix m) (+ (nfix n) 1)))))

; make list of observation id's for given evidential statement es.
; n is the index of the first observation sequence in es. 

(defun alles (es n)
  (if (atom es)
    nil
    (append (allobs (car es) (nfix n) 0) 
            (alles (cdr es) (+ (nfix n) 1)))))

; intersection-equal (taken from books/data-structures/set-defuns.lisp)
; returns list whose elements are members of both x and y

(defun intersection-equal (x y)
  ;(declare (xargs :guard (and (true-listp x) (true-listp y))))
  (cond ((endp x) nil)
	((member-equal (car x) y)
	 (cons (car x) (intersection-equal (cdr x) y)))
	(t (intersection-equal (cdr x) y))))

; sum first n elements of the list l

(defun sumpref (n l)
  (if (or (zp n) (atom l))       
    0
    (+ (nfix (car l)) (sumpref (1- n) (cdr l)))))

; ==== Calculating the lower (earliest) boundary ====


; Find id's of all runs in the given partition map pm,
; whose last computation appears in the partitioned run at a position 
; less than or equal to pos.
;  
; pm is the partition map;
; i is the index of the partition map in the partition map list;
; j is the index of the first run in the partition;
; cnt is the position of the first run of pm in the partitioned run.

(defun befpm (pm cnt pos i j)
  (if (atom pm)
    nil
    (if (<= (+ cnt (car pm)) pos)
      (cons (list i j) 
            (befpm (cdr pm) (+ cnt (car pm)) pos i (+ j 1)))
      (befpm (cdr pm) (+ cnt (car pm)) pos i (+ j 1)))))

; find id's of all runs in the given partition map list pml, 
; whose last computation appears in the partitioned run at a position
; less than or equal to pos.
;
; i is the index of the first partition map in pml.

(defun befpml (pos pml i)
  (if (atom pml)
    nil
    (append (befpm (car pml) 0 pos i 0)
            (befpml pos (cdr pml) (+ i 1)))))

; For the given list of partition list chunks v, 
; find id's of all runs that precede run with ID=(i j) in
; all partition chunks of v.
;
; bef is the set of id's that passed test so far. Initially
;       bef is set to the set of all observation ids.
;       At each step, it is intersected with the set of
;       id'satisfying test for the current partition list chunk. 

(defun findbef (v bef i j)
  (if (atom v)
    bef
    (findbef (cdr v)
             (intersection-equal 
                bef 
                (befpml (sumpref j (nth i (car (car v))))
                        (car (car v))
                        0))
             i j)))

; Find in the associative list tim of known observation times
; the maximal time whose id belongs to the set l. 

(defun maxtime (max l tim)
  (if (atom tim)
    max
    (let ((time (car (cdr (car tim))))
          (id (car (car tim))))
      (if (not (member-equal id l))
        (maxtime max l (cdr tim))
        (if (null max)
          (maxtime time l (cdr tim))
          (if (< max time)
            (maxtime time l (cdr tim))
            (maxtime max l (cdr tim))))))))


; Find the earliest time boundary for observation
; with ID=(i j) in evidential statement es, given list v of 
; partition list chunks and list of known observation times tim.  

(defun lbound (i j es v tim)
  (maxtime nil 
           (findbef v (alles es 0) i j)
           tim))

#|
(lbound 2 1 
        '((((1 2 3) 1 25) ((3 4) 1 25)) 
          (((1 2 3 4) 3 25)) 
          ((universe 0 25) ((2) 1 25) (universe 0 25))) 

        '(((4) ((1 3) (4) (2 1 1))) 
          ((4) ((2 2) (4) (2 1 1))) 
          ((3) ((1 2) (3) (1 1 1))) 
          ((4) ((1 2) (3) (2 1 0))) 
          ((4) ((2 1) (3) (2 1 0)))) 

        '(((0 0) 6) ((2 2) 7) ((0 0) 8)))

Answer: 8
|#

; ==== Calculating the upper (latest) boundary ====

; Find id's of all runs in the given partition map pm,
; whose first computation appears in the partitioned run at a position 
; greater than or equal to pos.
;  
; pm is the partition map;
; i is the index of the partition map in the partition map list;
; j is the index of the first run in the partition;
; cnt is the position of the first run of pm in the partitioned run.

(defun aftpm (pm cnt pos i j)
  (if (atom pm)
    nil
    (if (<= pos cnt)
      (cons (list i j) 
            (aftpm (cdr pm) (+ cnt (car pm)) pos i (+ j 1)))
      (aftpm (cdr pm) (+ cnt (car pm)) pos i (+ j 1)))))

; find id's of all runs in the given partition map list pml, 
; whose first computation appears in the partitioned run at a position
; greater than or equal to pos.
;
; i is the index of the first partition map in pml.

(defun aftpml (pos pml i)
  (if (atom pml)
    nil
    (append (aftpm (car pml) 0 pos i 0)
            (aftpml pos (cdr pml) (+ i 1)))))

; For the given list of partition list chunks v, 
; find id's of all runs that precede run with ID=(i j) in
; all partition chunks of v.
;
; aft is the set of id's that passed test so far. Initially
;       aft is set to the set of all observation ids.
;       At each step, it is intersected with the set of
;       id'satisfying test for the current partition list chunk. 

(defun findaft (v aft i j)
  (if (atom v)
    aft
    (findaft (cdr v)
             (intersection-equal 
                aft 
                (aftpml (sumpref (+ j 1) ; include length of run into sum
                                 (nth i (car (car v))))
                        (car (car v))
                        0))
             i j)))

; Find in the associative list tim of known observation times
; the minimal time whose id belongs to the set l. 

(defun mintime (min l tim)
  (if (atom tim)
    min
    (let ((time (car (cdr (car tim))))
          (id (car (car tim))))
      (if (not (member-equal id l))
        (mintime min l (cdr tim))
        (if (null min)
          (mintime time l (cdr tim))
          (if (< time min)
            (mintime time l (cdr tim))
            (mintime min l (cdr tim))))))))

; Find the latest time boundary for observation
; with ID=(i j) in evidential statement es, given list v of 
; partition list chunks and list of known observation times tim.  

(defun ubound (i j es v tim)
  (mintime nil 
           (findaft v (alles es 0) i j)
           tim))

