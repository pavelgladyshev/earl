;***************************
;* Printer analysis example
;***************************

(load "util")

; ======== Finite State Machine =======

      
; Proper state predicate

(defun valuep (v)
  (or (equal v 'empty)
      (equal v 'A)
      (equal v 'B)
      (equal v 'B_deleted)
      (equal v 'A_deleted)))

(defun statep (s)
  (and (equal (cdr (cdr s)) nil)
       (valuep (first s))
       (valuep (second s))))

; Proper event predicate

(defun eventp (e)
  (or (equal e 'add_A)
      (equal e 'add_B)
      (equal e 'take)))


; Set of all possible event-state pairs

(defconst *ALL-EVENT-STATE-PAIRS* 
  (product 
    '(take add_A add_B) 
    (listify-elements (product 
                        '(empty A B B_deleted A_deleted) 
                         (listify-elements 
                           '(empty A B B_deleted A_deleted))))))

; Transition funciton

(defun st (c s)
  (let ((d1 (first s))
	(d2 (second s)))
    (cond ((equal c 'add_A) 
             (if (or (equal d1 'A) 
		       (equal d2 'A))
	       s
	       (if (or (equal d1 'empty)
		       (equal d1 'A_deleted)
		       (equal d1 'B_deleted))
                 (list 'A d2)
		 (if (or (equal d2 'empty)
			 (equal d2 'A_deleted)
			 (equal d2 'B_deleted))
		   (list d1 'A)
		   s))))
           ((equal c 'add_B) 
               (if (or (equal d1 'B) 
		       (equal d2 'B))
		 s
		 (if (or (equal d1 'empty)
			 (equal d1 'A_deleted)
			 (equal d1 'B_deleted))
		   (list 'B d2)
		   (if (or (equal d2 'empty)
			   (equal d2 'A_deleted)
			   (equal d2 'B_deleted))
		     (list d1 'B)
		     s))))
	   ((equal c 'take)
	       (if (equal d1 'A)
		 (list 'A_deleted d2)
		 (if (equal d1 'B)
		   (list 'B_deleted d2)
		   (if (equal d2 'A)
		     (list d1 'A_deleted)
		     (if (equal d2 'B)
		       (list d1 'B_deleted)
		       s)))))
	   (t s))))


; Inverse transition function

(defun rev-st (s)
  (let ((d1 (first s))
	(d2 (second s)))
    (append
       (if (equal d1 'A_deleted)
         (list (list 'take (list 'A d2)))
	 nil)
       (if (equal d1 'B_deleted)
	 (list (list 'take (list 'B d2)))
	 nil)
      (if (and (not (equal d1 'A)) 
	       (not (equal d1 'B))
	       (equal d2 'A_deleted))
	 (list (list 'take (list d1 'A)))
	 nil)
      (if (and (not (equal d1 'A))
	       (not (equal d1 'B))
	       (equal d2 'B_deleted))
	 (list (list 'take (list d1 'B)))
	 nil)
      (if (and (or (equal d1 'A_deleted)
		   (equal d1 'B_deleted)
		   (equal d1 'empty))
	       (or (equal d2 'A_deleted)
		   (equal d2 'B_deleted)
		   (equal d2 'empty)))
	(list (list 'take s))
	nil)
      (if (and (equal d1 'A) (not (equal d2 'A)))
         (list (list 'add_A (list 'empty d2))
	       (list 'add_A (list 'A_deleted d2))
               (list 'add_A (list 'B_deleted d2)))
	 nil)
      (if (and (equal d1 'B) (not (equal d2 'B)))
         (list (list 'add_B (list 'empty d2))
	       (list 'add_B (list 'A_deleted d2))
               (list 'add_B (list 'B_deleted d2)))
	 nil)
      (if (and (equal d1 'B)
	       (equal d2 'A))
         (list (list 'add_A (list d1 'empty))
	       (list 'add_A (list d1 'A_deleted))
               (list 'add_A (list d1 'B_deleted)))
	 nil)
      (if (and (equal d1 'A)
	       (equal d2 'B))
        (list (list 'add_B (list d1 'empty))
	      (list 'add_B (list d1 'A_deleted))
              (list 'add_B (list d1 'B_deleted)))
	nil)
      (if (or (equal d1 'A) (equal d2 'A))
        (list (list 'add_A s))
	nil)
      (if (and (equal d1 'A) (equal d2 'A))
        (list (list 'add_B s))
	nil)
     (if (and (equal d1 'B) (equal d2 'B))
        (list (list 'add_A s))
	nil)
      (if (or (equal d1 'B) (equal d2 'B))
        (list (list 'add_B s))
	nil))))


; ---- Loading generic reconsturciton algorithm ------

(load "rec")

; ---- Loading drawing utility -----

(load "draw")

; ===== Formalisation of evidence ===== 

 ; The value of infinitum

 (defconst *infinitum* 6)

 ; The set of all computations

 (defconst *C_T* *ALL-SINGLE-STEP-PATT*)

 ; Carl's story

 (defpatt1 *B-DELETED* 
        (x) (and (equal (first (second x)) 'B_deleted) 
                 (equal (second (second x)) 'B_deleted)))

 (defconst *OS-CARL*  `((,*C_T* 0 ,*infinitum*) (,*B-DELETED* 1 0)))

 ; Manufacturer's story

 (defpatt1 *EMPTY* 
        (x) (and (equal (first (second x)) 'empty) 
                 (equal (second (second x)) 'empty)))


 (defconst *OS-MANU* `((,*EMPTY* 1 0) (,*C_T* 0 ,*infinitum*))) 

 (defconst *ES-ACME* `(,*OS-MANU* ,*OS-CARL*))

 ; ============ Investigative hypothesis =================

 ; Alice's claim restricted to exclude speculative transitions 

 (defpatt2 *ALICE-PRIME* (x y) 
        (and (not (or (equal (first (second x)) 'A) 
                      (equal (second (second x)) 'A)))
             (not (equal (second x) (second y)))
             (not  (and (equal (first (second x)) 'B_deleted) 
                        (equal (second (second x)) 'B_deleted)
                        (equal (first x) 'Add_B)
                        (equal (first (second y)) 'B) 
                        (equal (second (second y)) 'B_deleted)
                        (equal (first y) 'take)))))

 (defconst *OS-PRIME-ALICE* `((,*ALICE-PRIME* 0 ,*infinitum*) 
                            (,*B-DELETED* 1 0)))

 (defconst *ES-PRIME-PRIME-ACME* (cons *OS-PRIME-ALICE* *ES-ACME*))

 ; ========== Computing meanings of evidential statements =========

 (defconst *ES-ACME-SOL* (solve-es *ES-ACME*))
 (defconst *ES-PRIME-PRIME-ACME-SOL* (solve-es *ES-PRIME-PRIME-ACME*))

 (print "Is the meaning of es_ACME empty ?")
 (print (null *ES-ACME-SOL*))  

 (print "Is the meaning of es''_ACME empty ?")
 (print (null *ES-PRIME-PRIME-ACME-SOL*))  

 ; To generate GraphViz T.DOT file describing es_ACME solution
 ; as a diagram, uncomment the following:

 (draw *ES-ACME-SOL*) 









































