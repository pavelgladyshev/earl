;****************************************
;* Formalisation of slack space analysis
;****************************************

(load "util")

; ======== Finite State Machine =======


; Proper state predicate

(defun lenvaluep (l)
  (or (equal l 0)
      (equal l 1)
      (equal l 2)))

(defun datavalue1p (v)
  (or (equal v 'u)
      (equal v 't1)
      (equal v 'c1)))

(defun datavalue2p (v)
  (or (equal v 'c2)
      (equal v 't2)))

; The first element of state is the length of 
; The rest of the elements is the data store 
; e.g. (1 0 0)

(defun statep (s)
  (and (lenvaluep (car s))
       (datavalue1p (cadr s))
       (datavalue2p (caddr s))
       (null (cdddr s))))

; Proper event predicate

; Event is a sequence of 1s and 0s with at least 
; 1 element and at most 2 elements

(defun eventp (e)
  (or (equal e 'del)
      (and (datavalue1p (car e))
           (null (cdr e)))
      (and (datavalue1p (car e))
           (datavalue2p (cadr e))
           (null (cddr e)))))
 
; Set of all possible event-state pairs

(defconst *LENGTH*      '(0 1 2))

(defconst *ALL-STATES*  '((0 c1 c2) (0 c1 t2) (0 u c2) (0 u t2) (0 t1 c2) (0 t1 t2) 
                          (1 c1 c2) (1 c1 t2) (1 u c2) (1 u t2) (1 t1 c2) (1 t1 t2)
                          (2 c1 c2) (2 c1 t2) (2 u c2) (2 u t2) (2 t1 c2) (2 t1 t2)))

(defconst *ALL-EVENTS*  '(del (u) 
                              (t1) 
                              (c1) 
                              (u t2)  (u c2) 
                              (c1 c2) (c1 t2) 
                              (t1 c2) (t1 t2)))

(defconst *ALL-EVENT-STATE-PAIRS* 
  (product *ALL-EVENTS* (listify-elements *ALL-STATES*)))

; Transition funciton

(defun st (c s)
  (if (atom c)
    (if (equal c 'del)      (list 0  (cadr s) (caddr s))
                            s)
    (let ((l (length c)))
      (if (equal l 1)       (list 1 (car c) (caddr s))
      (if (equal l 2)       (list 2 (car c) (cadr c))
                            s)))))

; Inverse transition function

(defun rev-st (s)
  (let* ((l (car s))
         (data (cdr s)))
    (cond
      ((equal l 0) 
         (list (list 'del (list* 0 data))
               (list 'del (list* 1 data))
               (list 'del (list* 2 data))))
      ((equal l 1) 
         (list (list (list (car data)) (list* 0 'u (cdr data)))
               (list (list (car data)) (list* 0 't1 (cdr data)))
               (list (list (car data)) (list* 0 'c1 (cdr data)))
               (list (list (car data)) (list* 1 'u (cdr data)))
               (list (list (car data)) (list* 1 't1 (cdr data)))
               (list (list (car data)) (list* 1 'c1 (cdr data)))
               (list (list (car data)) (list* 2 'u (cdr data)))
               (list (list (car data)) (list* 2 't1 (cdr data)))
               (list (list (car data)) (list* 2 'c1 (cdr data)))))
      ((equal l 2) 
         (list (list data (list 0 'u 'c2))
               (list data (list 0 'u 't2))
               (list data (list 0 't1 'c2))
               (list data (list 0 't1 't2))
               (list data (list 0 'c1 'c2))
               (list data (list 0 'c1 't2))
               (list data (list 1 'u 'c2))
               (list data (list 1 'u 't2))
               (list data (list 1 't1 'c2))
               (list data (list 1 't1 't2))
               (list data (list 1 'c1 'c2))
               (list data (list 1 'c1 't2))
               (list data (list 2 'u 'c2))
               (list data (list 2 'u 't2))
               (list data (list 2 't1 'c2))
               (list data (list 2 't1 't2))
               (list data (list 2 'c1 'c2))
               (list data (list 2 'c1 't2))))
      (t nil))))

; ---- Loading generic reconsturciton algorithm ------

(load "rec")

; ---- Loading drawing utility -----

(load "draw")

; ---- Loading event time bounding algorithms ----

(load "ft")

; ===== Formalisation of evidence ===== 

 ; The value of infinitum

 (defconst *infinitum* 1)

 ; The set of all computations

 (defconst *C_T* *ALL-SINGLE-STEP-PATT*)

 ; Observed properties

 (defpatt1 *FINAL* 
        (x) (equal (second x) '(1 u t2)))

 (defpatt1 *BLACKMAIL-WRITE*
        (x) (equal (first x) '(t1 t2)))


 (defpatt1 *UNRELATED-WRITE*
        (x) (equal (first x) '(u)))

 (defpatt1 *NO-UNRELATED-WRITE*
        (x) (not (equal (first x) '(u))))
 
 (defconst *OS-FINAL*           `((,*C_T* 0 ,*infinitum*) (,*FINAL* 1 0)))
 
 (defconst *OS-UNRELATED*       `((,*C_T* 0 ,*infinitum*) 
                                (,*UNRELATED-WRITE* 1 0) 
                                (,*C_T* 0 ,*infinitum*)
                                (,*C_T* 0 0)
                                (,*C_T* 1 ,*infinitum*)))

 (defconst *OS-BLACKMAIL*       `((,*C_T* 0 ,*infinitum*)
                                (,*BLACKMAIL-WRITE* 1 0)
                                (,*C_T* 1 ,*infinitum*)))

 (defconst *OS-PRIME-UNRELATED* `((,*NO-UNRELATED-WRITE* 0 ,*infinitum*) 
                                (,*UNRELATED-WRITE* 1 0) 
                                (,*NO-UNRELATED-WRITE* 0 ,*infinitum*)
                                (,*NO-UNRELATED-WRITE* 0 0)
                                (,*NO-UNRELATED-WRITE* 1 ,*infinitum*)))

 (defconst *ES-BLACKMAIL* `( ,*OS-FINAL* 
                           ,*OS-UNRELATED* 
                           ,*OS-BLACKMAIL* ))

 (defconst *ES-PRIME-BLACKMAIL* `( ,*OS-FINAL* 
                                 ,*OS-PRIME-UNRELATED* 
                                 ,*OS-BLACKMAIL* ))


 (defconst *ES-ONE-STEP* `( ,*OS-FINAL* ))

 ; Compute the meanings of the evidential statements

 (defconst *SOL-1* (solve-es *ES-ONE-STEP*))

#|
 (defconst *SOL-4* (solve-es *ES-BLACKMAIL*))

 (defconst *SOL-PRIME-4* (solve-es *ES-PRIME-BLACKMAIL*))

 ; Run the time bounding algorithm 

 ; 5 is the time of the reception of unrelated event
 ; the times of all other events are unknown.
 (defconst *tim* '(((1 3) 5)))    

 ; time bouding of blackmail-write in es_blackmail

 (defconst *l* 
   (lbound 2 1 *ES-BLACKMAIL* *SOL-4* *tim*))

 (defconst *u* 
   (ubound 2 1 *ES-BLACKMAIL* *SOL-4* *tim*))

 ; time bounding of blackmail-write in es'_blackmail

 (defconst *l-prime* 
   (lbound 2 1 *ES-PRIME-BLACKMAIL* *SOL-PRIME-4* *tim*))

 (defconst *u-prime* 
   (ubound 2 1 *ES-PRIME-BLACKMAIL* *SOL-PRIME-4* *tim*))

|#

 ; To generate GraphViz T.DOT file describing *SOL-1* solution
 ; as a diagram, uncomment the following:

 ; (draw *SOL-1*) 

