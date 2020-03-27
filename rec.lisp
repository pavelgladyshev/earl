;******************************
;* Event reconsturciton algorithm
;******************************

; proper computation predicate

(defun cp (c)
  (if (atom c)
    t
    (if (atom (cdr c))
       (and (null (cdr c))
            (eventp (caar c))
	    (statep (cadar c)))
      (and (eventp (caar c))
	   (statep (cadar c))
           (equal (st (caar c) (cadar c)) (cadadr c))
	   (cp (cdr c))))))


(defvar *ALL-SINGLE-STEP-PATT* 
          (listify-elements *ALL-EVENT-STATE-PAIRS*))


;
; partial list description language
;
; "true" lists denote themselves
; "untrue" lists denote patterns -- sets of lists 

; proper description -- a list of patterns

(defun dp (d)
  (if (atom d)
    t
    (and (cp (car d))
	 (dp (cdr d)))))

; a pattern is a special kind of computation. It is assumed
; that a pattern denotes all computations that begin with it.
; the following function checks if given computation c
; matches given pattern p.

(defun matches (c p)
  (if (atom p)
    t
    (if (atom c)
      nil
      (and (equal (car c) (car p))
	   (matches (cdr c) (cdr p))))))
  
; a computation matches description if it matches one of the 
; patterns

(defun in (c d)
  (if (atom d)
    nil
    (or (matches c (car d))
	(in c (cdr d)))))

; Intersection of two descriptions

(defun intpp (p1 p2)
  (if (matches p1 p2) 
    p1
    (if (matches p2 p1)
      p2
      nil)))

(defun intpd (p d acc)
  (if (atom d)
    acc
    (let ((i (intpp p (car d))))
      (if (null i)
        (intpd p (cdr d) acc)
        (intpd p (cdr d) (cons i acc))))))

(defun intdd-helper (d1 d2 acc)
  (if (atom d1)
    acc
    (intdd-helper (cdr d1) d2 (intpd (car d1) d2 acc))))

(defun intdd (d1 d2)
  (intdd-helper d1 d2 nil))

; union of two descriptions

(defun uindd (d1 d2)
  (append d1 d2))

; test for emptiness of a description

(defun emp (d) (atom d))

; single-step reverser

(defun revcomp (c)
  (if (atom c)
    *ALL-SINGLE-STEP-PATT*
   (combine (rev-st (cadar c)) c)))

(defun rev (lst)
  (if (atom lst)
    nil
    (append (revcomp (car lst))
            (rev (cdr lst)))))

; multi-step reverser 

(defun revers (os d)
  (if (emp d)
    nil
    (if (atom os) 
      d
      (revers (cdr os) (intdd (car os) (rev d))))))


; convert observation into list of single-step observations

(defun single-step-obs (p n)
  (if (zp n)
    nil
    (cons p (single-step-obs p (1- n)))))

(defun single-step-os (fos)
  (if (atom fos) 
    nil
    (append (single-step-obs (first (car fos)) 
                             (second (car fos)))
            (single-step-os (cdr fos)))))


; convert generic observation sequence into equivalent 
; list of fixed-length observation sequences

(defun fix-obs (p min opt)
  (if (zp opt)
    (cons 
       (cons p (cons min (cons opt nil))) 
       nil)
    (cons 
       (cons p (cons (+ min opt) (cons 0 nil))) 
       (fix-obs p min (1- opt)))))

(defun fix-os (os)
  (if (atom os)
    (cons nil nil)
    (product (fix-obs (first (car os)) 
                      (second (car os)) 
                      (third (car os))) 
             (fix-os (cdr os)))))


; solving observation sequence

(defun fos-lengths (fos)
  (if (atom fos)
    nil
    (cons (second (car fos)) (fos-lengths (cdr fos)))))

(defun fos-total-len (fos-length-list)
   (if (atom fos-length-list) 
     0 
     (+ (car fos-length-list)
        (fos-total-len (cdr fos-length-list)))))

(defun solve-fix-os (fos)
  (cons (cons (fos-lengths fos) 
          (cons (revers (reverse (single-step-os fos)) '(nil)) nil)) nil))

(defun solve-fix-os-list (fix-os-list)
  (if (atom fix-os-list)
    nil
    (append (solve-fix-os (car fix-os-list)) 
            (solve-fix-os-list (cdr fix-os-list)))))

(defun solve-os (os)
  (solve-fix-os-list (fix-os os)))

(defun select-comps (l acc)
  (if (atom l)
    acc
    (if (cp (car l))
      (select-comps (cdr l) (cons (car l) acc))
      (select-comps (cdr l) acc))))

;(comp 'select-comps)

(defvar *ALL-TWO-STEP-PATT* 
  (select-comps 
    (product *ALL-EVENT-STATE-PAIRS* 
             *ALL-SINGLE-STEP-PATT*) nil))

             
(defmacro defpatt1-helper (const-name tester-name vars body)
  `(progn
     (defun ,tester-name (comp-list acc)
       (if (atom comp-list)
         acc
         (if ((lambda ,vars ,body) (caar comp-list))
           (,tester-name (cdr comp-list) (cons (car comp-list) acc))
           (,tester-name (cdr comp-list) acc))))

     (defvar ,const-name 
       (,tester-name *ALL-SINGLE-STEP-PATT* nil))))

(defmacro defpatt1 (name vars body)
  (let 
    ((tester-name 
      (intern
       (concatenate 'string (symbol-name name) "-TESTER"))))
    `(defpatt1-helper ,name ,tester-name ,vars ,body)))

(defmacro defpatt2-helper (const-name tester-name vars body)
  `(progn
     (defun ,tester-name (comp-list acc)
       (if (atom comp-list)
         acc
         (if ((lambda ,vars ,body) 
                  (caar comp-list) 
                  (cadar comp-list))
           (,tester-name (cdr comp-list) 
                         (cons (car comp-list) acc))
           (,tester-name (cdr comp-list) acc))))

     (defvar ,const-name 
       (,tester-name *ALL-TWO-STEP-PATT* nil))))

(defmacro defpatt2 (name vars body)
  (let 
    ((tester-name 
      (intern
       (concatenate 'string (symbol-name name) "-TESTER"))))
    `(defpatt2-helper ,name ,tester-name ,vars ,body)))


; intersecting solutions of two observation sequences


(defun singleton-es-chunk (os-chunk)
   (cons (cons (car os-chunk) nil) 
      (cons (cadr os-chunk) nil)))

(defun singleton-es-sol (os-sol)
   (if (atom os-sol)
     nil
     (cons (singleton-es-chunk (car os-sol)) 
           (singleton-es-sol (cdr os-sol)))))

(defun add-chunk (os-chunk es-chunk)
   (if (equal (fos-total-len (car os-chunk)) 
              (fos-total-len (caar es-chunk)))
     (let ((intersection 
            (intdd (cadr os-chunk) (cadr es-chunk))))
       (if (emp intersection)
         nil
         (cons 
          (cons (cons (car os-chunk) (car es-chunk)) 
                  (cons intersection nil)) 
          nil)))
     nil))

(defun add-chunk-to-sol (es-sol os-chunk)
   (if (atom es-sol)
     nil
     (append (add-chunk os-chunk (car es-sol)) 
             (add-chunk-to-sol (cdr es-sol) os-chunk))))

(defun add-sol (os-sol es-sol)
   (if (atom os-sol)
     nil
     (append (add-chunk-to-sol es-sol (car os-sol)) 
             (add-sol (cdr os-sol) es-sol))))

(defun solve-es (es)
  (if (atom es) 
    nil
    (if (atom (cdr es))
      (singleton-es-sol (solve-os (car es)))
      (add-sol (solve-os (car es)) (solve-es (cdr es))))))


; --- debugging tools ---

; stepper function
(defun stn (cl s)
  (if (atom cl)
    s
    (stn (cdr cl) (st (car cl) s))))











