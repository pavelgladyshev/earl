;********************
;* Helper functions 
;********************

; append suff to the end of each element in lst 
(defun combine (lst suff)
  (if (atom lst)
    nil
    (cons (cons (car lst) suff) 
	  (combine (cdr lst) suff))))

; make list of all possible pairwise 
; combinations of the  elements of two lists
(defun product (l1 l2)
  (if (atom l2)
    nil
    (append (combine l1 (car l2)) (product l1 (cdr l2)))))

; convert every element of list into a singleton list
(defun listify-elements (l) 
  (if (atom l)
    nil
    (cons (cons (car l) nil) (listify-elements (cdr l)))))

; similar to combine, but uses append instead of cons-ing 
; and listifies the result 
(defun combine-append (lst suff)
  (if (atom lst)
    nil
    (cons (cons (append (car lst) suff) nil)
	  (combine-append (cdr lst) suff))))

; prepends each element in the given gist with the given prefix
(defun prepend (pref lst)
   (if (atom lst)
     nil
     (cons (cons pref (cons (car lst) nil)) (prepend pref (cdr lst)))))

(defun zp (x)
  (if (integerp x) (eq x 0) t))

(defun take (n lst)
  (if (zp n)
    nil
    (cons (car lst) (take (- n 1) (cdr lst)))))

; defconst macro for use in ordinary Common Lisp environment
(defmacro defconst (x y) `(defvar ,x ,y))