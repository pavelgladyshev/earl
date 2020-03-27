;*******************************************
;* Drawing reconstruction results using DOT
;*******************************************

(defun digit-name (digit)
  (case digit
    (0 "0")
    (1 "1")
    (2 "2")
    (3 "3")
    (4 "4")
    (5 "5")
    (6 "6")
    (7 "7")
    (8 "8")
    (9 "9")
    (10 "A")
    (11 "B")
    (12 "C")
    (13 "D")
    (14 "E")
    (15 "F")
    (otherwise "?")))

(defun number-name-helper (n radix)
  (if (equal n 0)
    ""
    (concatenate 'string 
                 (number-name-helper (floor n radix) radix) 
                 (digit-name (rem n radix)))))
(defun number-name (n radix)
  (if (equal n 0) "0" (number-name-helper n radix)))

(defun pretty (l)
  (if (null l)
    ""
    (if (atom l) 
         (if (numberp l) (number-name l 10) (symbol-name l)) 
      (let ((s (car l)))
        (concatenate 
           'string 
           (if (symbolp s) (symbol-name s)
             (if (consp s) (concatenate 'string "(" (pretty s) ")")
               (if (numberp s) 
                 (number-name s 10) 
                 " ??? ")))
           (if (not (null (cdr l))) ", " "") (pretty (cdr l)))))))

(defun stringify (l acc)
   (if (atom l)
     acc
     (stringify (cdr l) (concatenate 'string acc "_" 
       (if (numberp (car l)) 
         (number-name (car l) 10)
         (symbol-name (car l)))))))

(defun flatten (l)
  (if (atom l)
    l
    (if (consp (car l)) 
      (append (flatten (car l)) (flatten (cdr l)))
      (cons (car l) (flatten (cdr l))))))

(defun draw-comp (file comp name1)
   (if (atom comp)
     nil
      (progn
        (format 
           file "n~A [label=\"~A\"];~%" name1 (pretty (cadar comp)))
        (if (atom (cdr comp)) 
          nil
          (let ((name2 (concatenate 
                         'string 
                         name1 
                         (stringify (flatten (cadr comp)) ""))))
            (progn
              (format 
                 file "n~A [label=\"~A\"];~%" name2 (pretty (cadadr comp)))
              (format 
                 file "n~A -> n~A [label=\"~A\"];~%" 
                      name2 name1 
                        (pretty (car (cadr comp))))
              (draw-comp file (cdr comp) name2)))))))

(defun draw-comps (file comps)
   (if (atom comps)
     nil
     (let ((rv (reverse (car comps))))
       (progn 
         (draw-comp file rv (stringify (flatten (cdar rv)) ""))
         (draw-comps file (cdr comps))))))


(defun draw-sol (file es-sol)
   (if (atom es-sol)
     nil
     (progn 
       (draw-comps file (cadar es-sol)) 
       (draw-sol file (cdr es-sol))))) 

(defun draw (es-sol)
   (with-open-file (f "t.dot" :direction :output :if-exists :supersede)
     (format f "strict digraph G { ~% size=\"8,11\";~%rankdir=LR;~%")
     (draw-sol f es-sol)
     (format f "}~%")
   ))