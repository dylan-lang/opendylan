(in-package dylan)

(defun collect-duplicate-symbols (s)
  (let ((sorted-s (sort s 
                        #'(lambda (x y)
			    (string-lessp 
			      (symbol-name x) (symbol-name y))))))
    (collect-duplicates-aux sorted-s '())))

(defun collect-duplicates-aux (s col)
  (if (null (cdr s)) col
    (let ((first (car s))
          (rest (cdr s)))
      (if (not (eq first (first rest))) (collect-duplicates-aux rest col)
        (progn
          (loop while (eq (car rest) first)
                do (setq rest (cdr rest)))
          (collect-duplicates-aux rest (cons first col)))))))



          
