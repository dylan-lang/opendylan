;; Misc function stuff:

(define-method as-apply-list (f (last <object>))
  (error "Apply called on ~s with non-sequence final argument ~s."
	 f last))

(define-method as-apply-list (f (last <list>))
  last)

(define-method as-apply-list (f (last <sequence>))
  (as <list> last))

(define apply
  (method (f #rest stuff)
    (case (size stuff)
      ((0) (f))
      ((1) (apply/list f 
                       (as-apply-list f (head stuff))))
      ((2) (apply/list f 
                       (head stuff) 
                       (as-apply-list f (head (tail stuff)))))
      ((3) (apply/list f
		       (head stuff) 
                       (head (tail stuff)) 
                       (as-apply-list f (head (tail (tail stuff))))))
      (else:
        (apply/list f
                    (bind-method loop ((args stuff))
                      (if (null? (tail args)) 
                        (as-apply-list f (head args))
                        (pair (head args) (loop (tail args))))))))))

(define-method make-read-only ((gf <generic-function>))
  (format #t "~&Warning: ignoring make-read-only on ~s~%" gf)
  gf)

(define-method freeze-methods ((gf <generic-function>) #rest specializers)
  (format #t "~&Warning: ignoring freeze-methods on ~s with ~s~%"
          gf specializers)
  gf)

;; eof
