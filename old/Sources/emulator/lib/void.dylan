;; A void object for use within the library:

(bind ((void-object (list 'void)))
  
  (define void 
    (method () void-object))

  (define void?
    (method (o) (id? o void-object)))

)

;; eof
