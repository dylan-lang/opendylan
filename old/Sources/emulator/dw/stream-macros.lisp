(in-package :dylan)

(define-dylan-macro print-unreadable-object (_ o-s &rest stuff)
  (destructuring-bind (o s) o-s
    (if (null stuff)
      `(begin
         (format ,s "[~s]" (class-debug-name (object-class ,o)))
         ,o)
      `(begin
         (format ,s "[~s - " (class-debug-name (object-class ,o)))
         ,@stuff
         (format ,s "]")
         ,o))))

(define-dylan-macro print-unreadable-named-object (_ o-n-s &rest stuff)
  (destructuring-bind (o n s) o-n-s
    (if (null stuff)
      `(begin
         (format ,s "[~a]" ,n)
         ,o)
      `(begin
         (format ,s "[~a - " ,n)
         ,@stuff
         (format ,s "]")
         ,o))))

(define-dylan-macro with-open-file (_ parameters &rest body)
   (destructuring-bind (stream-name &rest arguments) parameters
    `(bind ((,stream-name ,*dylan-canonical-false*))
      (unwind-protect
	  (begin
	   (set! ,stream-name (open ,@arguments))
	   ,@body)
        (when ,stream-name
	  (close ,stream-name))))))

;; eof
