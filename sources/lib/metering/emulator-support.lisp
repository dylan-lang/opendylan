(in-package dylan)

(define-dylan-translator emulator-binding translate-emulator-binding (form env)
  (destructuring-bind (_ name module) form
    (declare (ignore _))
    `(dylan-resolve-name ,name :module-name ,module)))

(define-dylan-translator 
    emulator-local-binding translate-emulator-local-binding (form env)
  (destructuring-bind (_ name) form
    (declare (ignore _))
    `(dylan-resolve-name ,name :module-name ,(current-module-name))))

(define-dylan-translator 
    emulator-module-package translate-emulator-module-package (form env)
  (destructuring-bind (_ name) form
    (declare (ignore _))
    `(module-package (find-translator-module ',name))))

(define-dylan-translator 
    emulator-symbol translate-emulator-symbol (form env)
  (destructuring-bind (_ name package) form
    (declare (ignore _))
    `(intern ',name ',package)))

(defun do-with-metering 
     (places thunk 
      &key (nested :exclusive)
           (threshold 0.01)
	   (key :percent-time)
	   (backtrace nil))
  (mon:with-monitoring places 
      (:nested nested :threshold threshold :key key :backtrace backtrace)
    (funcall thunk)))

;; eof
