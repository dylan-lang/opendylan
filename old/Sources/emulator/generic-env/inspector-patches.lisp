;; Patches to the inspector to "demangle" Dylan slot names

;; Author:  Tim McNerney

(in-package "SYSTEM")

(eval-when (load)
  (tools::ensure-inspector-loaded))

(defmethod object-attribute ((object standard-object) local-p)
  (loop with class = (class-of object)
        with names = (if local-p
			 (mapcar 'slot-definition-name 
                                 (class-direct-slots class))
                       (let* ((cpl (class-precedence-list class))
                              (all-names (mapcan #'(lambda (c)
                                                     (mapcar 'slot-definition-name
                                                             (class-direct-slots c)))
                                                 (if *inspect-less-specific-slots-first*
                                                     (reverse cpl)
                                                   cpl))))
			 (delete-duplicates all-names
                                            :from-end t)))
        for name in names
	collect (if (slot-boundp object name)
		    (slot-value object name)
		  (load-time-value (make-unbound 'slot)))
	into values
	finally
	(return 
	 (values ;; Demangle Dylan NAMES if there are any
		 (if (some #'(lambda (name) (null (symbol-package name))) names)
		     (mapcar 'demangle-dylan-slot-name names)
		     names)
		 values
		 #'(lambda (x attr index)
		     (declare (ignore index))
		     (slot-value x attr ))
		 #'(lambda (x attr index new-value)
		     (declare (ignore index))
		     (setf (slot-value x attr) new-value))
		 (let ((class-name (class-name class)))
		   (if local-p
		       (format nil "~A (local slots only)"
			       class-name)
		     class-name))))))

(defun demangle-dylan-slot-name (name)
  (if (and (null (symbol-package name))
	   (fboundp name)
	   (and (clos::generic-function-p (symbol-function name))))
      (clos::generic-function-name (symbol-function name))
      name))

#|
(defun set-$-and-$$-and-$$$ (self)
  (with-slots (history object) self
    (when (customized-value self 'bind-$)
      
      (setq $$$ (caadr history)
            $$ (caar history)
            $ object)

      ;; This isn't so generic is it...
      (setq dylan::$$$ (caadr history)
            dylan::$$ (caar history)
            dylan::$ object))))
|#

#|
;;; Hacks to CAPI inspector
(in-package "LISPWORKS-TOOLS")

(defun print-inspector-attribute (inspector attribute)
  (let ((attribute (demangle-dylan-slot-name attribute)))
    (with-interface-package (inspector)
      (if (and (interface-show-packages-p inspector)
	       (not (stringp attribute)))
	  (prin1-to-string attribute)
	  (princ-to-string attribute)))))
|#
