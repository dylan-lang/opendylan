;;----------------------------------------------------------------------
;;
;; Dylan inspector printing patch
;;
;; This patch robustifies LispWorks in the face of inspecting objects
;; with print methods that fail for whatever reason.
;;
;;----------------------------------------------------------------------
;; Aug 27 96, andrewa: initial version
;;----------------------------------------------------------------------

(in-package "SYSTEM")

(scm:note-private-patch 
 'dylan-inspector-printing-patch
 "andrewa for the Dylan group"
 "Fix up TTY and window inspector to handle objects that won't print") 

(defun print-broken-object (object stream)
  (handler-case
      (print-unreadable-object (object stream :type t :identity t)
        (format stream "*** failed to print ***"))
    (error (condition)
           (print-unreadable-object (object stream :identity t)
             (format stream "*** Object failed while printing ***")))))

(defun safe-print-object (object &optional (stream *standard-output*))
  (let ((string
         (handler-case
             (with-output-to-string (stream)
               (prin1 object stream))
           (error (condition) nil))))
    (if string
        (princ string stream)
      (print-broken-object object stream)))
  object)

(in-package "TOOLS")

(sys::without-warning-on-redefinition

(define-application-framework inspector
  :parent-classes (simple-application-framework-top-level lw-top-level) ; lw tool (Jason 3/2/92)
  :state
  ((object :initarg :object)
   (item :initarg :item :initform nil)
   (index-vector :initform (make-array 100 :adjustable t))
   (item-offset :initform nil)
   (sorted :initform nil)
   (package :initform :current)
   getter
   setter 
   (history :initarg :history :initform nil)
   (mode :initarg :mode :initform nil)
   (customization :initarg :customization 
		  :initform *inspector-default-customization*))
  :windows
  ((constraint-column
       col
     ((:conditional
       *inspector-listener*
       (interactive-stream
	listener
	nil
	:selectable t
	:scroll :yes
	:handle-commands :yes
	:buffer-modes '("Lisp" "Execute")))
      (display-pane display-pane nil :min-char-width 20)
      (inspector-list-pane
       lp
       nil
       :inspector top-level-window
       :interaction :single-selection
       :scroll :yes
       :notify `(inspector-item-selected ,top-level-window)
       :extend-callback `(inspector-item-inspect ,top-level-window)
       :item-data-fn #'(lambda (i) (apply 'values (cdr i)))
       :items nil
       ))
     :fixed-sizes (if *inspector-listener* '(nil t nil) '(t nil))
     :size-ratios (if *inspector-listener* '(1 nil 3) '(nil 3))
     ))
  :updaters (control-panel)
  :submenus-mutate? t
  :initform
  (progn
    (set-inspector-object top-level-window object
                          :selected-item nil)))

(define-inspector-command inspector-update (&key reset-scroll
                                                 default-sort)
  "Updates the inspector's display"
  (let* ((self top-level-window)
         (object (inspector-object self)))
    (with-slots (getter setter display-pane) self
      (multiple-value-bind
          (attributes values a-getter a-setter name)
          (inspector-values self)

        ;; If this is the first time we've seen this object, sort out
        ;; the default sorting
        (when default-sort
          (set-default-inspector-sorting self :no-update t))

        ;; Sort the attributes and values if sorting
        (initialize-vector self (length attributes))
        (when (inspector-sorted-p self)
          (multiple-value-bind
              (sorted-attributes sorted-values)
              (sort-inspector-values self attributes values)
            (setq attributes sorted-attributes)
            (setq values     sorted-values)))

        (set-message self
                     (format nil "~A~A"
                             (if (inspector-sorted-p self)
                                 "Sorted "
                               "")
                             (string name)))
        (setf getter a-getter
              setter a-setter)
        (set-contents (inspector-list-pane self)
		      (inspector-format-list self attributes values)
                      :reset-scroll reset-scroll
                      :reset-selected reset-scroll)
        (set-$-and-$$-and-$$$ self)
        (set-contents display-pane
                      (with-output-to-string (stream)
                        (system::safe-print-object object stream)))))))

(defun inspector-format-list (self attributes values)
  (with-slots (package) self
    (let* ((*package* (or (and (packagep package)
                               package)
                          *package*))
           (sys::*formatting-attributes* t)
           (table 
            (formatting-table-to-line-list
		()
	      (loop for a in attributes
		    for v in values
		    do
		    (formatting-row 
		     nil
		     (formatting-cell
		      nil
		      (format t "~:[~S~;~A~]"
			      (or (stringp a)
				  (and (eq package :none)
				       (symbol-but-not-keyword-p a)))
			      a))
		     (formatting-cell
		      nil
                      (system::safe-print-object v)))))))
      (declare (special sys::*formatting-attributes*))
      (loop for count from 0
	    for a in attributes
	    for v in values
	    for row in table
	    collect `(,row ,a ,v ,count)))))


)

(in-package "SYSTEM")

(sys::without-warning-on-redefinition

(defmethod describe-object (x stream)
  (setq *descr-continuation* nil)
  (let ((descr-stream-p (descr-stream-p stream))
	(*describe-object-label* (if (boundp '*describe-object-label*)
				     *describe-object-label*
				   *default-describe-object-label*)))
    (multiple-value-bind
	(attributes values getter setter name)
        (get-inspector-values x
			      (and descr-stream-p
				   (descr-stream-mode stream)))
      (declare (ignore getter))
      (when descr-stream-p
	(setf (descr-stream-attributes stream) attributes
	      (descr-stream-values stream) values
	      (descr-stream-setter stream) setter))
      (let* ((attributes attributes)
	     (max-attr-width 0)
	     (count 0)
	     (attribute-cells nil)
	     (too-many-attributes
	      (when attributes
		(loop
		 (let* ((attribute (pop attributes))
			(attribute-cell
			 (might-abbreviate (princ-to-string attribute)
					   *describe-attribute-width*))
			(width (length attribute-cell)))
		   (push attribute-cell attribute-cells)
		   (when (> width max-attr-width)
		     (setq max-attr-width width))
		   (when (null attributes)
		     (return nil))
		   (when (and *describe-length*
			      (= (incf count) *describe-length*))
		     (return attributes)))))))
	(setf attribute-cells (nreverse attribute-cells))
	(when name
          (system::safe-print-object x stream)
          (format stream " ~A"
                  (is-a (if (stringp name)
			   name
			 (prin1-to-string name)))));; nick 03.08.93
	(incf max-attr-width 5)
	(do ((attribute-cells attribute-cells (cdr attribute-cells))
	     (values values (cdr values)))
	    ((null attribute-cells)
	     (when too-many-attributes
	       (setq *descr-continuation*
		     (list* *describe-object-label*
			    too-many-attributes values))
	       (format stream
		       (if (and descr-stream-p
				(descr-stream-interactive-p stream))
			   " ........ (:dm for more)~%~%"
			 " ........~%~%"))))
          (format stream
		  *describe-object-label*
                  (car attribute-cells)
		  max-attr-width)
	  (if descr-stream-p
	      (describe (car values) stream)
            (system::safe-print-object (car values))))))))

(defmethod describe-object :around (x (stream descr-stream))
  (with-indentation
   (stream)
   (if (= *describe-level* (descr-stream-depth stream))
       (format-with-attributes stream "~a"
			       (with-output-to-string (stream)
				 (system::safe-print-object x stream)))
     (letf (((descr-stream-depth stream) 
	     (1+ (descr-stream-depth stream))))
	   (call-next-method)))))

)
