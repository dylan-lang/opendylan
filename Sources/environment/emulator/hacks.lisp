(in-package "CL-USER")

;;; Library and module handling

(defun find-direct-dylan-variable (name module)
  (let ((module
         (if (eq (dylan+dylan/internal::module-name module) 'dylan::dylan)
             (find-dylan-module "INTERNAL")
           module)))
    (when-let (symbol
	       (find-symbol (string-upcase name)
			    (dylan+dylan/internal::module-package module)))
      (when (or (boundp symbol)
                (fboundp symbol))
        symbol))))

(defun find-exported-dylan-variable (name module-info)
  (let* ((module-name (first module-info))
         (name (string-upcase name))
         (module-args (rest module-info))
         (exports (getf module-args :export))
         (module (find-dylan-module module-name)))
    (cond
     ((listp exports)
      (find name exports :test 'string=))
     ((find (find-symbol name "DYLAN")
	    (dylan+dylan/internal::export-spec module))
      (find-direct-dylan-variable name module)))))

(defun dylan-module-used-modules-info (module)
  (dylan+dylan/internal::use-spec module))

(defun find-dylan-variable (name module-name
                                 &key (default dylan::*dylan-canonical-false*))
  (let ((module (find-dylan-module module-name)))
    (or (unless (eq module dylan::*dylan-canonical-false*)
	  (find-direct-dylan-variable name module)
	  (loop for submodule-info in (dylan-module-used-modules-info module)
		for symbol = (find-exported-dylan-variable name submodule-info)
		when symbol
		return symbol))
	default)))

(defmethod dylan-variable-value ((name string) module-name)
  (when-let (symbol (find-dylan-variable name module-name :default nil))
    (dylan-variable-value symbol module-name)))

(defmethod dylan-variable-value ((symbol symbol) module-name)
  (or (find-class symbol nil)
      (when (boundp symbol)
        (symbol-value symbol))))

(defmethod dylan-variable-imported? (name module-name)
  (if (find-direct-dylan-variable name module-name)
      dylan::*dylan-canonical-false*
    dylan::*dylan-canonical-true*))

;;---*** Should implement this!
(defmethod dylan-variable-exported? (name module-name)
  dylan::*dylan-canonical-false*)

(defmethod dylan-variable-module ((symbol symbol))
  (let* ((package (symbol-package symbol))
         (package-name (package-name package)))
    (or (when-let (actual-name-position
                   (position #\/ package-name))
          (find-dylan-module (subseq package-name (1+ actual-name-position))))
        (find-dylan-module "DYLAN"))))

(defun map-over-dylan-modules (function)
  (loop for module in dylan+dylan/internal::$translator-modules
        do (funcall function module)))

(defun all-dylan-modules ()
  (let ((table dylan+dylan/internal::$translator-modules))
    (let ((values (dylan+dylan/internal::table-values table)))
      (mapcar #'(lambda (module)
                  (symbol-name (first module)))
              values))))

(defun find-dylan-module (name)
  (declare (special dylan+dylan/internal::$translator-modules))
  (or (etypecase name
        (string
         (let ((table dylan+dylan/internal::$translator-modules)
               (symbol (intern (string-upcase name) "DYLAN")))
           (let ((hash-table (dylan+dylan/internal::table-table table)))
             (first (gethash symbol hash-table)))))
        (symbol
         (find-dylan-module (symbol-name name)))
        (dylan::<translator-module>
         name))
      dylan::*dylan-canonical-false*))

(defun do-dylan-module-exported-variables (function module)
  (let ((exported-symbols (dylan+dylan/internal::export-spec module)))
    (loop for name in exported-symbols
          for symbol = (find-direct-dylan-variable name module)
          when symbol
          do (funcall function symbol))
    (loop for (used-module . options) in (dylan-module-used-modules-info module)
          for exported-symbols = (getf options :export)
	  do 
          (if (eq exported-symbols 'dylan::all)
              (do-dylan-module-exported-variables function used-module)
            (loop for name in exported-symbols
                  for symbol = (find-direct-dylan-variable name used-module)
                  when (dylan-symbol-p symbol)
                  do (funcall function symbol))))))

(defmethod dylan-symbol-p ((symbol symbol))
  (and (string= (package-name (symbol-package symbol)) "DYLAN")
       (not (boundp symbol))))

(defmethod dylan-symbol? ((symbol symbol))
  (or (dylan-symbol-p symbol)
      dylan::*dylan-canonical-false*))

(defmethod dylan-constant? ((symbol symbol))
  (let ((name (symbol-name symbol)))
    (and (> (length name) 0)
         (eq (elt name 0) #\$))))

(defun do-dylan-module-variables
       (function module 
                 &key
                 (internal? t)
                 (inherited? dylan::*dylan-canonical-false*))
  (let ((package (dylan+dylan/internal::module-package module)))
    (unless (eq internal? dylan::*dylan-canonical-false*)
      (do-symbols (symbol package)
        (unless (dylan-symbol-p symbol)
          (funcall function symbol))))
    (unless (eq inherited? dylan::*dylan-canonical-false*)
      (loop for (module . options) in (dylan-module-used-modules-info module)
            do (do-dylan-module-exported-variables function module)))))

(defmethod dylan-module? (module)
  dylan::*dylan-canonical-false*)

(defmethod dylan-module? ((module dylan::<translator-module>))
  dylan::*dylan-canonical-true*)

(defmethod dylan-module-name ((module dylan::<translator-module>))
  (string-downcase (symbol-name (dylan+dylan/internal::module-name module))))

(defmethod dylan-module-library ((module dylan::<translator-module>))
  (cond
   ((eq module (find-dylan-module "dylan"))
    (find-dylan-library "dylan"))
   ((eq module (find-dylan-module "dylan-user"))
    (find-dylan-library "dylan-user"))
   (t
    (let ((module-name (string-upcase (dylan-module-name module))))
      (maphash #'(lambda (library-name library)
		   (when-let (exports (getf library :exports))
		     (when (find module-name exports :test 'string=)
		       (return-from dylan-module-library
			 (find-dylan-library (symbol-name library-name))))))
	       dylan::*loaded-libraries*)))))

(defmethod dylan-module-used-modules ((module dylan::<translator-module>))
  (mapcar 'first (dylan-module-used-modules-info module)))

(defmethod dylan-module-client-modules ((module dylan::<translator-module>))
  (let ((table dylan+dylan/internal::$translator-modules))
    (let ((keys (dylan+dylan/internal::key-sequence table)))
      (loop for key in keys
            for temp-module = (dylan+dylan/internal::element table key)
            when (member module (dylan-module-used-modules temp-module))
            collect temp-module))))


;;; Class handling

(defparameter *dylan-classes-for-emulator-classes*
  '((character                  . dylan::<character>)
    (standard-object            . dylan::<object>)
    (clos::abstract-string      . dylan::<string>)
    (cons                       . dylan::<pair>)
    (class                      . dylan::<class>)
    (integer                    . dylan::<integer>)
    (sequence                   . dylan::<sequence>)
    (clos::abstract-vector      . dylan::<vector>)
    (standard-generic-function  . dylan::<generic-function>)
    (dylan-project              . dylan::<project>)
    (dylan-library              . dylan::<library>)
    (dylan::<translator-module> . dylan::<module>)))

(defmethod dylan-class? (object)
  dylan::*dylan-canonical-false*)

(defmethod dylan-class? ((class class))
  dylan::*dylan-canonical-true*)

(defmethod dylan-class-name ((class class))
  (dylan-class-name (class-name class)))

(defmethod dylan-class-name ((class symbol))
  (or (when-let (match (find class *dylan-classes-for-emulator-classes*
                             :key 'first))
        (rest match))
      class))

(defun dylan-object-class (object)
  (let ((class (class-of object)))
    (if (subtypep class 'standard-class)
        (find-class 'class)
      class)))

(defmethod dylan-slot? (slot)
  dylan::*dylan-canonical-false*)

(defmethod dylan-slot? ((slot slot-definition))
  dylan::*dylan-canonical-true*)

(defun dylan-slot-contents (slotd)
  (values (slot-definition-name slotd)
          (slot-definition-type slotd)
          (slot-definition-initargs slotd)
          (slot-definition-initform slotd)))

(defmethod dylan-class-initargs ((class class))
  (remove-duplicates
   (append (loop for slot in (clos:class-slots class)
                 appending (clos:slot-definition-initargs slot))
           (initialize-instance-args class))))

(defmethod initialize-instance-args ((class class))
  (remove-duplicates
   (mapcar #'(lambda (x)
               (intern (let ((target (if (consp x)
                                         (let ((keyword (first x)))
                                           (if (consp keyword)
                                               (first keyword)
                                             keyword))
                                       x)))
                         (if (symbolp target)
                             (symbol-name target)
                           target))
                       'keyword))
           (loop for method in (compute-applicable-methods
		                (fdefinition 'initialize-instance)
		                (list (clos:class-prototype class)))
                 for lambda-list = (clos:method-lambda-list method)
                 for keys = (rest (member '&key lambda-list))
                 appending (subseq keys 0 (position '&allow-other-keys keys))))))


;;; Range handling

(defmethod dylan-range-contents ((range dylan::<range>))
  (let ((first (dylan+dylan/internal::element range 0))
        (size (dylan+dylan/internal::range-size range)))
    (values size
            first
            (dylan+dylan/internal::range-last range)
            (if (or (eq size dylan::*dylan-canonical-false*)
                    (> size 1))
                (- (dylan+dylan/internal::element range 1) first)
              0))))


;;; Emulator printing

(defmethod print-emulator-object (object)
  (let ((*print-length* 10)
        (*print-level* 10))
    (format nil "~S" object)))

(defmethod print-emulator-object ((module dylan::<translator-module>))
  (symbol-name (dylan+dylan/internal::module-name module)))

(defmethod print-emulator-object ((class class))
  (string-downcase (symbol-name (dylan-class-name class))))

(defmethod print-emulator-object ((symbol symbol))
  (string-downcase (symbol-name symbol)))

(defmethod dylan-symbol-name ((symbol symbol))
  (string-downcase (symbol-name symbol)))


;;; Object handling

(defmethod object-slot-contents (object &key local?)
  (multiple-value-bind
      (names values)
      (get-inspector-values object (when local? 'system::local-slots))
    (values (mapcar #'(lambda (object)
			(if (symbolp object)
			    (symbol-function object)
			  object))
		    names)
	    values)))

(defmethod object-slot-value (object (name string))
  (multiple-value-bind
      (names values)
      (get-inspector-values object (when local? 'system::local-slots))
    (let (found-value
	  foundp)
      (loop for name in names
	    for value in values
	    for slot-name = (when (symbolp name) (symbol-name name))
	    when (string= (string-upcase name) slot-name)
	    do 
	    (setq found-value value)
	    (setq foundp t))
      (if foundp
	  (values found-value dylan::*dylan-canonical-true*)
	(values dylan::*dylan-canonical-false* 
		dylan::*dylan-canonical-false*)))))

(defmethod dylan-boolean-value ((object dylan::dylan-boolean))
  object)

(defmethod dylan-function-name ((slotd slot-definition))
  (slot-definition-name slotd))

(defmethod dylan-function-name (object)
  dylan::*dylan-canonical-false*)


;;; Fake library handling

(defclass dylan-library ()
  ((name :accessor dylan-library-name :initarg :name)))

(defun make-dylan-user-library ()
  (make-instance 'dylan-library :name "dylan-user"))

(defvar *libraries* nil)

(pushnew (make-dylan-user-library) *libraries*)

(defun all-dylan-libraries ()
  (let (results)
    (maphash #'(lambda (key value)
                 (push (symbol-name key) results))
             dylan::*loaded-libraries*)
    results))

(defmethod library-exists? ((name string))
  (let ((name (string-upcase name)))
    (or (gethash (intern (string-upcase name) "DYLAN")
                 dylan::*loaded-libraries*)
        (dylan::find-library-info (intern name "KEYWORD")))))

(defmethod find-dylan-library ((name string))
  (let ((name (string-downcase name)))
    (or (find name *libraries* :key 'dylan-library-name :test 'string-equal)
        (when (library-exists? name)
          (let ((library (make-instance 'dylan-library :name name)))
	    (push library *libraries*)
	    library))
        dylan::*dylan-canonical-false*)))

(defmethod find-dylan-library-arguments ((name string))
  (gethash (find-symbol (string-upcase name) "DYLAN")
           dylan::*loaded-libraries*))

(defmethod find-dylan-library-arguments ((library dylan-library))
  (let ((name (dylan-library-name library)))
    (if (string-equal name "DYLAN")
        '(:exports (dylan::dylan))
      (find-dylan-library-arguments (dylan-library-name library)))))

(defmethod dylan-library-used-libraries ((name string))
  (when-let (arguments (find-dylan-library-arguments name))
    (when-let (libraries (getf arguments :uses))
      (mapcar #'(lambda (library-info)
                  (find-dylan-library (symbol-name (first library-info))))
              libraries))))

(defmethod dylan-library-used-libraries ((library dylan-library))
  (dylan-library-used-libraries (dylan-library-name library)))

(defmethod dylan-library-client-libraries ((library dylan-library))
  (let ((name (intern (string-upcase (dylan-library-name library)) "DYLAN"))
        results)
    (maphash #'(lambda (key value)
                 (when (member name (getf value :uses) :key 'first)
                   (when-let (library
                              (find-dylan-library (symbol-name key)))
                     (push library results))))
             dylan::*loaded-libraries*)
    results))

(defmethod dylan-module-exported? ((library dylan-library) 
                                   (module dylan::<translator-module>))
  (when-let (arguments (find-dylan-library-arguments library))
    (let ((exports (getf arguments :exports)))
      (or (eq exports 'dylan::all)
          (member module exports)))))

(defmethod do-dylan-library-exported-modules
           ((function function) (library dylan-library)
            &key (imported t))
  (when-let (arguments (find-dylan-library-arguments library))
    (cond
     (imported
      (let ((uses (getf arguments :uses)))
        (loop for (name . rest) in uses
              for used-library = (find-dylan-library (symbol-name name))
              for exports = (getf rest :export)
              do (if (eq exports 'dylan::all)
                     (do-dylan-library-exported-modules function used-library)
                   (loop for name in exports
                         for module = (find-dylan-module name)
                         do (funcall function module)))))
      (do-dylan-library-exported-modules function library :imported nil))
     (t
      (let ((exports (getf arguments :exports)))
        (unless (eq exports 'dylan::all)
          (loop for name in exports
                for module = (find-dylan-module name)
                do (funcall function module))))))))

;; Unfortunately we can't find out anything about modules that aren't exported,
;; so this is the best we can do!
(defmethod do-dylan-library-modules
           ((function function) (library dylan-library)
            &key (imported t))
  (when-let (arguments (find-dylan-library-arguments library))
    (cond
     (imported
      (let ((uses (getf arguments :uses)))
        (loop for (name . rest) in uses
              for used-library = (find-dylan-library (symbol-name name))
              do (do-dylan-library-exported-modules function used-library)))
      (do-dylan-library-modules function library :imported nil))
     (t
      (let ((exports (getf arguments :exports)))
        (unless (eq exports 'dylan::all)
          (loop for name in exports
                for module = (find-dylan-module name)
                do (funcall function module))))))))


;;; Fake project handling

(defclass dylan-project ()
  ((name :accessor dylan-project-name :initarg :name)
   (library :accessor dylan-project-library :initarg :library :initform nil)
   (loaded-files :accessor dylan-project-loaded-files :initform nil)))

(defvar *projects* nil)

(defmethod find-dylan-project ((name string))
  (let ((name (string-downcase name)))
    (or (find name *projects* :key 'dylan-project-name :test 'string-equal)
        (when-let (table (dylan::find-library-info (intern name 'keyword)))
	  (let ((project
		 (make-instance 'dylan-project
                                :library name
				:name name)))
	    (push project *projects*)
	    project))
        dylan::*dylan-canonical-false*)))

(defmethod find-dylan-project ((project dylan-project))
  project)

(defmethod file-truename ((filename string))
  (namestring (truename filename)))

(defmethod dylan-project-directory ((project dylan-project))
  (let* ((name (intern (dylan-project-name project) 'keyword))
         (lid-file (dylan::load-entry name)))
    (namestring (pathname-location lid-file))))

(defmethod dylan-project-libraries ((project dylan-project))
  (when-let (library
             (find-dylan-library (dylan-project-name project)))
    (vector library)))

(defmethod dylan-project-files ((project dylan-project))
  (let* ((name (intern (dylan-project-name project) 'keyword))
         (table (dylan::find-library-info name)))
    (dylan::lid-value :files table)))

(defmethod dylan-project-contents ((project dylan-project))
  (let* ((name (intern (dylan-project-name project) 'keyword))
         (table (dylan::find-library-info name))
         (files (dylan::lid-value :files table))
         (library (dylan-project-library project)))
    (values (when library
              (vector library))
            files)))

;; Taken from ~ext/editor/grep-browser.lisp
(defun editor::goto-line (buffer line)
  (editor::with-point ((po (editor::buffer-%start buffer)))
    (if (editor::line-offset po line)
        (let ((point (editor::buffer-point buffer)))
	  (editor::move-point point po))
      (editor::editor-error "Line Number ~a not exists in current buffer" line))))

(defmethod dylan-project-file-truename ((project dylan-project) name &key type)
  (let ((directory (dylan-project-directory project))
        (pathname (pathname name)))
      (merge-pathnames (make-pathname :directory (pathname-directory pathname)
                                      :name (pathname-name pathname)
                                      :type (or type
                                                (pathname-type pathname)
                                                "dylan"))
                       directory)))
  
(defmethod edit-source-file (file line)
  (let* ((truename (truename file))
         (buffer (editor:find-file-command nil truename)))
    (editor:goto-buffer buffer (editor:current-window))
    (when line
      (editor::goto-line buffer line))
    (when-let (window (first (editor:buffer-windows buffer)))
      (editor:update-buffer-window window))
    (if line
        (editor::quiet-message "Editing ~A at line ~D" file line)
      (editor::quiet-message "Editing ~A" file))))

(defmethod compile-dylan-project-file ((project dylan-project) file)
  (generic-env:generic-compile file)
  (setf (dylan-project-loaded-files project)
        (remove file (dylan-project-loaded-files project) :test 'string=)))

(defmethod load-dylan-project-file ((project dylan-project) file)
  (load (compiler::compile-file-pathname file))
  (pushnew file (dylan-project-loaded-files project) :test 'string=))

(defmethod dylan-project-file-loaded? ((project dylan-project) file)
  (or (member file (dylan-project-loaded-files project)
              :test 'string=)
      dylan::*dylan-canonical-false*))

(defmethod dylan-project-file-compiled? (file)
  (let* ((src-path file)
         (obj-path (compiler::compile-file-pathname src-path))
         (src-date (file-write-date src-path))
	 (obj-date (file-write-date obj-path)))
    (or (and src-date obj-date 
             (< src-date obj-date))
        dylan::*dylan-canonical-false*)))


;;; Method and function handling

(defmethod dylan-method-specializers ((method method))
  (substitute (find-class 'standard-object) (find-class t)
              (method-specializers method)))

(defmethod dylan-singleton-value ((list list))
  (and (eq (first list) 'eql)
       (second list)))

(defun lambda-list-keys (lambda-list)
  (when-let (keys-pos (position '&key lambda-list))
    (let* ((rest-pos (or (position '&rest lambda-list) (length lambda-list)))
           (rest-after-keys-p (> rest-pos keys-pos))
           (end-pos (if rest-after-keys-p rest-pos)))
      (subseq lambda-list (1+ keys-pos) end-pos))))

(defmethod dylan-function-parameters ((function function))
  (let* ((specializers (dylan-method-specializers function))
         (lambda-list (function-lambda-list function)))
    (values (if lambda-list
                (mapcar #'(lambda (type name)
                            (cons name type))
                      specializers lambda-list)
              (mapcar #'(lambda (type)
                          (cons "unknown" type))
                      specializers))
            (let ((pos (position '&rest lambda-list)))
              (if pos
                  (cons (nth (1+ pos) lambda-list) (find-class 'sequence))
		dylan::*dylan-canonical-false*))
            (lambda-list-keys lambda-list)
            dylan::*dylan-canonical-false*
            '()
            (cons "objects" (find-class 'sequence)))))

;;--- Sometimes Lisp functions appear in inspectors etc. so we
;;--- need to pretend that they are methods.
(defmethod dylan-method-specializers ((function function))
  (let* ((lambda-list (function-lambda-list function))
         (size (length lambda-list))
         (required-arg-count
          (min (or (position '&rest lambda-list) size)
               (or (position '&key lambda-list) size)
               (or (position '&optional lambda-list) size))))
    (loop for i below required-arg-count
          collect (find-class 'standard-object))))

(defmethod dylan-function-name ((function function))
  'unknown)

(defmethod dylan-function-name ((function generic-function))
  (generic-function-name function))

(defmethod dylan-function-name ((method method))
  (dylan-function-name (method-generic-function method)))

(defmethod dylan-generic-function-methods ((function generic-function))
  (generic-function-methods function))

(defmethod dylan-generic-function-methods ((function function))
  nil)


;;; Debugger hacks

(defun stack-frame-function (frame)
  (dbg::get-frame-functions frame))

(defun function-name (function)
  (sys::function-name function))

(defun stack-frame-next-call-frame (frame &optional (test #'true))
  (loop for f = (dbg::frame-next frame) then (dbg::frame-next f)
        when (or (null f)
                 (and (typep f 'dbg::call-frame)
                      (funcall test f)))
	return f))

(defun stack-frame-previous-call-frame (frame &optional (test #'true))
  (loop for f = (dbg::frame-prev frame) then (dbg::frame-prev f)
        when (or (null f)
                 (and (typep f 'dbg::call-frame)
                      (funcall test f)))
          return f))

(defun stack-frame-previous-frame (frame)
  (stack-frame-next-call-frame frame))

(defun stack-frame-previous-active-frame (frame)
  (stack-frame-next-call-frame frame))

(defun stack-frame-previous-interesting-frame (frame)
  (stack-frame-next-call-frame frame #'dbg::interesting-frame-p))

(defun stack-frame-next-active-frame (frame)
  (stack-frame-previous-call-frame frame))

(defun stack-frame-interesting-p (frame)
  (dbg::interesting-frame-p frame))

(defun stack-frame-active-p (frame)
  t)

(defun stack-frame-value-disposition (frame) 
  ;;--- Until we can figure out how to get a real disposition
  :multiple)

(defun stack-frame-relative-exit-pc (frame)
  (dbg::call-frame-call-address frame))

(defun eval-in-frame-environment (expression frame)
  (if (dylan-frame-p frame)
      (multiple-value-list
        (dbg::dbg-eval
          (dylan::translate-dylan-expr
            expression
            (generic-env::lisp->xlator-env
              (dbg::call-frame-environment-for-eval frame)))
          frame))
      (multiple-value-list
        (dbg::dbg-eval expression frame))))

(defun dylan-frame-p (frame)
  (dylan-function-p (stack-frame-function frame)))

(defun dylan-function-p (function)
  ;; In the emulator, Dylan modules use a package-name convention that
  ;; we can use to determine whether a function is a Dylan function
  (let ((function-name (function-name function)))
    (when (listp function-name)
      (setq function-name (second function-name)))
    (and (symbolp function-name)
         (string-equal (package-name (symbol-package function-name))
                       "DYLAN+DYLAN" :end1 11))))

(defun stack-frame-argument-value (frame n &optional (error-p t))
  (declare (values value location))
  (multiple-value-bind (args-and-locals n-args)
      (lispworks-stack-frame-info frame)
    (when (< n n-args)
      ;; No locatives here, just return index as the location
      (values (nth 3 (nth n args-and-locals)) n))))

(defun (setf stack-frame-argument-value) (value frame n)
  (multiple-value-bind (args-and-locals n-args)
      (lispworks-stack-frame-info frame)
    (when (< n n-args)
      (let* ((elt (nth n args-and-locals))
             (var-spec (nth 2 elt)))
        (dbg::dbg-set-value-of var-spec value)
        value))))

(defun stack-frame-local-value (frame n &optional (error-p t))
  (declare (values value location))
  (multiple-value-bind (args-and-locals n-args)
      (lispworks-stack-frame-info frame)
    (declare (ignore n-args))
    (when (< n (length args-and-locals))
      ;; No locatives here, just return index as the location
      (values (nth 3 (nth n args-and-locals)) n))))

(defun (setf stack-frame-local-value) (value frame n)
  (multiple-value-bind (args-and-locals n-args)
      (lispworks-stack-frame-info frame)
    (declare (ignore n-args))
    (when (< n (length args-and-locals))
      (let* ((elt (nth n args-and-locals))
             (var-spec (nth 2 elt)))
        (dbg::dbg-set-value-of var-spec value)
        value))))

(defun stack-frame-number-of-locals (frame)
  (multiple-value-bind (args-and-locals n-args)
      (lispworks-stack-frame-info frame)
    (- (length args-and-locals) n-args)))

(defun stack-frame-rest-arg-value (frame)
  (declare (values rest-arg rest-arg-p))
  (multiple-value-bind (args-and-locals n-args)
      (lispworks-stack-frame-info frame)
    (loop for i below n-args
	  for (nil kind nil value) in args-and-locals
	  when (eql kind ':rest)
	    return (values value t))))

(defun stack-frame-argument-name (frame n)
  (multiple-value-bind (args-and-locals n-args)
      (lispworks-stack-frame-info frame)
    (and (< n n-args)
	 (nth 0 (nth n args-and-locals)))))

(defun stack-frame-rest-arg-name (frame)
  (multiple-value-bind (args-and-locals n-args)
      (lispworks-stack-frame-info frame)
    (loop for (name kind nil nil) in args-and-locals
	  repeat n-args
	  when (eql kind ':rest)
	    return name)))

(defun stack-frame-local-name (frame n &optional pc)
  (multiple-value-bind (args-and-locals n-args)
      (lispworks-stack-frame-info frame)
    (declare (ignore n-args))
    (nth 0 (nth n args-and-locals))))

;; This utility is used by all kinds of things that want to extract
;; information from stack frames. 
(defun lispworks-stack-frame-info (frame)
  ;; ARGS-AND-LOCALS is of the form (name kind varspec value)
  (declare (values args-and-locals-alist n-args specials-alist))
  (let* ((lambda-list (aref (dbg::call-frame-constants frame) 3))
	 (var-specs (dbg::call-frame-vars-in-scope frame))
         (all-vars nil)
	 (args-and-locals nil)
	 (n-args 0))
    (dolist (c var-specs)
      (let* ((name (dbg::name-from-var-spec c frame))
	     (value (dbg::value-from-var-spec c frame)))
	(push (list name c value) all-vars)))
    ;; Disgusting special case for macros.  Apparently, when we're
    ;; looking at the stack frame of a macro, and we get to this
    ;; point, ALL-VARS doesn't contain varspecs with the real 
    ;; names, but rather some special stuff which looks 
    ;; suspiciously like &WHOLE and &ENV.  Try to get past that
    ;; by doing arglist matching by hand.
    (when (eq (caar all-vars) 'definition::%%macroarg%%)
      (setq lambda-list '(definition::%%macroarg%% &rest environment)))
    ;; LW seems to sometimes hide extra cruft in lambda lists, marked
    ;; with preceding keyword-oid things.  Strip them off here.
    (loop while (keywordp (car lambda-list))
          do (setq lambda-list (cddr lambda-list)))
    ;; At this point, ALL-VARS contains an alist of all the locally
    ;; accessable vars, in order of definition (I think).  Map over
    ;; the lambda list, extracting the variables that are defined there.
    ;; Walk the rest, extracting all the ones that aren't special.
    (flet ((special-variable-p (symbol)
             (boundp symbol)))
      (let ((kind ':required))
	(dolist (name lambda-list)
	  (when (member name '(&optional &rest &key &allow-other-keys))
	    (setq kind ':optional))
	  (unless (member name '(&optional &rest &key &allow-other-keys
				 &aux &whole &environment &body))
	    (let ((elt (assoc name all-vars)))
	      (when (and (eql kind ':required)
			 ;;--- Kludge?  LW lambda lists seem to require this
			 (not (or (keywordp name)
				  (member name '(t nil)))))
		(assert (not (null elt)) ()
			"Argument ~A not found" name))
	      (when elt
		(unless (special-variable-p name)
		  ;; Remove non-specials from all-vars
		  (setq all-vars (delete elt all-vars)))
		;; Add it to args-and-locals
		(setq args-and-locals
		      (nconc args-and-locals
			     (list (list (first elt) kind (second elt) (third
elt)))))
		(incf n-args))))))
      ;; Got the args; anything else non-special is a local
      (loop for elt in all-vars
	    for (arg spec val) = elt
	    unless (special-variable-p arg)
	    do (setq all-vars (delete elt all-vars)
		     args-and-locals (nconc args-and-locals
					    (list (list arg :local spec
val))))))
    (values args-and-locals n-args all-vars)))

(defun return-from-stack-frame (frame values)
  (dbg::exiting-debugger)
  (let ((offset
         (sys:raw-int-fixnum
	   (sys:raw-- (dbg::stack-address
		        (dbg::frame-start (dbg::frame-next frame)) 0)
		      sys:*%current-stack)))
	(preserved-regs-stuff
	 #-harp::wsparc (nconc (multiple-value-list
				 (dbg::find-preserved-registers frame))
			       (list
(dbg::call-frame-preserved-registers-count frame)))))
    (sys::throw-to-address (list offset 'dbg::in-in-return-from-frame
				 frame
				 values
				 preserved-regs-stuff)
			   lw::*debugger-execute-unwind-protect*)))

(defun restart-stack-frame (frame new-args)
  (when new-args
    (loop for i upfrom 0
	  for arg in new-args
	  do (setf (stack-frame-argument-value frame i) arg)))
  (setf (dbg::debugger-stack-current-frame dbg::*debugger-stack*) frame)
  (dbg::restart-frame frame :same-args t))

(defun set-trap-on-exit (frame trap-p)
  (if trap-p
      (dbg::in-dbg-trap-on-exit frame)
      ;;--- No way to do this right now in LispWorks
      (format *query-io* "~%Clear trap-on-exit not implemented")))

 ;;; Debugger and notifier entry hacks

(defvar *dylan-notifier-entry-function* nil)
(defvar *dylan-debugger-entry-function* nil)

(defun install-notifier-function (function)
  (when (eq function dylan::*dylan-canonical-false*)
    (setq function nil))
  (setf *dylan-notifier-entry-function* function))

(defun install-debugger-function (function)
  (when (eq function dylan::*dylan-canonical-false*)
    (setq function nil))
  (setf *dylan-debugger-entry-function* function))

(defadvice
    (tools::notify-user dylan-advice :around)
    (&rest args &key condition restarts &allow-other-keys)
  (if *dylan-notifier-entry-function*
      (funcall *dylan-notifier-entry-function*
               condition restarts
               :abort-restart (find 'abort restarts :key 'restart-name)
               :continue-restart (find 'continue restarts :key 'restart-name))
    (apply #'call-next-advice args)))

(defadvice
    (tools::full-wmdebug dylan-advice :around)
    ()
  (if *dylan-debugger-entry-function*
      (funcall *dylan-debugger-entry-function*)
    (call-next-advice)))

 ;;; Definition handling

(defmethod object-variable (object)
  (multiple-value-bind
      (name module)
      (dylan+dylan/variable-search::locate-variable object)
    (find-direct-dylan-variable name
                                (find-dylan-module (symbol-name module)))))

(defmethod dspec-matches-object? (object dspec)
  nil)

(defmethod dspec-matches-object? ((object class) dspec)
  (and (listp dspec)
       (eq (first dspec) 'dylan::define-class)
       (string= (second dspec) (class-name object))))

(defmethod dspec-matches-object? ((object generic-function) dspec)
  (and (listp dspec)
       (eq (first dspec) 'dylan::define-generic)
       (string= (second dspec) (generic-function-name object))))

(defmethod dspec-matches-object? ((object method) dspec)
  (when (and (listp dspec)
             (eq (first dspec) 'dylan::define-method)
             (string= (second dspec)
                      (generic-function-name
                       (method-generic-function object))))
    (let* ((arguments (rest (rest dspec)))
           (method-type
	    (find (first arguments) '(:before :after :around)))
	   (lambda-list
	    (if method-type
	        (second arguments)
	      (first arguments))))
      (method-matches-dspec object method-type lambda-list))))

(defmethod compare-specializers (specializer1 specializer2)
  (eq (dylan-class-name specializer1)
      (if (or (eq specializer2 'dylan::<object>)
	      ;; It is a type-union, so mapped to <object>
	      (and (symbolp specializer2)
	           (not (find-class specializer2 nil))
                   (not (member specializer2 *dylan-classes-for-emulator-classes*
                                :key 'rest))))
          t
        specializer2)))

(defmethod compare-specializers ((specializer1 list) (specializer2 list))
  (and (eq (first specializer1) 'eql)
       (eq (first specializer2) 'dylan::singleton)
       (eq (second specializer1) (second (second specializer2)))))

(defmethod method-matches-dspec ((method method) qualifier specializers)
  (let ((method-qualifier (first (method-qualifiers method)))
        (method-specializers (method-specializers method)))
    #+comment
    (format t "~&Comparing ~S and ~S"
            specializers method-specializers)
    (and (eq method-qualifier qualifier)
         (every #'(lambda (method-specializer specializer)
                    (compare-specializers
                     (if (typep method-specializer 'class)
			 (class-name method-specializer)
		       method-specializer)
                     specializer))
                method-specializers specializers))))

(defmethod find-matching-source-location (object sources)
  (when-let (match
             (find-if #'(lambda (dspec)
			  (dspec-matches-object? object dspec))
                      sources
                      :key 'first))
    (second match)))

(defmethod object-symbol-name ((object standard-object))
  (object-variable object))

(defmethod object-symbol-name ((object symbol))
  object)

(defmethod object-symbol-name ((object method))
  (object-symbol-name (method-generic-function object)))

(defmethod object-source-location ((object standard-object))
  (when-let (name (object-symbol-name object))
    (when-let (sources (compiler::find-source-file name))
      (when-let (source (find-matching-source-location object sources))
        (namestring source)))))

(defmethod object-source-location ((library dylan-library))
  nil)

(defmethod dylan-object-source-location (object)
  (let ((location (object-source-location object)))
    (if location
        (multiple-value-bind
            (start end)
            (dylan-object-source-position object location)
          (values location
                  (or start dylan::*dylan-canonical-false*)
                  (or end dylan::*dylan-canonical-false*)))
      (values dylan::*dylan-canonical-false*
              dylan::*dylan-canonical-false*
              dylan::*dylan-canonical-false*))))


;;; Editor buffer sectioning

(defmethod buffer-line-for-offset (buffer offset)
  (let ((point (editor::copy-point (editor::buffer-point buffer))))
    (editor::buffer-start point)
    (editor::character-offset point offset)
    (editor::count-lines (editor::buffer-%start buffer) point)))
  
(defmethod dylan-object-source-position (object
                                         (group editor::definition-group))
  (let ((definitions (editor::definition-group-definitions group)))
    (when-let (definition
               (loop for definition in definitions
                     for dspec = (editor::definition-dspec definition)
	             when (dspec-matches-object? object dspec)
	             return definition))
      (let ((start (editor::definition-start definition))
            (end (editor::definition-end definition))
            (buffer (editor::definition-buffer definition)))
        (values (buffer-line-for-offset buffer start)
                (buffer-line-for-offset buffer end))))))

(defmethod dylan-object-source-position (object (file string))
  (multiple-value-bind
      (buffer new)
      (editor::find-file-buffer (pathname file))
    (unwind-protect
        (when-let (definition-group
                   (and buffer (editor::section-buffer :buffer buffer)))
          (dylan-object-source-position object definition-group))
      (when new
        (editor::delete-buffer buffer)))))


;;; Definition handling

(defmethod coerce-object-to-dspec ((object function))
  (object-variable object))

(defmethod coerce-object-to-dspec ((object method))
  `(dylan::define-method
    ,(coerce-object-to-dspec (method-generic-function object))
    ,@(method-qualifiers object)
    ,(mapcar 'class-name (method-specializers object))))

(defmethod coerce-object-to-dspec (object)
  nil)

(defmethod coerce-object-to-dspec ((object symbol))
  object)

(defmethod coerce-dspec-to-object ((object symbol))
  (when (fboundp object)
    (symbol-function object)))

(defmethod coerce-dspec-to-object (object)
  (error "Unable to coerce dspec ~S into an object" object))

(defmethod coerce-list-dspec-to-object 
           ((object (eql 'dylan::define-method))
            &rest arguments)
  (destructuring-bind
      (name . arguments)
      arguments
    (when-let (function (coerce-dspec-to-object name))
      (let* ((method-type
              (find (first arguments) '(:before :after :around)))
             (lambda-list
              (if method-type
                  (second arguments)
                (first arguments))))
        (loop for method in (generic-function-methods function)
              when (method-matches-dspec method method-type lambda-list)
              return method)))))

(defmethod coerce-dspec-to-object ((object list))
  (or (apply 'coerce-list-dspec-to-object object)
      (call-next-method)))

;;--- Hack out some of the more obviously internal definitions
(defvar *dspecs-to-ignore* '(add-method))

(defmethod do-dylan-definitions-used-definitions (function object)
  (when-let (dspec (coerce-object-to-dspec object))
    (loop for dspec in (sys::calls-who dspec)
          for object = (coerce-dspec-to-object dspec)
          when (and object
                    (not (member dspec *dspecs-to-ignore*))
                    (or (not (typep object 'function))
                        (typep object 'generic-function)))
          do (funcall function object))))

(defmethod do-dylan-definitions-used-definitions (function 
                                                  (object generic-function))
  (call-next-method)
  (map nil function (generic-function-methods object)))

(defmethod do-dylan-definitions-client-definitions (function object)
  (when-let (dspec (coerce-object-to-dspec object))
    (loop for dspec in (sys::who-calls dspec)
          for object = (coerce-dspec-to-object dspec)
          when (and object
                    (not (member dspec *dspecs-to-ignore*))
                    (or (not (typep object 'function))
                        (typep object 'generic-function)
			(typep object 'method)))
          do (funcall function object))))

(defmethod do-dylan-definitions-client-definitions (function (object method))
  (call-next-method)
  (funcall function (method-generic-function object)))


;;; Compiler warning handling

(defvar *pathname-warnings* (make-hash-table :test 'equal))

(defclass dylan-warning ()
  ((pathname :accessor dylan-warning-pathname :initarg :pathname)
   (dspec :accessor dylan-warning-dspec :initarg :dspec)
   (warning :accessor dylan-warning-warning :initarg :warning)))

(defmethod dylan-warning-definition ((warning dylan-warning))
  (coerce-dspec-to-object (dylan-warning-dspec warning)))

(defmethod dylan-warning-message ((warning dylan-warning))
  (princ-to-string (dylan-warning-warning warning)))

(defmethod make-pathname-warning (pathname dspec warning)
  (let ((old-warnings (gethash pathname *pathname-warnings*)))
    (or (find warning old-warnings :key 'dylan-warning-warning)
        (let ((new-warning
               (make-instance 'dylan-warning
                              :warning warning
                              :dspec dspec
                              :pathname pathname)))
          (push new-warning (gethash pathname *pathname-warnings*))
          new-warning))))

(defmethod do-dylan-compiler-warnings (function object)
  nil)

(defmethod do-dylan-compiler-warnings (function (pathname pathname))
  (let* ((pathname (truename pathname))
         (warnings
          (find pathname compiler::*error-database*
                :key 'first :test 'equal)))
    (loop for (dspec warning) in (rest warnings)
          for dylan-warning = (make-pathname-warning
                               pathname dspec warning)
          do (funcall function dylan-warning))))

(defmethod do-dylan-compiler-warnings (function (project dylan-project))
  (loop for file in (dylan-project-files project)
        for pathname = (dylan-project-file-truename project file)
        do (do-dylan-compiler-warnings function pathname)))

(defmethod do-dylan-compiler-warnings (function (library dylan-library))
  (let* ((name (dylan-library-name library))
         (project (find-dylan-project name)))
    (do-dylan-compiler-warnings function project)))


;;; Dylan code evaluation

(defmethod parsed-dylan-form ((form string) &key module)
  (when module
    (setf (dylan::current-module) (find-dylan-module module)))
  (handler-case
      (values
       (let ((*package* (find-package :dylan))
             (*readtable* dylan::*infix-dylan-load-eval-readtable*))
         (let ((command (read-from-string form)))
	   `(dylan::begin ,command)))
       nil)
    (error (error)
           (values dylan::*dylan-canonical-false* (format nil "~A" error)))))

(defmethod evaluate-dylan-form ((form string) &key module)
  (let ((expression (parsed-dylan-form form :module module)))
    (if expression
        (multiple-value-list (dylan::dylan-eval expression))
      dylan::*dylan-canonical-false*)))

(defmethod bind-dylan-variable ((variable string) value module)
  (let ((symbol
         (intern (string-upcase variable)
		 (dylan+dylan/internal::module-package module))))
    (setf (symbol-value symbol) value)))

(defgeneric environment-browse-object (object))


;;; Override the LW tools, if required

(defvar *use-dylan-tools* nil)

(defadvice (tools::win-inspect dylan-advice :around) (object)
  (if *use-dylan-tools*
      (environment-browse-object object)
    (call-next-advice object)))
