(in-package :generic-env)

;; A particular file type may have any number of equivalent source types but,
;; for now at least, a unique object file type.

(defclass file-type ()
  ((name
     :accessor file-type-name
     :initarg  :name)
   (object-extension 
     :accessor file-type-object-extensions
     :initarg  :object-extensions)
   (compiler 
     :accessor file-type-compiler
     :initarg  :compiler)
   (source-loader 
     :accessor file-type-source-loader
     :initarg  :source-loader)
   (object-loader
     :accessor file-type-object-loader
     :initarg  :object-loader)))

(defmethod print-object ((ft file-type) (s stream))
  (format s "#<file-type: ~s>" (file-type-name ft)))

(defvar *object-file-types* (make-hash-table :test #'equalp))
(defvar *source-file-types* (make-hash-table :test #'equalp))

(defun register-file-type (name &rest args
                                &key (source-extensions)
				     (object-extensions)
                                     (compiler)
                                     (source-loader)
                                     (object-loader))
  (let ((file-type (apply #'make-instance 'file-type :name name args)))
    (dolist (type source-extensions)
      (setf (gethash type *source-file-types*) file-type))
    (dolist (type object-extensions)
      (setf (gethash type *object-file-types*) file-type))
    file-type))

(defun find-file-type (type)
  (let ((obj (gethash type *object-file-types*))
        (src (gethash type *source-file-types*)))
    (cond
      (obj (values :object obj))
      (src (values :source src))
      (t   (values nil     nil)))))

(defun find-all-files-from (path extensions)
  (let ((acc '()))
    (maphash
      #'(lambda (ext file-type)
	  (let ((full-path (merge-pathnames (make-pathname :type ext) path)))
            (when (probe-file full-path)
              (push full-path acc))))
     extensions)
    acc))

(defun find-all-files (path)
  (values
    (find-all-files-from path *source-file-types*)
    (find-all-files-from path *object-file-types*)))

(defun find-matching-files (path &key all)
  (let ((type (pathname-type path)))
    (cond
      ((and type (not all))
        (if (not (probe-file path))
          (values nil nil nil)
          (multiple-value-bind
              (kind file-type) (find-file-type type)
            (case kind
              ((:source)
               (values file-type path nil))
              ((:object)
               (values file-type nil path))
              (t
               (error "Unknown file type ~s for ~s" type))))))
      (type
       (multiple-value-bind (kind file-type) (find-file-type type)
         (let ((obj 
                (merge-pathnames 
	          (make-pathname 
                    :type 
		      (first (file-type-object-extensions file-type)))
                  path)))
           (values
             file-type 
             (if (probe-file path) path nil)
             (if (probe-file obj) obj nil)))))
      (t
        (multiple-value-bind
             (srcs objs) (find-all-files path)
           (let ((key (or (car srcs) (car objs))))
             (values 
	      (when key
                (cadr (multiple-value-list (find-file-type (pathname-type key)))))
              (car srcs)
              (car objs))))))))

(defun generic-load (name &rest args)
  (let* ((path (pathname name)))
    (multiple-value-bind
      (file-type src-path obj-path) (find-matching-files name)
      (cond
        ((not file-type)
          (error "couldn't find ~a to load" name))
        ((not src-path)
	  (apply (file-type-object-loader file-type) obj-path args))
        ((not obj-path)
          (apply (file-type-source-loader file-type) src-path args))
        (t
          (let ((src-date (file-write-date src-path))
                (obj-date (file-write-date obj-path)))
            (when (> src-date obj-date)
              (format t "; warning -- object file ~a is out-of-date" obj-path))
	    (apply (file-type-object-loader file-type) obj-path args)))))))

(defun generic-compile (name &rest args)
  (let* ((path (pathname name)))
    (multiple-value-bind
      (file-type src-path obj-path) (find-matching-files name)
      (cond
        ((or (not file-type) (not src-path))
          (error "couldn't find source file ~a to compile" name))
        (t
	  (apply (file-type-compiler file-type) src-path args))))))

(defun generic-compile-if-needed (name &rest args &key load force &allow-other-keys)
  (let* ((path (pathname name)))
    (multiple-value-bind
	(file-type src-path obj-path) (find-matching-files name :all t)
      (cond
        ((or (not file-type) (not src-path))
          (error "couldn't find source file ~a to compile" name))
        ((or (not obj-path) 
             force
	     (> (file-write-date src-path) (file-write-date obj-path)))
	 (let ((args (sys::remove-properties args '(:force))))
	   (apply (file-type-compiler file-type) src-path args))
	 t)
        (load
	  (apply (file-type-object-loader file-type) obj-path '())
	  nil)))))

;; Lisp files:

(register-file-type 'lisp-file  
  :source-extensions '("lisp" "lsp")
  :object-extensions (list compiler:*fasl-extension-string*)
  :compiler          'compile-file
  :source-loader     'load
  :object-loader     'load)

;; eof

