(in-package dylan)

(defun read-dylan-lid-file (name)
  (with-open-file (s name)
    (let* ((dylan-table
             (funcall
               (dylan-resolve maybe-read-file-header 
                              :module-name internal) s))
           (table
             (funcall
               (dylan-resolve table-table
                              :module-name internal) dylan-table)))
      table)))

(defun lid-value (key info)
  (car (gethash key info)))

;; Set to T if you want to do "serial compilation", that is,
;; any changed file causes all following files to be compiled
;; even if they haven't changed.
(defvar *enforce-serial-constraints* nil)

(defun load-dylan-lid-file (name &rest args)
  (with-compilation-unit ()
    (let* ((info (read-dylan-lid-file name))
           (library (lid-value :library info))
	   (files   (lid-value :files info)))
      (unless (consp library)
        (error "No library name specified in ~s" name))
      (unless (= (length library) 1)
        (error "More than one library name specified in ~s" name))
      (setq library (first library))
      (format t "~&;; Loading ~a as per ~a~%" library name)
      (let ((template (merge-pathnames "foo.dylan" name))
	    (force nil))
        (dolist (file files)
	  (setq force (apply #'generic-env:generic-compile-if-needed
		       (merge-pathnames file template)
		       :load t
		       :force (and *enforce-serial-constraints* force)
		       args))))
      (format t "~&;; Loaded ~a~%" library))))

(defun compile-dylan-lid-file (name &rest args &key (load t) &allow-other-keys)
  (with-compilation-unit ()
    (let* ((info (read-dylan-lid-file name))
           (library (lid-value :library info))
	   (files   (lid-value :files info)))
      (unless (consp library)
        (error "No library name specified in ~s" name))
      (unless (= (length library) 1)
        (error "More than one library name specified in ~s" name))
      (setq library (first library))
      (format t "~&;; Compiling ~a as per ~a~%" library name)
      (let ((template (merge-pathnames "foo.dylan" name)))
        (dolist (file files)
	  (apply #'generic-env:generic-compile
	   (merge-pathnames file template)
	   args)))
      (format t "~&;; Compiled ~a~%" library))))

(defun lid-filename-p (name)
  (string= (pathname-type (pathname name)) "lid"))

(generic-env:register-file-type 'dylan-lid-file
  :source-extensions '("lid")
  :object-extensions '()
  :compiler 'compile-dylan-lid-file
  :source-loader 'load-dylan-lid-file
  ;; :object-loader
)

;; eof
