(in-package dylan)

;; String splitting utility

(defun split-string (path &key (separator #\:))
  (let ((cursor 0))
    (loop for sep = (position separator path :start cursor)
          when sep
            collect (subseq path cursor sep)
            and do  (setf cursor (+ sep 1))
          else
            collect (subseq path cursor)
            and do  (loop-finish))))

;; Registry entry parsing

(defun dylan-locator-p (line)
  (search "://" line))

(defgeneric resolve-dylan-locator (registry line)) ;; Implemented in Dylan

(defun lisp-pathname-p (line)
  t)

(defun resolve-lisp-pathname (line)
  (let ((path (pathname line)))
    (if (pathname-host path)
       (translate-logical-pathname path)
      path)))

(defun translate-dylan-registry-entry (registry line)
  (cond
    ((dylan-locator-p line)
       (resolve-dylan-locator registry line))
    ((lisp-pathname-p line)
       (resolve-lisp-pathname line))
    (t
       (error "Registry entry in unknown format ~s." line))))

;; Parameters
(defparameter *emulator-compiler-print-message-level* t)

;; Old stuff.
(defparameter *dylan-root-environment-variable* "DYLAN_ROOT")
(defparameter *default-dylan-root* "/u/dylan/sources")
(defparameter *user-registries-environment-variable* "EMULATOR_LIBRARY_PATH")

;; New stuff.
(defparameter *system-root-environment-variable* "TRUNK_SYSTEM_ROOT")
(defparameter *default-system-root* 
  (concatenate 'string *default-dylan-root* "/"))
(defparameter *personal-root-environment-variable* "TRUNK_PERSONAL_ROOT")

;; Set up some useful logical pathname stuff.

(defun install-dylan-root ()
  (setf (logical-pathname-translations "DYLAN")
        `(("**;*" 
           ,(pathname (concatenate 'string 
		        (or (getenv *dylan-root-environment-variable*)
                            (and (boundp 'cl-user::*dylan-root*)
                                 cl-user::*dylan-root*)
                            *default-dylan-root*)
			"/**/*"))))))

(install-dylan-root)

;; Library memory.

(defvar *loaded-libraries* (make-hash-table))

(defun loaded-library-names ()
  (loop for k being each hash-key in *loaded-libraries*
        collect k))

(defun find-library (name) 
  (gethash name *loaded-libraries*))

(defun (setf find-library) (val name)
  (setf (gethash name *loaded-libraries*) val))

(defun unfind-library (name)
  (remhash name *loaded-libraries*))

(defun define-library 
     (name &rest info &key uses creates exports &allow-other-keys)
  (dolist (use uses)
    (ensure-library (first use)))
  (setf (find-library name) info)
  (values))

(defun ensure-library (name)
  (or (find-library name)
      (load-library name)))

;; Registries.

(defparameter *library-registries* 
  (list (pathname "DYLAN:emulator;registry")))

;; Read user config.

(defun root-registries (root)
  (values
    (concatenate 'string root "registry/emulator")
    (concatenate 'string root "registry/generic")))

(defun install-user-registries ()
  ;; New stuff. Put the platform-specific directories before the
  ;; generic directories for now.
  (let ((platform '()) (generic '()))
    (let ((system-root 
	    (or (getenv *system-root-environment-variable*) 
		*default-system-root*)))
      (when system-root
	 (multiple-value-bind 
	     (system-platform system-generic) (root-registries system-root)
	   (push system-platform platform)
	   (push system-generic generic))))
    (let ((personal-root (getenv *personal-root-environment-variable*)))
      (when personal-root
	 (multiple-value-bind 
	     (personal-platform personal-generic) 
	       (root-registries personal-root)
	   (push personal-platform platform)
	   (push personal-generic generic))))
    (setq *library-registries*
	  (append platform generic *library-registries*)))
  ;; Backward compatibility stuff. This has priority in order to keep
  ;; things up and running.
  (setq *library-registries* 
	(cons (pathname "DYLAN:emulator;registry") *library-registries*))
  (let ((user-registries (getenv *user-registries-environment-variable*)))
    (when user-registries
      (let ((registries (split-string user-registries :separator #\:)))
        (setq *library-registries* 
              (append registries *library-registries*))))))

(install-user-registries)

(defun load-library (name)
  (let ((normal-exit nil))
    (unwind-protect 
      (progn
        (dolist (registry *library-registries*)
          (when (load-library-from-registry name registry)
	    (setq normal-exit t)
            (return-from load-library name)))
        (error "Failed to find the ~s library in any registry - looked in ~s"
               name *library-registries*))
      (unless normal-exit
	(format *standard-output* "~&;; Undefining library ~s~%" name)
	(unfind-library name)))))

(defun print-registry-usage (name registry)
  (format *standard-output* "~&;; Using registry entry ~A/~A~%"
	  registry (string-downcase name)))

(defun load-library-from-registry (name registry)
  (unless (probe-file registry)
    (warn "Registry ~s does not exist" registry)
    (return-from load-library-from-registry nil))
  (let ((files (directory registry
		  :include-contents t
                  :test #'(lambda (file)
			    (and (not (directoryp file))
                                 ;; equalp does caseless comparison
				 (equalp (pathname-name file) 
					 (symbol-name name)))))))
    (if (null files) nil
      (let ((file (first files)))
        (with-open-file (stream file)
          (let ((*readtable* sys::*std-lisp-readtable*))
            (print-registry-usage name registry)
            ;; This forced translation is here because merging doesn't seem
            ;; to work the way I'd expect it to.
            (generic-env:generic-load 
              (translate-dylan-registry-entry registry (read-line stream))
	      :print *emulator-compiler-print-message-level*))
          (unless (find-library name)
            (setf (find-library name) t)))
        t))))

(defun compile-library (name)
  (let ((normal-exit nil))
    (unwind-protect 
      (progn
        (dolist (registry *library-registries*)
          (when (compile-library-from-registry name registry)
	    (setq normal-exit t)
            (return-from compile-library name)))
        (error "Failed to find the ~s library in any registry - looked in ~s"
               name *library-registries*))
      (unless normal-exit
	(format *standard-output* "~&;; Undefining library ~s~%" name)
	(unfind-library name)))))

(defun compile-library-from-registry (name registry)
  (unless (probe-file registry)
    (warn "Registry ~s does not exist" registry)
    (return-from compile-library-from-registry nil))
  (let ((files (directory registry
		  :include-contents t
                  :test #'(lambda (file)
			    (and (not (directoryp file))
                                 ;; equalp does caseless comparison
				 (equalp (pathname-name file) 
					 (symbol-name name)))))))
    (if (null files) nil
      (let ((file (first files)))
        (with-open-file (stream file)
          (let ((*readtable* sys::*std-lisp-readtable*))
            (print-registry-usage name registry)
            ;; This forced translation is here because merging doesn't seem
            ;; to work the way I'd expect it to.
            (generic-env:generic-compile 
              (translate-dylan-registry-entry registry (read-line stream))
              :load t :print *emulator-compiler-print-message-level*))
          (unless (find-library name)
            (setf (find-library name) t)))
        t))))

(defun load-entry (name)
  (dolist (registry *library-registries*)
    (let ((entry (load-entry-from-registry name registry)))
      (when entry
        (return-from load-entry entry))))
  nil)

(defun load-entry-from-registry (name registry)
  (unless (probe-file registry)
    (warn "Registry ~s does not exist" registry)
    (return-from load-entry-from-registry nil))
  (let ((files (directory registry
		  :include-contents t
                  :test #'(lambda (file)
			    (and (not (directoryp file))
                                 ;; equalp does caseless comparison
				 (equalp (pathname-name file) 
					 (symbol-name name)))))))
    (if (null files) nil
      (let ((file (first files)))
        (with-open-file (stream file)
          (print-registry-usage name registry)
	  ;; This forced translation is here because merging doesn't seem
	  ;; to work the way I'd expect it to.
          (translate-dylan-registry-entry registry (read-line stream)))))))

;; Load everything (for builds, mainly).

(defun ensure-all-libraries ()
  (let ((libs (available-library-names))
        (failures '()))
    (dolist (lib libs)
      (format t "; Ensuring ~s~%" lib)
      (handler-case (ensure-library lib)
	(error (c)
	  (push lib failures)
	  (format t "; Caught error: ~a~%" c)
	  (format t "; Moving on~%"))))
    failures))

(defun available-library-names ()
  (loop for registry in *library-registries*
        append (available-library-names-from-registry registry)))

(defun available-library-names-from-registry (registry)
  (unless (probe-file registry)
    (warn "Registry ~s does not exist" registry)
    (return-from available-library-names-from-registry nil))
  (loop for path in (directory registry 
		       :include-contents t
                       :test #'(lambda (file) (not (directoryp file))))
        for name = (pathname-name path)
        when (> (length name) 0) 
        collect 
	  (intern (string-upcase (pathname-name name))
		  *the-dylan-package*)))

(defun find-library-info (name)
  (let ((entry (load-entry name)))
    (when (and entry (lid-filename-p entry))
      (read-dylan-lid-file entry))))

(defun find-library-synopsis (name)
  (let ((info (find-library-info name)))
    (when info
      (lid-value :synopsis info))))

(defun find-library-description (name)
  (let ((info (find-library-info name)))
    (when info
      (lid-value :description info))))

(defun find-library-author (name)
  (let ((info (find-library-info name)))
    (when info
      (lid-value :author info))))

;; Make sure that registries and the Dylan root get reinitialized
;; when the emulator restarts, in case the environment variables
;; have changed (for example when doing a release).

(pushnew #'install-user-registries *emulator-restart-hooks*)
(pushnew #'install-dylan-root *emulator-restart-hooks*)

;; eof
