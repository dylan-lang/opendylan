(in-package :dylan)

(defun read-infix-dylan (stream)
  (funcall
    (dylan-resolve-name read-infix :module-name infix-reader)
    :stream stream))

(defun compile-infix-dylan-file (file &rest args)
  (let ((orig-pkg *package*)
        (orig-rt  *readtable*))
    (handler-bind
      ((condition 
         #'(lambda (ignore)
             (setq *package*   orig-pkg)
             (setq *readtable* orig-rt))))
      (let* ((dylan-path (pathname file))
             (lisp-path  (merge-pathnames (make-pathname :type "lisp") dylan-path))
             (*package*   (find-package :dylan))
             (*readtable* *infix-dylan-load-eval-readtable*))
        (apply #'compile-file dylan-path args)))))

(defun load-infix-dylan-file (file &rest args)
  (let ((*readtable* *infix-dylan-load-eval-readtable*)
        (*package* (find-package :dylan)))
    (unless (apply #'load file :if-does-not-exist nil args)
      (apply #'load (merge-pathnames (make-pathname :type "dyl")
                                     (pathname file))))))

(generic-env::register-file-type 'dylan::infix-dylan-file
  :source-extensions '("idyl" "idylan" "id")
  :object-extensions (list compiler:*fasl-extension-string*)
  :compiler          'dylan::compile-infix-dylan-file
  :object-loader     'dylan::load-infix-dylan-file)

;; The reader used at load/eval time.

(defparameter *infix-dylan-load-eval-readtable* 
  (let ((table (copy-readtable)))

    (labels

      ((character-hook (s c)
	 (unread-char c s)
	 (if (eq *package* (find-package :dylan))
	   (let ((*readtable* *dylan-readtable*))
	     (let* ((dyl (read-infix-dylan s)))
               (cond
                 ((dylan-defining-form-p dyl)
                   dyl)
                 ((or (keywordp dyl) (numberp dyl)
                      (and (consp dyl) (keywordp (first dyl))))
                   dyl)
                 (t
                   `(begin ,dyl)))))
           (read-infix-dylan s))))

      (dotimes (i 128)
        (let ((c (character i)))
          (unless (member c '
                          (#\space #\tab #\newline #\; #\Page)
                          :test #'eql)
            (set-macro-character (character i) #'character-hook nil table))))

      (set-macro-character 
        #\/
        #'(lambda (s c)
            (let ((next (peek-char nil s)))
              (case next
                ((#\/)
                  (read-line s)
                  (values))
                ((#\*)
                  (read-infix-area-comment s)
                  (values))
                (otherwise
                  (character-hook s c)))))
        nil
        table)

      table)))

(register-translating-readtable 
  'infix-dylan *infix-dylan-load-eval-readtable*)

;; eof
