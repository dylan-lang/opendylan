(in-package "SOURCE-CHANGES")

;;; Source change directory modelling

(defvar *source-changes-directory* ".dylan/source-changes/")

(defmethod source-changes-directory ()
  (ensure-directory #P"~/" *source-changes-directory*))

(defmethod source-change-directory ((name string))
  (ensure-directory (source-changes-directory) (format nil "~A/" name)))


;;; Source changes

(defvar *current-change* nil)

(defclass source-change ()
  ((name
    :initarg :name
    :accessor source-change-name)
   (subject
    :initarg :subject
    :accessor source-change-subject)
   (entries
    :initform nil
    :accessor source-change-entries)))

(defclass source-change-entry ()
  ((filename 
    :initarg :filename
    :accessor source-change-entry-filename)
   (compound
    :initarg :compound
    :initform nil
    :accessor source-change-entry-compound)
   (action
    :initarg :action
    :accessor source-change-entry-action)
   (comments
    :initarg :comments
    :accessor source-change-entry-comments)
   (checked-out-p
    :initarg :checked-out-p
    :initform nil
    :accessor source-change-entry-checked-out-p)))

(defmethod source-change-directory ((change source-change))
  (source-change-directory (source-change-name change)))

(defun select-source-change (name)
  (if (string= name (source-change-name *current-change*))
      *current-change*
    (let ((new-change (restore-source-change name)))
      (when new-change
        (setq *current-change* new-change)))))
      
(defmethod source-change-empty-p ((change source-change))
  (null (source-change-entries change)))

(defmethod find-source-change-entry ((change source-change) pathname)
  (when (probe-file pathname)
    (find (truename pathname) (source-change-entries change)
          :key 'source-change-entry-filename
          :test 'equal)))

(defun start-change (name subject)
  (unless (source-change-exists-p name)
    (let ((change
           (make-instance 'source-change
                          :name name
                          :subject subject)))
      (save-source-change change)
      (setq *current-change* change))))

(defmethod close-source-change ()
  (setq *current-change* nil))

(defmethod delete-source-change ((change source-change))
  (let* ((directory (namestring (source-change-directory change)))
         (directory-name
	  (subseq directory 0 (1- (length directory)))))
    (close-source-change change)
    (sys::call-system
     (format nil "cd ~A; rm *; cd ..; rmdir ~A"
	     directory-name
	     (first (last (pathname-directory directory)))))))

(defmethod record-file-change ((change source-change) filename action 
                               &key comments compound checked-out-p)
  (let ((entry
	 (make-instance 'source-change-entry
			:filename (truename filename)
			:action action
			:comments (or comments "")
                        :compound compound
                        :checked-out-p checked-out-p)))
    (push entry (source-change-entries change))
    entry))

(defmethod remove-change-file ((change source-change) filename)
  (let ((entry (find-source-change-entry change filename)))
    (when entry
      (setf (source-change-entries change)
            (remove entry (source-change-entries change)))
      t)))

(defun record-add-file-change (change filename &rest args)
  (apply 'record-file-change change filename :add args))

(defun record-change-file-change (change filename &rest args)
  (apply 'record-file-change change filename :change args))

(defun record-remove-file-change (change filename &rest args)
  (apply 'record-file-change change filename :remove args))

(defmethod add-comment ((filename string) (comment string))
  (add-comment (pathname filename) comment))

(defmethod add-comment ((entry source-change-entry) (comment string))
  (let* ((old-comments (source-change-entry-comments entry))
         (new-comments
          (if (> (length old-comments) 0)
              (format nil "~A~%~A" old-comments comment)
            comment)))
    (setf (source-change-entry-comments entry) new-comments)))

(defun file-checked-out-p (change filename)
  (let ((entry (find-source-change-entry change filename)))
    (when entry
      (source-change-entry-checked-out-p entry))))

(defun (setf file-checked-out-p) (checked-out-p change filename)
  (let ((entry (find-source-change-entry change filename)))
    (when entry
      (setf (source-change-entry-checked-out-p entry) checked-out-p))))


;;; Source changes directory handling

(defmethod create-directory (parent name)
  (let ((parent-name (namestring parent)))
    (eq (sys::call-system
         (format nil "cd ~A; mkdir ~A"
	         (subseq parent-name 0 (1- (length parent-name)))
                 name))
        0)))

(defun ensure-directory (parent directory)
  (let ((directory-pathname (format nil "~A~A" parent directory)))
    (unless (probe-file directory-pathname)
      (loop for current-directory = parent then new-directory
            for directory-name in (rest (pathname-directory directory))
            for new-directory = (format nil "~A~A/"
                                        current-directory directory-name)
            do
            (unless (probe-file new-directory)
              (unless (create-directory current-directory directory-name)
                (return-from ensure-directory nil)))))
    directory-pathname))


;;; Description file handling

(defmethod source-change-description-filename ((change source-change))
  (merge-pathnames (format nil "~A.description" (source-change-name change))
                   (source-change-directory change)))

(defun file-contents (filename)
  (with-output-to-string (string-stream)
    (with-open-file (stream filename :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do (format string-stream "~A~%" line)))))

(defmethod source-change-description ((change source-change))
  (let ((filename (source-change-description-filename change)))
    (when (probe-file filename)
      (file-contents filename))))


;;; Report generation

(defmethod generate-change-report ((change source-change) (stream stream))
  (format stream "Author: ~A~%~%" (sys::get-user-name))
  (let ((description (source-change-description change)))
    (when description
      (format stream "Description:~%~%~A~%" description)))
  (format stream "Files changed:~%~%")
  (print-source-changes change stream))

(defmethod source-change-entries-by-compound ((change source-change))
  (let (results)
    (loop for entry in (source-change-entries change)
          do
	  (multiple-value-bind
	      (unit compound)
	      (file-unit-and-compound entry)
            (let ((field (assoc compound results :test 'string=)))
              (if field
                  (push (list unit entry) (rest field))
		(setf (assoc compound results :test 'string=)
		      (list (list unit entry)))))))
    (sort results 'string< :key 'first)))

(defmethod print-text-indented ((text string) (indent string) (stream stream))
  (loop for old-position = 0 then (1+ position)
        for position = (position #\newline text :start old-position)
        do (format stream "~A~A~%" indent (subseq text old-position position))
        while position))

(defmethod print-source-changes ((change source-change) (stream stream))
  (loop for (compound . entries) in (source-change-entries-by-compound change)
        do
        (format stream "Changes in compound ~A:~%" compound)
        (loop for (unit entry) in entries
              for comments = (source-change-entry-comments entry)
              do
	      (format stream "  ~A unit ~A"
		      (case (source-change-entry-action entry)
			(:add    "Added")
			(:remove "Removed")
			(:change "Changed"))
		      unit)
	      (if (> (length comments) 0)
                  (progn
                    (format stream ":~%")
                    (print-text-indented comments "    " stream))
                (format stream "~%")))
        (format stream "~%")))


;;; HTML report generation

(defmethod make-relative-directory ((directory1 list) (directory2 list))
  (cond
   ((equal (first directory1) (first directory2))
    (make-relative-directory (rest directory1) (rest directory2)))
   (t
    (cons :relative
          (append (loop for i below (length directory2)
                        collect "..")
                  directory1)))))

(defmethod source-change-entry-relative-filename ((entry source-change-entry)
                                                  directory)
  (let* ((filename (source-change-entry-filename entry))
         (relative-directory
          (make-relative-directory (pathname-directory (truename filename))
                                   (pathname-directory (truename directory)))))
    (lw::make-pathname :directory relative-directory
                       :name (pathname-name filename)
                       :type (pathname-type filename))))

(defmethod html-change-file-name ((change source-change))
  (merge-pathnames "source-changes.html"
                   (source-change-directory change)))

(defmethod generate-html-change-file ((change source-change))
  (with-open-file (stream 
                   (html-change-file-name change)
                   :direction :output
		   :if-does-not-exist :create
		   :if-exists :supersede)
    (generate-html-change-report change stream)))

(defmethod generate-html-change-report ((change source-change) (stream stream))
  (format stream "<html>~%<head>~%")
  (format stream "<title>~A: ~A</title>~%</head>~%~%<body>~%~%"
          (sys::get-user-name)
          (source-change-subject change))
  (format stream "<h1>~A: ~A</h1>~%"
          (sys::get-user-name)
          (source-change-subject change))
  (format stream "<h2>File changes</h2>~%~%")
  (print-html-source-changes change stream)
  (format stream "</body>~%</html>~%"))

(defmethod source-change-entry-html-address ((change source-change)
                                             (entry source-change-entry))
  (format nil "../../source/~A"
          (source-change-entry-relative-filename 
           entry 
           #P"~/dylan/")))

(defmethod print-html-source-changes ((change source-change) (stream stream))
  (format stream "<ul>~%")
  (loop for (compound . entries) in (source-change-entries-by-compound change)
	do
	(format stream "  <li>Changes in compound ~A~%    <ul>~%" compound)
	(loop for (unit entry) in entries
	      for comments = (source-change-entry-comments entry)
	      do
	      (format stream "      <li>~A unit <a href=\"~A\">~A</a>"
		      (case (source-change-entry-action entry)
			(:add    "Added")
			(:remove "Removed")
			(:change "Changed"))
		      (source-change-entry-html-address change entry)
		      unit)
	      (when (> (length comments) 0)
		(format stream ":~%<br>  ~A" comments))
	      (format stream "~%"))
	(format stream "    </ul>~%"))
  (format stream "</ul>~%"))


;;; Log file handling

(defmethod all-source-changes ()
  (mapcar #'(lambda (pathname)
              (first (last (pathname-directory pathname))))
          (directory (source-changes-directory))))

(defmethod source-change-log-file-name (change)
  (merge-pathnames "changes.log" (source-change-directory change)))

(defmethod save-source-change ((change source-change))
  (print-source-change-log change (source-change-log-file-name change)))

(defmethod source-change-exists-p ((name string))
  (let ((directory
	 (format nil "~A/~A/" (source-change-directory) name)))
    (probe-file directory)))

(defmethod restore-source-change ((name string))
  (when (source-change-exists-p name)
    (read-source-change-log (source-change-log-file-name name))))

(defmethod print-source-change-log ((change source-change) file)
  (with-open-file (stream file :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (print-source-change-log change stream)))

(defmethod print-source-change-log ((change source-change) (stream stream))
  (format stream "(~S~% :SUBJECT ~S~% :ENTRIES ("
          (source-change-name change)
          (source-change-subject change))
  (let ((indent "           "))
    (loop for entry in (source-change-entries change)
          do
          (format stream "~%~A(~A"
                  indent
                  (source-change-entry-filename entry))
          (format stream "~%~A :ACTION ~S"
                  indent (source-change-entry-action entry))
          (format stream "~%~A :COMMENTS ~S"
                  indent (source-change-entry-comments entry))
          (format stream "~%~A :CHECKED-OUT-P ~S"
                  indent (source-change-entry-checked-out-p entry))
          (format stream "~%~A :COMPOUND ~S"
                  indent
                  (source-change-entry-compound entry))
          (format stream ")")))
  (format stream "))~%"))

(defmethod read-source-change-log (file)
  (when (probe-file file)
    (with-open-file (stream file :direction :input)
      (read-source-change-log stream))))

(defmethod read-source-change-log ((stream stream))
  (let ((data (read stream)))
    (destructuring-bind (name . plist) data
      (let ((change (make-instance 'source-change :name name)))
        (setf (source-change-subject change) (getf plist :subject))
        (setf (source-change-entries change) nil)
	(loop for (filename . plist) in (getf plist :entries)
	      do (record-file-change 
                  change filename
		  (getf plist :action)
		  :checked-out-p (getf plist :checked-out-p)
		  :compound (getf plist :compound)
		  :comments (getf plist :comments)))
        change))))
