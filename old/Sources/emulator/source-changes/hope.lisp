(in-package "SOURCE-CHANGES")

;;; Hope handling

(defmethod file-hope-version-file (pathname)
  (let ((directory (lw:pathname-location pathname)))
    (let ((file (merge-pathnames ".version" directory)))
      (when (probe-file file)
	file))))

(defmethod parse-hope-version-line (line)
  (let ((hope-info
         (loop for old-position = 0 then (1+ position)
               for position = (position #\, line :start old-position)
               collect (subseq line old-position position)
               while position)))
    (values (third hope-info) (first hope-info))))

(defmethod file-unit-and-compound ((entry source-change-entry))
  (let ((compound (source-change-entry-compound entry))
        (filename (source-change-entry-filename entry)))
    (if compound
        (values (file-namestring filename) compound)
      (file-unit-and-compound filename))))

(defmethod name-matches-hope-version-line ((name string) (line string))
  (search (format nil ",~A," name) line))

(defmethod file-unit-and-compound ((pathname pathname))
  (let ((name (file-namestring pathname)))
    (let ((version-file (file-hope-version-file pathname)))
      (cond
       (version-file
        (multiple-value-bind
            (unit compound)
	    (with-open-file (stream version-file)
	      (loop for line = (read-line stream nil nil)
		    while line
		    when (name-matches-hope-version-line name line)
		    return (parse-hope-version-line line)))
          (values (or unit name) compound)))
       (t
        (values name nil))))))

(defmethod invoke-hope ((command string) &key directory)
  (let* (result)
    (let* ((system-command
            (format nil "~Ahope ~A"
                    (if directory
                        (format nil "cd ~A~%" (truename directory))
                      "")
                    command))
           (output-string
            (with-output-to-string (stream)
              (setq result
                    (sys::call-system-showing-output
                     system-command
                     :prefix ""
                     :output-stream (make-broadcast-stream 
                                     *standard-output*
				     stream))))))
      (values (eq result 0) output-string))))

(defmethod run-hope-commands ((change source-change) &key test)
  (let* ((script (save-hope-command-script change :test test)))
    (format t "~&~A Hope commands:~%~%"
            (if test "Test running" "Running"))
    (invoke-hope (format nil "source ~A" script))))

(defmethod save-hope-command-script ((change source-change) &key test)
  (let ((pathname
         (merge-pathnames (if test
			      "test-hope-script"
			    "hope-script")
			  (source-change-directory change))))
    (with-open-file (stream pathname
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (generate-hope-script change stream :test test))
    pathname))

(defvar *last-directory* nil)

;;--- this is getting slightly out of control... it is trying to group
;;--- commands together to minimize the number of times it needs to
;;--- change directories. Any better organisations available?
(defmethod generate-hope-command-script (entries action (stream stream)
                                                 &key reason test)
  (let* ((entries 
          (remove-if-not #'(lambda (entry)
                             (eq action (source-change-entry-action entry)))
                         entries
                         :key 'second))
         (directories
          (remove-duplicates
           (mapcar #'(lambda (entry-info)
                       (lw:pathname-location
                        (source-change-entry-filename (second entry-info))))
                   entries)
           :test 'equal)))
    (loop for directory in directories
          for directory-entries
                = (remove-if-not
		   #'(lambda (entry)
		       (equal directory 
			      (lw:pathname-location
			       (source-change-entry-filename entry))))
		   entries
		   :key 'second)
          do
          (when directory-entries
            (unless (equal directory *last-directory*)
              (format stream "cd ~A~%" directory)
              (setq *last-directory* directory))
            (format stream "~A~A -reason ~S "
                    (if test
                        "test "
                      "")
                    (case action
                      (:add    "add   ")
                      (:change "ci    ")
                      (:remove "remove"))
                    (or reason (error "No reason supplied")))
            (loop for (entry-info . rest) = directory-entries then rest
                  for unit = (first entry-info)
                  do
                  (format stream "-u ~A" unit)
                  (when rest
                    (format stream " -and "))
                  while rest)
            (format stream "~%")))))

(defmethod generate-hope-script ((change source-change) (stream stream) 
                                 &key test)
  (let ((reason (source-change-subject change))
        (*last-directory* nil))
    (loop for (compound . entries) in (source-change-entries-by-compound change)
	  do
          (format stream "select ~A~%" compound)
          (dolist (action '(:add :change :remove))
            (generate-hope-command-script 
             entries action stream
	     :reason reason :test test))
          (format stream "~%"))))
