(in-package "SOURCE-CHANGES")

(defvar *user-dylan-directory* "~/dylan")

(defvar *source-change-mail-alias* "dylan-source-changes")


;;; Useful macro

;; Stolen from the editor sources
(defmacro with-random-typeout-to-window ((&optional the-contact) &body body)
  (lw:with-unique-names (contact stream stream-already-mapped-p normal-exit)
    `(let* ((,contact (or ,the-contact (current-editor-pane)))
	    (,normal-exit nil))
      (multiple-value-bind
	  (,stream ,stream-already-mapped-p)
	  (and ,contact (ccl::pop-up-stream-window ,contact))
	(unwind-protect 
	    (progn ,@body
	      (setq ,normal-exit t))
	  (unless ,stream-already-mapped-p
	    (and ,contact (ccl::rto-ask-for-space ,stream ,normal-exit))))))))


;;; Editor commands

(editor:defcommand "DSC Start Change" (p)
     "Start recording a Dylan source change"
     "Start recording a Dylan source change"
  (declare (ignore p))
  (let ((name
	 (editor:prompt-for-string
	  :prompt "Enter change name (no spaces): ")))
    (if (source-change-exists-p name)
        (editor:editor-error "A source change ~A already exists" name)
      (let ((subject
             (editor:prompt-for-string
              :prompt "Enter change subject (for mail etc): ")))
        (start-change name subject)
        (editor:message "Created new change ~A" name)))))

(editor:defcommand "DSC Abandon Change" (p)
     "Abandon a Dylan source change"
     "Abandon a Dylan source change"
  (declare (ignore p))
  (cond
   (*current-change*
    (when (tk:confirm-yes-or-no
           "Do you really want to abandon your entire change?")
      (loop for entry in (source-change-entries *current-change*)
            do (editor::dsc-abandon-file-command 
                nil
                :filename (source-change-entry-filename entry)))
      (delete-source-change *current-change*)
      (editor:message "Abandoned change ~A"
                      (source-change-name *current-change*))
      (setf *current-change* nil)))
   (t
    (editor:editor-error 
     "Unable to abandon change: no current change selected"))))

(defun ensure-current-change ()
  (or *current-change*
      (editor::dsc-start-change-command nil)))

(editor:defcommand "DSC Finish Change" (p)
     "Finish recording a Dylan source change"
     "Finish recording a Dylan source change"
  (declare (ignore p))
  (assert-current-change)
  (when (run-hope-commands *current-change*)
    (let ((change *current-change*))
      (delete-source-change change)
      (display-mail-message change)
      (setf *current-change* nil)
      (editor:message "Change completed" (source-change-name change)))))

(defun complete-source-change-name (string)
  (let* ((length (length string))
         (all-changes (all-source-changes))
         (matches
          (remove-if-not #'(lambda (x)
                             (unless (< (length x) length)
			       (string= string x 
					:end1 length
					:end2 length)))
                         all-changes))
         (match
          (when (eq (length matches) 1)
            (first matches))))
    (values (or match string)
            (when match :unique)
            matches
            (not (null matches)))))

(defun prompt-for-change ()
  (let ((all-changes (all-source-changes)))
    (editor:prompt-for-string
     :prompt "Select change: "
     :complete-func #'(lambda (string info)
			(declare (ignore info))
			(complete-source-change-name string))
     :verify-func #'(lambda (string info)
		      (declare (ignore info))
		      (member string all-changes :test 'string=)))))

(defun prompt-for-comment (&key default)
  (string-trim
   #(#\space #\tab)
   (editor:prompt-for-string
    :prompt "Add a comment: "
    :default (or default "")
    :in-prompt nil)))

(defun prompt-for-file ()
  (editor:prompt-for-file
   :must-exist t
   :default (namestring (editor:buffer-pathname (editor:current-buffer)))))

(editor:defcommand "DSC Select Change" (p)
     "Select an existing Dylan change"
     "Select an existing Dylan change"
  (declare (ignore p))
  (let* ((name (prompt-for-change)))
    (if (select-source-change name)
        (editor:message "Selected source change ~A" name)
      (editor:editor-error "No such change ~A" name))))

(editor:defcommand "DSC Close Change" (p)
     "Closes an existing Dylan change"
     "Closes an existing Dylan change"
  (declare (ignore p))
  (assert-current-change :not-empty-p nil)
  (close-source-change))

(defun dsc-record-file-change (action)
  (ensure-current-change)
  (let* ((filename (prompt-for-file))
         (comments (prompt-for-comment)))
    (record-file-change *current-change* filename action :comments comments)
    (save-source-change *current-change*)
    (values filename comments)))

(defun revert-buffer-for-file (pathname)
  (let ((buffer (editor::find-file-in-buffer-list (truename pathname))))
    (when buffer
      (editor:revert-file-command nil buffer)
      t)))

(defun user-directory-for-pathname (pathname)
  (let* ((pathname (lw:pathname-location pathname))
         (dylan-directory-pathname (namestring (truename "~dylan/")))
         (prefix-length (length dylan-directory-pathname))
         (pathname-name (namestring (truename pathname))))
    (when (string= pathname-name
                   dylan-directory-pathname
                   :end1 prefix-length)
      (let ((user-dylan-directory (truename *user-dylan-directory*))
            (relative-pathname (subseq pathname-name prefix-length)))
        (ensure-directory user-dylan-directory relative-pathname)
        (format t "~A ~A" user-dylan-directory pathname-name relative-pathname)
        (truename
         (format nil "~A~A"
                 user-dylan-directory
                 relative-pathname))))))

(defun checkout-pathname-for-file (filename)
  (let ((directory (lw:pathname-location filename)))
    (if (lw:file-writable-p directory)
        filename
      (let ((unit (file-unit-and-compound (pathname filename))))
        (let ((user-directory (user-directory-for-pathname filename)))
          (when user-directory
            (format nil "~A~A" user-directory unit)))))))

(defun checkout-file (filename unit compound comments)
  (let ((directory (lw:pathname-location filename)))
    (if (lw:file-writable-p directory)
	(with-random-typeout-to-window ()
          (prog1
	      (invoke-hope
	       (format nil "checkout -co ~A -unit ~A -soft -reason ~S" 
		       compound unit comments)
	       :directory (lw:pathname-location filename))
	    (revert-buffer-for-file filename)))
      (error "Attempting to checkout in write protected directory ~A"
             directory))))

(defun edit-latest-version-of-file (filename &key force)
  (let ((pathname (pathname filename)))
    (multiple-value-bind
        (buffer new-buffer-p)
        (editor::find-file-buffer-verbose pathname)
      (editor:goto-buffer buffer t)
      (unless new-buffer-p
        (editor:revert-file-command nil buffer (not force))))))

(editor:defcommand "DSC Change File" (p)
     "Record details of a source change in a file"
     "Record details of a source change in a file"
  (declare (ignore p))
  (ensure-current-change)
  (let* ((filename (prompt-for-file))
         (comments (prompt-for-comment))
         (pathname (checkout-pathname-for-file filename))
         (file-writable-p (lw:file-writable-p filename)))
    (multiple-value-bind
        (unit compound)
        (file-unit-and-compound filename)
      (declare (ignore unit))
      (if compound
	  (let ((checked-out-p
                 (and (not file-writable-p)
		      (checkout-file pathname unit compound
                                     (if (> (length comments) 0)
                                         comments
				       (source-change-description *current-change*))))))
            (record-file-change *current-change* pathname :change
                                :comments comments
                                :compound compound
                                :checked-out-p checked-out-p)
            (save-source-change *current-change*)
            (edit-latest-version-of-file pathname))
	(editor:editor-error "Change not possible as file is not in HOPE")))))

(editor:defcommand "DSC Add File" (p)
     "Record details of a file being added"
     "Record details of a file being added"
  (declare (ignore p))
  (dsc-record-file-change :add))

(editor:defcommand "DSC Remove File" (p)
     "Record details of a file being removed"
     "Record details of a file being removed"
  (declare (ignore p))
  (dsc-record-file-change :remove))

(defun abandon-hope-file (filename &key quiet)
  (prog1
      (with-random-typeout-to-window ()
        (multiple-value-bind
	    (unit compound)
	    (file-unit-and-compound filename)
          (when (invoke-hope
	         (format nil "abandon -co ~A -unit ~A" 
		         compound unit)
	         :directory (lw:pathname-location filename))
            t)))
    (edit-latest-version-of-file filename :force t)
    (unless quiet
      (editor:message "Abandoned file ~A" (file-namestring filename)))))

(editor:defcommand "DSC Abandon File" (p &key filename)
     "Remove the change entry from a Dylan source change"
     "Remove the change entry from a Dylan source change"
  (declare (ignore p))
  (let* ((filename (or filename (prompt-for-file)))
         (checked-out-p 
          (and *current-change*
               (file-checked-out-p *current-change* filename))))
    (if (and *current-change*
             (remove-change-file *current-change* filename))
        (progn
          (save-source-change *current-change*)
          (when checked-out-p
            (abandon-hope-file filename :quiet t))
	  (editor:message "Removed change for file ~A" 
			  (file-namestring filename)))
      (when (tk:confirm-yes-or-no
             "File not part of change -- abandon from HOPE anyway?")
        (abandon-hope-file filename)))))

(defmethod add-comment ((filename pathname) (comment string))
  (let ((entry
	 (or (find-source-change-entry *current-change* filename)
	     (record-change-file-change *current-change* filename))))
    (add-comment entry comment)))

(editor:defcommand "DSC Add Comment" (p)
     "Add a source change comment for a file"
     "Add a source change comment for a file"
  (declare (ignore p))
  (ensure-current-change)
  (let* ((filename (prompt-for-file))
         (comment (prompt-for-comment)))
    (add-comment filename comment)
    (save-source-change *current-change*)))

(editor:defcommand "DSC Edit Description" (p)
     "Edit the description file for a source change"
     "Edit the description file for a source change"
  (declare (ignore p))
  (ensure-current-change)
  (let* ((filename (source-change-description-filename *current-change*)))
    (edit-latest-version-of-file filename)
    (editor:auto-fill-mode-command 1)))


;;; Definition handling

(defmethod default-definition-change-string (definition action)
  (format nil "~A ~A:"
          (case action
            (:add "Added")
            (:change "Changed")
            (:remove "Removed"))
          definition))

(defmethod record-definition-change ((change source-change) action)
  (ensure-current-change)
  (let* ((buffer (editor:current-buffer))
         (definition 
	  (editor::buffer-definition buffer))
         (filename (editor:buffer-pathname buffer))
         (comments 
          (prompt-for-comment 
           :default (default-definition-change-string definition action))))
    (add-comment filename comments)
    (save-source-change change)))

(editor:defcommand "DSC Change Definition" (p)
     "Record details of a source change in the current definition"
     "Record details of a source change in the current definition"
  (declare (ignore p))
  (record-definition-change *current-change* :change))

(editor:defcommand "DSC Add Definition" (p)
     "Record details of a source change in the current definition"
     "Record details of a source change in the current definition"
  (declare (ignore p))
  (record-definition-change *current-change* :add))

(editor:defcommand "DSC Remove Definition" (p)
     "Record details of a source change in the current definition"
     "Record details of a source change in the current definition"
  (declare (ignore p))
  (record-definition-change *current-change* :remove))


;;; Mail handling
(defun assert-current-change (&key (not-empty-p t))
  (unless *current-change*
    (editor:editor-error "Command failed -- no current change"))
  (when (and not-empty-p
             (source-change-empty-p *current-change*))
    (editor:editor-error "Source change is currently empty")))

(editor:defcommand "DSC Mail Change" (p)
     "Creates a source change mail message for the current change"
     "Creates a source change mail message for the current change"
  (declare (ignore p))
  (assert-current-change)
  (display-mail-message *current-change*))

(defvar *code-reviewer* nil)

(editor:defcommand "DSC Mail Reviewer" (p)
     "Creates a source change mail message for the current change"
     "Creates a source change mail message for the current change"
  (declare (ignore p))
  (assert-current-change)
  (let ((reviewer
         (editor:prompt-for-string 
          :default (or *code-reviewer* "")
          :in-prompt (when *code-reviewer* t))))
    (setq *code-reviewer* reviewer)
    (display-mail-message 
     *current-change*
     :to reviewer
     :subject (format nil "To Review: ~A"
		      (source-change-subject *current-change*)))))
    
(defvar *previous-compound* nil)

(defmethod ensure-change-entry-compounds ((change source-change))
  (loop for entry in (source-change-entries change)
        do
        (multiple-value-bind
            (unit compound)
            (file-unit-and-compound entry)
          (declare (ignore unit))
          (unless compound
            (let ((entry-compound
                   (editor:prompt-for-string
                    :default *previous-compound*
                    :prompt (format nil "Failed to deduce compound for file ~A~% -- please enter compound name: "
                                    (source-change-entry-filename entry)))))
              (setf (source-change-entry-compound entry) entry-compound)
              (setq *previous-compound* entry-compound))))))

(defmethod display-mail-message ((change source-change) &key to subject)
  (ensure-change-entry-compounds change)
  (mail:find-a-mail-tool
   :to (or to *source-change-mail-alias*)
   :subject (or subject (source-change-subject change))
   :contents (with-output-to-string (stream)
               (generate-change-report change stream))))


;;; Hope commands

(defun current-editor-pane ()
  (let ((window (editor:current-window)))
    (when window
      (editor:window-text-pane window))))

(editor:defcommand "DSC Show Hope Commands" (p)
     "Run all Hope commands associated with a source change"
     "Run all Hope commands associated with a source change"
  (declare (ignore p))
  (assert-current-change)
  (ensure-change-entry-compounds *current-change*)
  (let ((pathname (save-hope-command-script *current-change*)))
    (edit-latest-version-of-file pathname :force t)))

(editor:defcommand "DSC Run Hope Commands" (p)
     "Run all Hope commands associated with a source change"
     "Run all Hope commands associated with a source change"
  (declare (ignore p))
  (assert-current-change)
  (ensure-change-entry-compounds *current-change*)
  (with-random-typeout-to-window ()
    (run-hope-commands *current-change*)))

(editor:defcommand "DSC Test Run Hope Commands" (p)
     "Run all Hope commands associated with a source change"
     "Run all Hope commands associated with a source change"
  (declare (ignore p))
  (assert-current-change)
  (ensure-change-entry-compounds *current-change*)
  (with-random-typeout-to-window ()
    (run-hope-commands *current-change* :test t)))
