(in-package "EDITOR")

(do-demand-pre-loads :shell-buffer)
(do-demand-pre-loads :shell-window)

(unless (fboundp 'revert-buffer-command)
(defun revert-buffer-command (&rest args)
  (apply 'revert-file-command args))
)

(defun find-hope-compound-and-unit (object)
  (let ((pathname (cond ((bufferp object) (buffer-pathname object))
                        ((pathnamep object) object)
                        ((stringp object) (pathname object)))))
    (find-hope-compound-and-unit-from-pathname
     pathname "" (sys::copy-pathname-modified pathname
                                              :host nil
                                              :device nil
                                              :directory '(:relative)))))

(defun find-hope-compound-and-unit-from-pathname
     (pathname unit-prefix filename)
  (declare (type simple-string unit-prefix)
           (type pathname pathname))
  (let ((compound
         (with-open-file (hope-compound-stream
		          (merge-pathnames (make-pathname :name ".version"
						          :type :unspecific)
				           pathname)
		          :if-does-not-exist nil)
	   (and hope-compound-stream
                (let ((version (read-line hope-compound-stream nil nil)))
                  (subseq version 0 (search "," version)))))))
    (if compound
	(values compound
                (concatenate 'simple-string
			     unit-prefix (file-namestring pathname))
                (directory-namestring pathname)
                filename)
      (let ((directory (pathname-directory pathname)))
	(if (cdr directory)
            (let ((directory-component (car (last directory))))
              (find-hope-compound-and-unit-from-pathname
               (sys::copy-pathname-modified pathname
                                            :directory (butlast directory))
               (concatenate 'simple-string
                            directory-component
                            ":"
                            unit-prefix)
               (merge-pathnames 
                filename
		(make-pathname
                 :directory (list :relative directory-component)))))
          (editor-error "No HOPE compound for ~A" pathname))))))

#|
(defun find-hope-compound-and-unit (object)
  (let ((pathname (cond ((bufferp object) (buffer-pathname object))
                        ((pathnamep object) object)
                        ((stringp object) (pathname object)))))
    (find-hope-compound-and-unit-from-pathname
     pathname "" (sys::copy-pathname-modified pathname
                                              :host nil
                                              :device nil
                                              :directory '(:relative)))))

(defun hope-compound-pathname (pathname)
  (merge-pathnames (make-pathname :name ".hope-compound"
				  :type :unspecific)
		   pathname))

(defun find-hope-compound-and-unit-from-pathname (pathname unit-prefix filename)
  (declare (type simple-string unit-prefix)
           (type pathname pathname))
  (let ((compound
         (with-open-file (hope-compound-stream
                          (hope-compound-pathname pathname)
		          :if-does-not-exist nil)
	   (and hope-compound-stream
	        (read-line hope-compound-stream nil nil)))))
    (if compound
	(values compound
                (concatenate 'simple-string
			     unit-prefix (file-namestring pathname))
                (directory-namestring pathname)
                filename)
      (let ((directory (pathname-directory pathname)))
	(if (cdr directory)
            (let ((directory-component (car (last directory))))
              (find-hope-compound-and-unit-from-pathname
               (sys::copy-pathname-modified pathname
                                            :directory (butlast directory))
               (concatenate 'simple-string
                            directory-component
                            ":"
                            unit-prefix)
               (merge-pathnames filename
                                (make-pathname :directory (list :relative directory-component)))))
          (editor-error "No HOPE compound for ~A" pathname))))))
|#

(defvar *hope-editor-window* nil)

;; 18 Jul 1994, AA - ensure that the window isn't onto a dead display
;; 19 Jul 1994, AA - be a little more careful about dead displays!
;; 08 Aug 1994, AA - this isn't valid if the contact itself has been
;; destroyed.
(defun valid-hope-listener ()
  (and *hope-editor-window*
       (when-let (contact (window-text-pane *hope-editor-window*))
         (not (lw::destroyed-or-no-display-p contact)))
       (variable-value "Shell FD"
		       :buffer (window-buffer *hope-editor-window*))))

(defun ensure-hope-listener ()
  (unless (valid-hope-listener)
    (setq *hope-editor-window* (make-hope-listener))
    (set-hope-ping)))

(defvar *hope-ping-interval* (* 6 60 60)
  "Interval in seconds for the keep-HOPE-connection-alive ping.")

(defun set-hope-ping ()
  (mp:delete-scheduled-named-event 'hope-ping)
  (mp:schedule-named-event 'hope-ping *hope-ping-interval* nil
                           'do-hope-ping))

(defun do-hope-ping ()
  (when (valid-hope-listener)
    (tell-hope "# I am still here")))

(defun send-command-to-window (window command)
  (send-to-window window `(process-character ,command ,window)))

(defun send-keys-to-window (window string)
  (loop for char across string
        do (send-to-window window `(process-character ,char ,window))))

(defun tell-hope (string &rest args)
  (apply 'format t "~%~@?" string args)
  (send-command-to-window *hope-editor-window* "End of Buffer")
  (send-keys-to-window *hope-editor-window*
                       (format nil "~?~A" string args #\Return))
  (set-hope-ping))

(defun show-hope-window ()
  (sys::bring-to-front (window-text-pane *hope-editor-window*)))

;; e.g. (("~clc/**/*.*" "/usr/users/martin/nfs-files/hacks/**/*.*"))
(defvar *hope-pathname-translations* '()
  "Pathname translations for HOPE file operations.  Same format as LOGICAL-PATHNAME-TRANSLATIONS.")

(defun translate-hope-directory (directory)
  (loop while
        (loop for (translate-from translate-to) in *hope-pathname-translations*
	      do
	      (when (pathname-match-p directory translate-from)
		(setq directory
                      (translate-pathname directory translate-from translate-to))
		(return t))
	      finally (return nil)))
  directory)

(defun ensure-hope-for-file (directory compound unit private-copy-p)
  (ensure-hope-listener)
  (let ((translated-directory (if private-copy-p
                                  (translate-hope-directory directory)
                                directory)))
    (when private-copy-p
      (unless (directoryp translated-directory)
        (sys:make-directory (sys::directory-pathname-as-file-pathname
                             translated-directory))))
    (tell-hope "cd ~A" translated-directory)
    (tell-hope "select ~A" compound)
    (and private-copy-p translated-directory)))

(defmacro def-hope-file-command (name doc (&key private-copy-p bindings) &body body)
  `(defcommand ,name (p &optional (buffer (current-buffer)))
        ,doc
        ,doc
     (declare (ignore p))
     (let* ((pathname (buffer-pathname buffer)))
       (multiple-value-bind (compound unit directory filename)
           (find-hope-compound-and-unit pathname)
         compound unit directory filename ;ignorable
         (let* ,bindings
           (let ((private-file-directory (ensure-hope-for-file directory compound unit
                                                               ',private-copy-p)))
             private-file-directory ;ignorable
	     ,@body))))))

(def-hope-file-command "HOPE CheckOut"
    "Checkout the file for the current buffer"
  (:private-copy-p t
   :bindings ((reason (prompt-for-string :prompt "Reason for checkout: "
			                 :default "Editing"))))

  (setf (variable-value 'hope-checkout-reason :buffer buffer) reason)
  (when private-file-directory
    ;; ensure the user has a .hope-compound file in the private directory
    #|
    (unless (probe-file (hope-compound-pathname private-file-directory))
      (tell-hope "checkout -ci-date -unit ~A"
                 (hope-compound-pathname "")
	         unit))
    |#
    (setq pathname
          (setf (buffer-pathname buffer)
                (merge-pathnames filename private-file-directory))))
  (tell-hope "checkout -ci-date -unit ~A -claim soft -reason ~S"
	     unit reason)
  (show-hope-window)
  (revert-when-file-is-not-read-only pathname buffer))

(def-hope-file-command "HOPE CheckIn"
    "Checkin the file for the current buffer.  The checkin reason defaults to the checkout reason.  To enter a new multi-line reason, give - for the reason when prompted."
  (:bindings ((checkout-reason (variable-value-if-bound
                                'hope-checkout-reason :buffer buffer))
              (reason (loop (let ((reason
                                   (prompt-for-string :prompt (format nil "Reason for checkin (~A): "
                                                                      checkout-reason)
                                                      :default checkout-reason
                                                      :help "Enter a single-line checkin reason, or - to request more prompting")))
                              (unless (string= reason "Editing")
                                (return reason)))))))

  (when (string= reason "-")
    (setq reason nil))
  (ensure-buffer-saved buffer)
  (tell-hope "checkin -unit ~A ~@[-reason ~S~]"
	     unit reason)
  (show-hope-window)
  (revert-when-file-is-read-only pathname buffer)
  (delete-variable 'hope-checkout-reason :buffer buffer))

(defun revert-when-file-is-read-only (pathname buffer)
  (revert-when #'(lambda ()
                   (and (probe-file pathname)
                        (not (file-writable-p pathname))
		        (> (file-length pathname) 0)))
               buffer))

(defun revert-when-file-is-not-read-only (pathname buffer)
  (revert-when #'(lambda ()
                   (and (probe-file pathname)
                        (file-writable-p pathname)
                        (> (file-length pathname) 0)))
               buffer))

(defun revert-when (predicate buffer)
  (hope-sleep-until predicate)
  (revert-buffer-command t ; Pekka 09Nov94: stops prompts about auto-save files
                          buffer nil))

(defun hope-sleep-until (predicate)
  (loop (when (funcall predicate)
          (return))
        (mp:sleep-for-time 1))
  (mp:sleep-for-time 1) ;allow for hope being slow etc...
  )

(defun ensure-buffer-saved (buffer)
  (when (and (buffer-modified buffer)
             (prompt-for-y-or-n
              :prompt "Buffer is modified, save it first? "
              :default t))
    (save-file-command nil buffer)))

(defun get-reason (prompt)
  (let ((reason
         (prompt-for-string
          :prompt prompt
	  :default "-"
	  :help "Enter a single-line reason, or - to request more prompting")))
    (if (string= reason "-")
        nil
      reason)))

(def-hope-file-command "HOPE Add"
    "Add the current buffer as a new unit the compound for its directory"
  (:bindings ((reason (get-reason "Reason for adding unit: "))))

  (ensure-buffer-saved buffer)
  (tell-hope "add -unit ~A ~@[-reason ~S~]"
	     unit reason)
  (show-hope-window)
  (revert-when-file-is-read-only pathname buffer))

(def-hope-file-command "HOPE Remove"
    "Remove the latest version of the current buffer"
  (:bindings ((reason (get-reason "Reason for removing unit: "))))

  (tell-hope "remove -unit ~A ~@[-reason ~S~]"
	     unit reason)
  (show-hope-window))

(def-hope-file-command "HOPE Abandon"
    "Abandon the claim on the file for the current buffer and check the latest version out again"
  ()

  (delete-variable 'hope-checkout-reason :buffer buffer)
  (delete-file (buffer-pathname buffer))
  (tell-hope "checkout -ci-date -unit ~A"
	     unit)
  (show-hope-window)
  (revert-when-file-is-read-only pathname buffer)
  ;; do the abandon last as there is no way Lisp can check it succeeded.
  (tell-hope "abandon -unit ~A"
	     unit))

(def-hope-file-command "HOPE Abandon Preserving Working File"
    "Abandon the claim on the file for the current buffer"
  ()

  (delete-variable 'hope-checkout-reason :buffer buffer)
  (tell-hope "abandon -unit ~A"
	     unit)
  (show-hope-window))

(def-hope-file-command "HOPE Status"
    "Generate a status for the file for the current buffer"
  ()

  (tell-hope "status -unit ~A"
	     unit)
  (show-hope-window))

(def-hope-file-command "HOPE Report"
    "Generate a report for the file for the current buffer"
  ()

  (tell-hope "report -unit ~A"
	     unit)
  (show-hope-window))

(def-hope-file-command "HOPE Diff"
    "Generate a diff for the file for the current buffer"
  ()

  (ensure-buffer-saved buffer)
  (tell-hope "diff -unit ~A"
	     unit)
  (show-hope-window))

(def-hope-file-command "HOPE Diff Versions"
    "Generate a diff for the file for the current buffer"
  (:bindings ((ver1 (prompt-for-string :prompt "Oldest version: "))
              (ver2 (prompt-for-string :prompt "Newest version: "))))

  (when (string= ver1 "")
    (setq ver1 nil))
  (when (string= ver2 "")
    (setq ver2 nil))
  (ensure-buffer-saved buffer)
  (tell-hope "diff -unit ~A~@[ -ver ~A~]~:[~; -and~]~@[ -ver ~A~]"
	     unit ver1 (and ver1 ver2) ver2)
  (show-hope-window))

(def-hope-file-command "HOPE Command On File"
    "Run a HOPE command for the file for the current buffer"
  (:bindings ((command (prompt-for-string :prompt "HOPE Command: "))))

  (ensure-buffer-saved buffer)
  (tell-hope "~A -unit ~A"
	     command unit)
  (show-hope-window))

(defcommand "HOPE Command" (p)
     "Run a HOPE command for the file for the current buffer"
     "Run a HOPE command for the file for the current buffer"
  (let ((command (prompt-for-string :prompt "HOPE Command: ")))
    (ensure-hope-listener)
    (tell-hope "~A" command)
    (show-hope-window)))

(defcommand "HOPE Show Current Claims" (p)
     "Show the claims which the current image knows about."
     "Show the claims which the current image knows about."
  (let ((claim-count 0))
    (dolist (buffer *buffer-list*)
      (when-let (reason (variable-value-if-bound 'hope-checkout-reason :buffer buffer))
        (incf claim-count)
        (format t "~%~A: ~A~%" (buffer-name buffer) reason)))
    (format t "~%~A: ~d~%" "Count of current claims" claim-count)))


(bind-key "HOPE CheckIn" '#(#\control-\c #\i))
(bind-key "HOPE CheckOut" '#(#\control-\c #\o))
(bind-key "HOPE Diff" '#(#\control-\c #\d))


(defun make-hope-listener (&optional (owner (tk:current-root)
                                            #+comment
                                            (find-echo-contact t)))
  (let* ((process (make-command-run-tool owner "/usr/local/bin/hope")))
    (mp:process-wait "Waiting for hope to start"
                     'mp:process-event-queue process)
    (let ((queue (mp:process-event-queue process)))
      (mp:process-wait  "Waiting for hope windows"
			'clue::event-queue-windows queue)
      (let ((window (car (clue::event-queue-windows queue))))
        (mp:process-wait "Waiting for hope stream"
                         'slot-boundp window 'tk::stream)
        (let ((editor-window (slot-value (slot-value window 'tk::stream)
		                         'xlib:window)))
          editor-window)))))


;; ---------------------------------------------------------------------------
;; MJS 13Jul94: this definition should be made less CLUE/TK specific

(defun send-to-window (window function-spec)
  (mp::put-in-process-queue (clue:contact-event-queue
			     (window-text-pane window))
			    function-spec))
;; ---------------------------------------------------------------------------


(in-package "TOOLKIT")

(defmethod editor::make-command-run-tool ((self clue:contact) command)
  (fork-window #'make-shell
               (cond ((stringp command) command)
		     ((consp command) (car command))
		     (t (format nil "Shell tool ~A" command)))
               nil command))

(defmethod lw::destroyed-or-no-display-p ((contact contact))
  (or (xlib::display-dead (contact-display contact))
      (destroyed-p contact)))


(in-package "SYSTEM")

(defun directory-pathname-as-file-pathname (pathname)
  (let* ((pathname (pathname pathname))
	 (dir-list (%pathname-directory pathname)))
    (copy-pathname-modified pathname
			    :directory (butlast dir-list)
			    :name (car (last dir-list)))))

;; eof