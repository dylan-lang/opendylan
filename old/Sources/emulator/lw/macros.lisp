;;----------------------------------------------------------------------------
;;
;; editor/macros.lisp
;;
;;----------------------------------------------------------------------------
;; 18/12/91 - MJS delete the newly created points in with-point, rather than
;; the point pointed to by the bound variable, as this could have been setq'd.
;; 20/12/91 - AA export collect-undo as it is needed inside defcommands
;; 01/07/92 - DHD clearer error message for defcommand with nil lambda-list
;;----------------------------------------------------------------------------

(in-package 'editor)

(proclaim '(optimize (safety 2)))

(eval-when (compile) 
  (editor-comp-init))

(export '(
          collect-undo
          ))

(defmacro move-the-hole-to-the-end(bigline)
  `(let ((b ,bigline))
     (move-the-hole b (contents-size b))))

(defmacro find-attribute(point attribute &optional (test ''not-zerop))
  `(id-find-attribute ,point ,attribute ',(gensym) ,test))

(defmacro reverse-find-attribute(point attribute &optional (test ''not-zerop))
    `(id-reverse-find-attribute ,point ,attribute ',(gensym) ,test))


(defmacro char-find-attribute(point attribute &optional (test ''not-zerop))
  `(id-x-find-attribute ,point ,attribute ',(gensym) ,test))

(defmacro char-reverse-find-attribute(point attribute &optional (test ''not-zerop))
  `(id-reverse-x-find-attribute ,point ,attribute ',(gensym) ,test))

(defmacro invoke-hook* (hook-var &rest args)

  `(dolist (f (variable-value ,hook-var :current nil))
     (funcall f ,@args)))

;; this takes only the global value
(defmacro invoke-global-hook* (hook-var &rest args)
  `(dolist (f (variable-value ,hook-var :global nil))
     (funcall f ,@args)))

(defmacro value (name)
  `(variable-value ',name :current nil))

(defmacro setv (name new-value)
  `(%set-variable-value ',name :current nil ,new-value))

(defmacro with-variable-object (name kind where &body forms)
  `(let ((obj (get-variable-object ,name ,kind ,where)))
     (declare (type variable-object obj))
     (unless obj (setq obj (undefined-variable-error ,name ,kind ,where)))
     ,@forms))

;;; string-to-keyword  --  Internal
;;;
;;;    Mash a string into a Keyword.
;;;

(defun string-to-keyword (string)
  (intern (nsubstitute #\- #\space
		       (the simple-string (string-upcase string)))
	  (find-package "KEYWORD")))

;;----------------------------------------------------------------------------
;; Macros to add and delete hook functions.
;;
;;   add-hook
;;   remove-hook
;;
;;   add-global-hook
;;   remove-global-hook
;;----------------------------------------------------------------------------

(defmacro add-hook (place hook-fun)
  (if (symbolp place)
      `(pushnew ,hook-fun (value ,place))
      `(pushnew ,hook-fun ,place)))

(defmacro remove-hook (place hook-fun)
  (if (symbolp place)
      `(setf (value ,place) (delete ,hook-fun (value ,place)))
      `(setf ,place (delete ,hook-fun ,place))))

(defmacro add-global-hook (place hook-fun)
  `(fun-add-global-hook ',place ,hook-fun))

(defmacro remove-global-hook (place hook-fun)
  `(setf (variable-value ',place :global nil)
	 (delete ,hook-fun (variable-value ',place :global nil))))

(defvar *auto-load-path* nil)
(defvar *auto-load-system* nil)
(defvar *auto-load-keyword* nil)

;;; defcommand  --  Public
;;;
;;;    Define a new command.
;;;
;; 29/01/93 AA - add undefcommand, and command-name-to-function

(defmacro defcommand (name lambda-list command-doc function-doc
			   &body forms)
  (unless (stringp function-doc)
	  (push function-doc forms)
	  (setq function-doc ""))
  (unless (stringp command-doc)
	  (push command-doc forms)
	  (setq command-doc ""))
  (unless (stringp function-doc)
	  (push function-doc forms))
  (when (atom lambda-list)
    (error (format nil 
  "The lambda list for defcommand must have at least one argument. Lambda list given: ~a" lambda-list)))
  (let ((function-name (command-name-to-function name)))
    `(compiler::top-level-form
      (editor-command ,name)
      ,(if *auto-load-path*
	   `(ccl::load-on-demand :functions '(,function-name)
				 :pathname ,*auto-load-path*
				 :system ',*auto-load-system*
				 :keyword ',*auto-load-keyword*)
	 `(defun ,function-name ,lambda-list ,function-doc
           #|
            (command-hook ',function-name
                          ,@(subseq lambda-list 0
                                    (position-if
                                     #'(lambda (x)
					 (member x lambda-list-keywords))
                                                 lambda-list)))
|#
	   ,@forms))
      (export ',function-name 'editor)
      (make-command ',name ,command-doc ',function-name)
      ',function-name)))

;; 01 Sep 1993, AA - make delete-command do everything this used to do...
(defmacro undefcommand (name &rest rest)
  (declare (ignore rest))
  `(delete-command ,name))

(defmacro auto-load-command (name pathname &optional keyword system
                                  (directory "editor/"))
  `(compiler-let ((*auto-load-path*  ,(concatenate 'simple-string
						   directory
						   pathname))
		  (*auto-load-system* ',system)
		  (*auto-load-keyword* ',keyword))
      (defcommand ,name (p) nil nil nil)))


;;----------------------------------------------------------------------------
;; 01 Sep 1993, AA - add define-command-synonym
;;----------------------------------------------------------------------------

(defmacro define-command-synonym (new-name old-name)
  `(compiler::top-level-form (editor-command ,new-name)
     (make-command-synonym ,new-name ,old-name)))


;;; Parse-Forms  --  Internal
;;;
;;;    Used for various macros to get the declarations out of a list of
;;; forms.
;;;
(eval-when (compile load eval)
(defmacro parse-forms ((decls-var forms-var forms) &body gorms)
  "Parse-Forms (Decls-Var Forms-Var Forms) {Form}*
  Binds Decls-Var to leading declarations off of Forms and Forms-Var
  to what is left."
  `(do ((,forms-var ,forms (cdr ,forms-var))
	(,decls-var ()))
       ((or (atom ,forms-var) (atom (car ,forms-var))
	    (not (eq (caar ,forms-var) 'declare)))
	,@gorms)
     (push (car ,forms-var) ,decls-var)))
 )

(defmacro with-point (point-bindings &rest forms)
  "With-Point ({(Point Pos [Kind])}*) {declaration}* {form}*
  With-Point binds a variable named Point to a point specified by Pos.  This
  point is :temporary, or of kind Kind.  The forms are then evaluated."
  (do ((bindings point-bindings (cdr bindings))
       (let-slots ())
       (x-let-slots ())
       (cleanup ()))
      ((null bindings)
       (if cleanup
	   (parse-forms (decls forms forms)
	     `(let ,(nreverse let-slots)
		,@decls
		(let ,(nreverse x-let-slots)
		  (unwind-protect
		      (progn ,@forms)
		    ,@cleanup))))
	   `(let ,(nreverse let-slots) ,@forms)))
    (let ((name (caar bindings))
	  (pos (cadar bindings))
	  (type  (caddar bindings) ))
      (cond ((not (eq type :temporary))
	     (with-unique-names (x-name)
	       (push `(,name (copy-point ,pos ,type)) let-slots)
	       (push `(,x-name ,name) x-let-slots)
	       (push `(delete-point ,x-name) cleanup)))
	    (t
	     (push `(,name (copy-point ,pos :temporary)) let-slots))))))

(defmacro use-buffer (buffer &body forms)
  "Use-Buffer Buffer {Form}*
  Has The effect of making Buffer the current buffer during the evaluation
  of the Forms.  For restrictions see the manual."
    `(let ( (*current-buffer* ,buffer))
	 ,@forms))

; Altered on 14/3/89. The binding of *e-current-window* seems essential,
; but the other actions associated with %set-current-window do not. MSM.

(defmacro use-buffer-and-window (buffer window &body forms)
  "Use-Buffer Buffer {Form}*
  Has The effect of making Buffer the current buffer during the evaluation
  of the Forms.  For restrictions see the manual."
    `(let* ((*current-buffer* ,buffer)
	    (*current-window* ,window)
	    (*e-current-window* *current-window*))
       ,@forms))



;;;; Editor-Error and stuff
;;;

;; 04 Aug 1993, AA - add editor variable cerror-for-editor-error
;;; moved these to interp.lisp  Y 4/8/93





(defmacro catch-default-editor-error(&rest body)
  `(catch 'command-loop-catcher ,@body))


(defvar *editor-error-handler* 'default-editor-error-handler
  "This variable contains the function which Editor-Error invokes with
  its arguments.")


;;; The call to signal have been removed from here, so anywhere where
;;;  handler-cse was use to catch it needs to ue catch-editor-error



(defmacro catch-editor-error (&body forms)
  "Catch-Editor-Error {Form}*
  If an editor-error is signaled during the evaluation of the Forms,
  abort their execution and return NIL.  If no editor-error occurs,
  then the value of the last Form is returned."
  `(let ((*editor-error-handler* 'throw-to-catch-editor))
     (catch 'catch-editor-error-catcher ,@forms)))

(defun report-error (&rest args)

  (editor-beep)
  (apply 'message args)
  (setq *can-use-echo-area* nil)
  (queue-message-remove))

;;; do-strings  --  Public
;;;
;;; Iterate over the entries in a string table using the NEXT link.
;;;
(defmacro do-strings ((string-var value-var table &optional result) &body forms)
  "Do-Strings (String-Var Value-Var Table [Result]) {declaration}* {form}*
  Iterate over the strings in a String Table.  String-Var and Value-Var
  are bound to the string and value respectively of each successive entry
  in the string-table Table in alphabetical order.  If supplied, Result is
  a form to evaluate to get the return value."
  (let ((tab (ccl::a-gensym))
	(current (ccl::a-gensym))
	(i (ccl::a-gensym))
	(vec (ccl::a-gensym)))
    `(let* ((,tab ,table)
	    (,vec (string-table-table ,tab)))
       (dotimes (,i (string-table-num-entries ,tab) ,result)
	 (declare (fixnum ,i))
	 (let* ((,current (svref ,vec ,i))
		(,string-var (string-table-entry-proper ,current))
		(,value-var (string-table-entry-value ,current)))
	   (declare (simple-string ,string-var))
	   ,@forms)))))

;;; preserve the current window appearance, currently only warping back on exit
;;; 

(defmacro with-warping-back (&body  forms)
    `(let ((cur-window *current-window*))
      (unwind-protect
          (progn ,@forms)
          (warp-to-contact (window-text-pane cur-window)))))
;;; Command-Case  --  Public
;;;
;;;    Grovel the awful thing and spit out the corresponding Cond.  See Echo
;;; for the definition of command-case-help and logical char stuff.
;;;
(eval-when (compile load eval)
(defun command-case-tag (tag char)
  `(logical-char= ,char ,tag))
); eval-when (compile load eval)
;;;  
;;; Modified to expect a return value from command-case-help which is treated as
;;; the character pressed - KMM 11/12/90

(defmacro command-case ((&key (change-window t) character
			      (prompt "Command character: ")
			      (help "Choose one of the following characters:")
			      (bind (ccl::a-gensym)))
			&body forms)
  (do* ((forms forms (cdr forms))
	(form (car forms) (car forms))
	(cases ())
	(bname (ccl::a-gensym))
	(upper (ccl::a-gensym))
	(again (ccl::a-gensym))
	(n-prompt (ccl::a-gensym))
	(n-change (ccl::a-gensym))
        (choice (ccl::a-gensym))
	(docs ())
        ;;MJS 14/12/92: removed illegal "Invalid command" from call to editor-beep
	(t-case `(t (editor-beep) (reprompt))))
       ((atom forms)
	`(macrolet ((reprompt ()
		      `(progn
			(setq ,',bind (prompt-for-character*
				       ,',n-prompt ,',n-change))
			(go ,',AGAIN))))
	   (block ,bname
	     (let* ((,n-prompt ,prompt)
		    (,n-change ,change-window)
		    (,bind ,(or character
				`(prompt-for-character* ,n-prompt ,n-change))))
	       (tagbody
		,AGAIN
		 (let ((,upper  ,bind))
		   (return-from
		    ,bname
		    (cond
		     ,@(nreverse cases)
		     ((logical-char= ,upper :abort) (editor-error))
		     ((logical-char= ,upper :help)
                      (let ((,choice (command-case-help ,help ',(nreverse docs))))
                        (if ,choice
                          (progn (setq ,bind ,choice)
                            (go ,again))
		          (reprompt))))
		     ,t-case))))))))
    
    (cond ((atom form)
	   (error "Malformed Command-Case clause: ~S" form))
	  ((eq (car form) t)
	   (setq t-case form))
	  ((or (< (length form) 2)
	       (not (stringp (second form))))
	   (error "Malformed Command-Case clause: ~S" form))
	  (t
	   (let ((tag (car form))
		 (rest (cddr form)))
	     (cond ((atom tag)
		    (push (cons (command-case-tag tag upper) rest) cases)
		    (setq tag (list tag)))
		   (t
		    (do ((tag tag (cdr tag))
			 (res () (cons (command-case-tag (car tag) upper) res)))
			((null tag)
			 (push `((or ,@res) . ,rest) cases)))))
	     (push (cons tag (second form)) docs))))))


;;;; Some random macros used everywhere.

(defmacro strlen (str)
  `(length (the simple-string ,str)))

(defmacro neq (a b)
  `(not (eq ,a ,b)))


;;;; With-Input & With-Output macros
;;;;

(defvar *free-editor-output-streams* ()
  "This variable contains a list of free editor output streams.")

(defmacro with-output-to-point ((var point &optional (buffered ':line))
			       &body gorms)
  "With-Output-To-Point (Var Point [Buffered]) {Declaration}* {Form}*
  During the evaluation of Forms, Var is bound to a stream which inserts
  output at the permanent point Point.  Buffered is the same as for
  Make-editor-Output-Stream."
  (parse-forms (decls forms gorms)
    `(let ((,var (pop *free-editor-output-streams*)))
       ,@decls
       (if ,var
	   (modify-editor-output-stream ,var ,point ,buffered)
	   (setq ,var (make-editor-output-stream ,point ,buffered)))
       (unwind-protect
	 (progn ,@forms)
	 (free-editor-stream ,var)))))

(defvar *free-editor-region-streams* ()
  "This variable contains a list of free Editor input streams.")




(defmacro with-input-from-region ((var start end) &body gorms)
  "With-Input-From-Region (Var Region) {Declaration}* {Form}*
  During the evaluation of Forms, Var is bound to a stream which
  returns input from Region."
  (parse-forms (decls forms gorms)
    `(let ((,var (pop *free-editor-region-streams*)))
       ,@decls
       (if ,var
	   (setq ,var (modify-editor-region-stream ,var ,start ,end))
	   (setq ,var (make-editor-region-stream ,start ,end)))
       (unwind-protect
	 (progn ,@forms)
	 (free-region-stream ,var)))))

;;; With-Random-Typeout  --  Internal
;;;
;;;    Set up the *random-output-stream* and then blow it away.
;;;
;;; Changed on 1/12/88 so that the start of the new part of the
;;; buffer is displayed. (MSM).
(defmacro with-random-typeout ((var n &optional name modes) &body body/decls)
  (declare (ignore n))
  (let ((*dont-update-screen* t))
    (multiple-value-bind
        (body doc decls)
        (compiler::parse-body1 body/decls)
      (declare (ignore doc))
      `(let* ((,var (make-output ,name ,modes))
	      (buff (point-buffer (editor-output-point ,var)))
	      (old-end (buffer-%end buff))
	      (old-line (point-bigline old-end))
	      (old-offset (point-offset old-end))
	      point)
	,(when decls
	   `(declare ,@decls))
	(multiple-value-prog1
	    (progn ,@body)
	  (setq point (buffer-point buff))
	  (push-buffer-point  point)
	  (move-to-position point old-offset old-line)
	  (ccl::soft-force-output ,var)
          (ccl::wake-up-window buff))))))

;; Altered on 25/1/89 to return the value given by the body. (MSM)
(defmacro handle-lisp-errors (&body body)
  `(multiple-value-bind 
    (ret error)
    (ccl::ignore-errors ,@body)
    (if (and error (not ret))
        (editor-error "~a" error)
        (temp-message "~a" ret))
    ret))

(defmacro define-file-type-hook (type-list (buffer type) &body body)
  "Define-File-Type-Hook ({Type}*) (Buffer Type) {Form}*
  Define some code to be evaluated when a file having one of the specified
  Types is read by a file command.  Buffer is bound to the buffer the
  file is in, and Type is the actual type read."
  (let ((fun (ccl::a-gensym)) (str (ccl::a-gensym)))
    `(flet ((,fun (,buffer ,type) ,@body))
       (dolist (,str ',(mapcar #'string-downcase type-list))
	 (setf (cdr (or (assoc ,str *file-type-hooks*  
			       :test #'string=)
			(car (push (cons ,str nil)
				   *file-type-hooks*))))
	       #',fun)))))


;;;; File options and file type hooks.

(defmacro define-file-option (name lambda-list &body body)
  "Define-File-Option Name (Buffer Value) {Form}*
  Defines a new file option to be user in the -*- line at the top of a file.
  The body is evaluated with Buffer bound to the buffer the file has been read
  into and Value to the string argument to the option."
  (let ((name (string-downcase name)))
    `(setf (cdr (or (assoc ,name *mode-option-handlers* 
			   :test #'string=)
		    (car (push (cons ,name nil) 
			       *mode-option-handlers*))))
	   #'(lambda ,lambda-list ,@body))))

;;;  Do a find-character-with-attribute on the *losing-character-mask*.
(defmacro %fcwa (str start end mask)
  `(ccl::%sp-find-character-with-attribute
    ,str ,start ,end *losing-character-mask* ,mask))

;;; Get the print-representation of a character.
(defmacro get-rep (ch)
  `(svref *print-representation-vector* (char-code ,ch)))

;; normally use theone with out %, and then then the setf is doing the right 
;; things
(defun buffer-major-mode(x)
  (buffer-%major-mode x))


;;----------------------------------------------------------------------------
;; Undo handling
;;----------------------------------------------------------------------------

;; This record the region before of tp and tp1 in a string, evaluate the body
;; with *dont-undo* t, and then record the changes that were made for undo.
;; The region of tp1 and tp2 is called %region%, and can be used in the body,

(defmacro recording-for-undo (point1 point2 &rest body)
  (let ((old-string (ccl::a-gensym "old-string-"))
	(tp1 (ccl::a-gensym "tp1-"))
	(end (ccl::a-gensym "end-"))
        (dont (ccl::a-gensym "dont-"))
	(start (ccl::a-gensym "start-"))
	(changed (gensym "changed-"))
	(tp2 (ccl::a-gensym "tp2-")))
    
    `(let* ((,dont *dont-undo*)
            (,tp1 ,point1)
	    (,tp2 ,point2)
	    (,changed (buffer-modified (point-buffer ,tp1)))
	    (,start  (unless ,dont (copy-i-point ,tp1 :before-insert)))
            (,end    (unless ,dont (copy-i-point ,tp2 :after-insert)))
	    (,old-string (unless ,dont (points-to-string ,tp1 ,tp2))))
       (unwind-protect
	   (let ((*dont-undo* t))
	     ,@body)
	 (progn
	   (unless ,dont
               (record-replace-region ,start ,end ,old-string ,changed)
	       (delete-it ,start)
	       (delete-it ,end)))))))

(defmacro collect-undo (buffer &body body)
  (declare (ccl::lambda-list buffer &body body))
  (let ((undo (gensym))
	(buffer-sym (gensym "buffer-"))
	(changed (gensym "changed-")))
    `(let* ((,undo nil)
	   (,buffer-sym ,buffer)
	   (,changed (buffer-modified ,buffer-sym)))
       (unwind-protect
	   (let ((*collect-undo* nil))
	     (sys::fast-multiple-value-prog1 (progn ,@body)
					     (setq ,undo *collect-undo*)))
	 (unless *dont-undo*
		 (record-undo-in-buffer ,buffer-sym ,undo nil ,changed ))))))
 
;; binds %point% and %mark% to current-point and current-mark, but in the
;right order

(defmacro with-buffer-point-and-mark ((buffer &key no-error-p) &rest body)
  (rebinding (buffer)
    `(let ((%point% (buffer-point ,buffer))
	   (%mark% (buffer-mark ,buffer ,no-error-p)))
      (if (and %mark% (point> %point% %mark%))
	  (rotatef %point% %mark%))
      ,@body)))

(defmacro with-point-and-mark( &rest body)
  `(let ((%point% (current-point))
	(%mark% (current-mark )))
    (if (point> %point% %mark%)
	(rotatef %point% %mark%))
    ,@body))

;; some p[laces we want to know if we change the order

(defmacro with-point-and-mark-e(&rest body)
  `(let ((%point% (current-point))
	(%mark% (current-mark))
	(%order% nil))
    (when (point> %point% %mark%)
	  (setq %order% t)
	  (rotatef %point% %mark%))
    ,@body))

;; display the message during evaluation of the body, and then display
;; done
(defmacro with-a-message (message &body body)
  `(progn
     (clear-echo-area ,message t)
     (redisplay)
     ,@body
     (clear-echo-area  "Done" :time)))

;; bind %temp% to a temporary file, and delete it on exit
(defmacro with-temp-file(&rest body)
  `(let ((%temp% (ccl-extensions::make-temp-file)))
     (unwind-protect
	 (progn ,@body)
       (delete-file %temp% :no-error))))

(defmacro save-excursion (&rest body)
  `(let* ((%save-point% (copy-point (current-point) :temporary))
	  (%is-no-mark% (zerop (ring-length
				(buffer-points-ring (current-buffer)))))
	  (%save-mark% (if %is-no-mark%
			   nil
			 (copy-point (current-mark) :temporary)))
	  %res%)
     (unwind-protect 
	 (setq %res% (progn ,@body))
	 (move-point (current-point) %save-point%)
       (let ((ring (buffer-points-ring (current-buffer))))
	 (if %is-no-mark%
	     (if (not (zerop (ring-length ring)))
		 (progn
		   (when *verbose*
		     (format *special-trace-output*
			     "save-excursion has to set the ring-length to 0"))
		   ;;		   (editor-error "Mark has been set unexpectedly")
		   ))
	   (when (point-bigline %save-mark%)
	     (setf (ring-ref ring 0) 
		   (move-point (ring-ref ring 0) %save-mark%))))
	 %res%))))



;;;macros for checking if we are looking at the last line in the window (the one after
;;; th elast one displayed ) .


(defmacro is-last-line(x)
  `(not ,x))

(defmacro create-last-line ()
   nil)

(defmacro with-temp-buffer ((buffer &rest options) &body body)
  `(let ((,buffer (make-buffer ,@options)))
    (unwind-protect
        (progn ,@body)
        (delete-buffer ,buffer t))))


(defmacro restore-point (&rest body)
  (let ((old-offset (gensym "offset-"))
	(res (gensym "res-")))
    `(let ((,old-offset (find-point-offset (current-buffer) (current-point)))
	   ,res)
       (setq ,res (progn ,@body))
       (find-place-in-buffer ,old-offset (current-buffer))
       ,res)))


(defmacro with-buffer-bindings-for-eval ((buffer &optional package) &body body)
  (let ((package-name (gensym))
	(buffer-var (gensym)))
    `(let* ((,package-name (or ,package
			       (variable-value-if-bound
				'current-package)))
	    (,buffer-var ,buffer)
	    (*package* (cond ((null ,package-name) *package*)
			     ((packagep ,package-name) ,package-name)
			     (t (find-package ,package-name))))
	    (compiler::*input-pathname*
	     (if ,buffer-var
		 (buffer-pathname ,buffer-var))))
       ,@body)))


(defmacro with-output-to-help-window ((stream &rest options) 
                                      &body body)
  `(with-output-to-help-window-1 #'(lambda (,stream)
                                       ,@body)
                                 ,@options))


;; 17/12/92 AA - add buffer-package-to-use function which takes account
;; of the variable *use-previous-in-package*.

(defmacro with-package-for-point ((buffer &key point) &body body)
  `(let ((*package* (buffer-package-to-use (or ,point ,buffer))))
     ,@body))

;; 15 Sep 1993, AA - add this as a silent version of in-buffer-eval
;; 06 Oct 1993, AA - add start-message and end-message
(defmacro with-compilation-environment-at-point ((point
                                                  &key
                                                  start-message
                                                  end-message)
                                                 &body body)
  (rebinding (point)
    `(let* ((buffer (point-buffer ,point))
	    (*can-use-echo-area* *message-on-eval*)
	    (*standard-output* (buffer-stream buffer))
	    (*trace-output* *standard-output*)
	    (compiler::*input-pathname* (buffer-pathname buffer)))
      (with-package-for-point (buffer :point ,point)
        ,@(when start-message
            `((when *can-use-echo-area*
                (clear-echo-area ,start-message t)
                (redisplay))))
        ,@body
        ,@(when end-message
            `((when *can-use-echo-area*
                (clear-echo-area ,end-message t)
                (redisplay))))))))

;; 15 Sep 1993, AA - separate out with-compilation-environment-at-point
;; 06 Oct 1993, AA - add start-message and end-message
(defmacro in-buffer-eval (buffer &body body)
  `(with-compilation-environment-at-point ((buffer-%start ,buffer)
                                           :start-message "Evaluating.."
                                           :end-message "Finished Evaluating")
    ,@body))

(defmacro output-eval-results(stream &rest body)
  `(in-output-eval-results ,stream (multiple-value-list ,@body)))
  
;; this macro  duplicates that in the toolkit but is designed
;; to do nothing when the environment is not there.


(defmacro  with-random-typeout-to-window ((the-contact) &body body)
  (let
      ((stream (gensym))
       (normal-exit (gensym))
       (contact (gensym))
       )
    `(let*
        ((,contact ,the-contact)
	 (,stream (and ,contact (ccl::pop-up-stream-window ,contact)))
	 (,normal-exit nil)
	 )
      (unwind-protect 
	  (progn ,@body
	    (setq ,normal-exit t))
          (and ,contact (ccl::rto-ask-for-space ,stream ,normal-exit))))))

(defmacro with-random-typeout-to-current-window (&body body)
  `(with-random-typeout-to-window
       ((let ((window (current-window)))(and window (window-text-pane window))))
       ,@body))

(defmacro list-offset (point offset)
  `(form-offset ,point ,offset nil 0))



(defmacro modifying-buffer (buffer &body forms)
  "Does groovy stuff for modifying buffers."
  `(without-interrupts 
    (let ((%modified%  (prepare-buffer-for-modification ,buffer)))
      (declare (ignore %modified%))
      (let ((*buffer-change* (buffer-first-change ,buffer)))
	(declare (special *buffer-change*))
	(prog1 (ccl::allow-stack-overflow
		,@forms)
	  (setf (buffer-first-change ,buffer) *buffer-change*)
	  (check-after-modifying-buffer buffer))))))
