;; Dylan editor support:

(in-package :editor)

(eval-when (:compile-toplevel)
  (system::without-warning-on-redefinition
   (load "DYLAN:emulator;lw;macros")))

(defun current-buffer-binding (name)
  (if (and (symbolp name) (a-dylan-mode-p (current-buffer)))
    (dylan::%runtime-resolve-name name :module-name (current-buffer-module))
    name))
  
;; The section parsing function gets the text form??

(setf (get 'dylan::define 'section-parsing-function)
      'section-parse-simple-tlf)

(setf (get 'dylan::define-method 'section-parsing-function)
      'section-parse-define-method-tlf)

(setf (get 'dylan::define-generic-function 'section-parsing-function)
      'section-parse-simple-tlf)

(defun section-parse-define-method-tlf (stream def)
  (let ((gf (current-buffer-binding (dylan::dylan-read stream)))
	(options nil)
	(option nil))
    (loop do (setq option (dylan::dylan-read stream))
	  while (not (listp option))
	  do (setq options (cons option options)))
    ;; options is the lambda list
    ;; lets assume they are all simple
    (let ((specializers nil))
      (catch 'done
	(mapcar (function (lambda (x)
			    (if (member x '(&rest &key &next 
                                            dylan::&return-types))
				(throw 'done nil)
                              (let ((specializer (if (consp x)
					      (car (cdr x))
					    'dylan::<object>)))
			        (setq specializers 
				      (cons specializer
					    specializers))))))
		option))
      (append '(dylan::define-method) 
	      (list gf)
	      (nreverse options)
	      (list (nreverse specializers))))))

;; Indentation: (global which is naff)

(setup-indent "define" 1)
(setup-indent "define-class" 2 2 2)
(setup-indent "define-method" 2 2 2)
(setup-indent "define-generic-function" 2 2 2)

(setup-indent "bind-methods" 1 2 2)
(setup-indent "method" 1)
(setup-indent "bind" 1 2)

(setup-indent "for" 1 2)
(setup-indent "for-each" 2 2 4)

;; Mode:

;;; The command to invoke Dylan Mode.

(defcommand "Dylan Mode" (p)
  "Put current buffer in Dylan mode." 
  "Put current buffer in Dylan mode."  
  (declare (ignore p))
  (setf (buffer-major-mode (current-buffer)) "Dylan")
  (shared-dylan-mode-startup (current-buffer)))

(defmode "Dylan" :major-p t :setup-function 'setup-lisp-mode
  :vars '(("Paren Pause Period" . nil)
	  ("Highlight Matching Parens" . t)
	  ("Comment Start" . ";")
	  ("Comment Begin" . ";")
	  ("Auto Fill Space Indent" . t) 
	  ("Indent Function" . indent-for-lisp)
	  ("Check Mismatched Parens" . :warn))
  :syntax-table *default-syntax-table*)

(bind-key "Lisp Insert )" #\) :mode "Dylan")

;; Files:

(define-file-type-hook ("dyl" "dylan") (buffer type)
  (declare (ignore type))
  (set-buffer-current-package buffer "DYLAN")
  (setf (buffer-major-mode buffer) "Dylan"))

;; Nasty defadvice: why not make these generic on the buffer class??

(defparameter *dylan-mode* (getstring "Dylan" *mode-names*))

(defun infix-dylan-mode-p (buf)
  (let ((mode (buffer-major-mode buf)))
    (eq mode *infix-dylan-mode*)))

;; andrewa, 22 May 1996 - use variable-value-if-bound since there are
;; some situations when it won't be bound yet.
(defun buffer-module (buffer)
  (variable-value-if-bound 'module :buffer buffer))

(defun current-buffer-module ()
  (buffer-module (current-buffer)))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro with-correct-readtable ((buf start end) &rest body)
    (with-unique-names (buffer)
    `(let ((,buffer ,buf))
      ;; Hack because I have no idea what's actually going on here
      (process-file-options ,buffer (buffer-pathname ,buffer))
      (let ((mode (buffer-major-mode ,buffer))
            (header (and ,start
		         (or (eq ,start t) (= (point-position ,start) 0)))))
        (cond
         ((eq mode *dylan-mode*)
	  (dylan::reading-as-translated-dylan
	   (:language 'dylan::prefix-dylan
	    :module    (buffer-module ,buffer)
	    :header    header)
	   ,@body))
         ((eq mode *infix-dylan-mode*)
	  (dylan::reading-as-translated-dylan
	   (:language 'dylan::infix-dylan
	    :module    (buffer-module ,buffer)
	    :header    header)
	   ,@body))
         (t 
	  ,@body))))))

)

(defadvice 
  (process-file-options dylan-advice :around) (buf path)
  (let ((res (call-next-advice buf path)))
    (when (eq (buffer-major-mode buf) *dylan-mode*)
      (handler-case
        (with-input-from-region (stream (buffer-%start buf) (buffer-%end buf))
          (multiple-value-bind 
              (lang mod) (dylan::read-dylan-file-header-values stream)
            (let ((lang 
		    (intern (string-upcase lang) dylan:*the-dylan-package*))
                  (mod
                    (intern (string-upcase mod) dylan:*the-dylan-package*)))
              (case lang
	        ((dylan::infix-dylan) 
                  (setf (buffer-major-mode buf) "Infix-Dylan"))
	        ((dylan::prefix-dylan) 
                  (setf (buffer-major-mode buf) "Dylan")))
	      (defhvar "Module" 
	        "Holds the buffer's module"
	        :buffer buf
	        :value mod)
	      (defhvar "Language" 
	        "Holds the buffer's language"
	        :buffer buf
	        :value lang)
	      (setf (variable-value 'module) mod))))
        (error (c)
          (message "Caught - ~a~%" c))))
    res))

(defadvice (default-modeline-function-function dylan-advice :around) (window)
  (let ((buf (window-buffer window)))
    (if (not (a-dylan-mode-p buf))
        (call-next-advice window)
      (multiple-value-bind
          (perms name package mode pc disp path) 
          (call-next-advice window)
        (declare (ignore package))
        (values perms
	        name
	        (string (buffer-module buf))
	        mode
	        pc
	        disp
	        path)))))

(defun read-dylan-form (start end)
  (with-correct-readtable ((point-buffer start) start end)
    (let ((text (points-to-string start end)))
      (with-input-from-string (stream text)
        (dylan::read-infix-dylan stream)))))
            
(defadvice (region-eval dylan-advice :around) (buf start end &optional print)
  (with-correct-readtable (buf start end)
    (call-next-advice buf start end print)))

(defadvice (region-compile dylan-advice :around) (buf start end 
                                                      &optional print)
  (with-correct-readtable (buf start end)
    (call-next-advice buf start end print)))

(defadvice 
  (compile-file-with-message generic-env :around) (file &optional load-p)
  (let ((mode (buffer-major-mode (current-buffer))))
    (if (or (eq mode *dylan-mode*) (eq mode *infix-dylan-mode*))
      (dylan::reading-as-translated-dylan (:header t)
        (call-next-advice file load-p))
      (call-next-advice file load-p))))

(defadvice
  (macroexpand-form-command dylan-advice :around) (p)
  (with-correct-readtable ((current-buffer) nil nil)
    (call-next-advice p)))

(defadvice
  (walk-form-command dylan-advice :around) (p)
  (with-correct-readtable ((current-buffer) nil nil)
    (call-next-advice p)))

;;; Address problems with interaction between dylan-read-token and m-.

(defadvice (editor:prompt-for-symbol dylan-prompt-for-symbol :around)
    (prefix &rest rest-args)
  (declare (dynamic-extent rest-args))
  (current-buffer-binding (apply #'call-next-advice prefix rest-args)))


;;----------------------------------------------------------------------------
;; Find the next and previous definition forms
;;----------------------------------------------------------------------------

(defvar *dylan-defining-form-prefix* "define ")

(defun dylan-defining-form-p (point)
  (let ((prefix
         (with-point ((end point))
           (character-offset end (length *dylan-defining-form-prefix*))
           (editor:points-to-string point end))))
    (string-equal prefix *dylan-defining-form-prefix*)))

(defun next-dylan-defining-form-start (point &optional (offset 1))
  (line-offset point offset)
  (loop do (line-start point)
        when (dylan-defining-form-p point)
        return point
        while (line-offset point offset)))


;;----------------------------------------------------------------------------
;; Make 'End of Defun' and 'Beginning of Defun' work  
;; (Meta-control-a and meta-control-e, respectively)
;;----------------------------------------------------------------------------

(defun dylan-backward-top-level-offset (start offset)
  (with-point ((point start))
    (unless (loop for count below offset
                  unless (next-dylan-defining-form-start point -1)
                  return t)
      (move-point start point)
      t)))

(defun next-dylan-end-of-form (point offset)
  (when (loop for count below offset
	      unless (next-dylan-defining-form-start point 1)
	      return t)
    (buffer-end point))
  (loop when (eq (previous-character point) #\;)
	return t
	while (character-offset point -1)))

;; andrewa, 28 Sep 1995 - this has a ropey definition of the end: find
;; the following form, and then back up to the first semicolon. It should
;; be much smarter about comments, in particular.
(defun dylan-forward-top-level-offset (start offset)
  (with-point ((point start))
    (line-offset point 1)
    (unless (next-dylan-defining-form-start point -1)
      (next-dylan-defining-form-start point 1))
    (if (next-dylan-end-of-form point offset)
        (progn
          (when (point<= point start)
	    (move-point point start)
	    (next-dylan-defining-form-start point 1)
	    (next-dylan-end-of-form point 1))
          (move-point start point)
          t)
      (progn
        (unless (next-dylan-defining-form-start point 1)
          (buffer-end point))
        (next-dylan-end-of-form point 1)))))

(defadvice (backward-top-level-offset dylan-advice :around) (point offset)
  (if (infix-dylan-mode-p (point-buffer point))
      (dylan-backward-top-level-offset point offset)
    (call-next-advice point offset)))

(defadvice (forward-top-level-offset dylan-advice :around) (point offset)
  (if (infix-dylan-mode-p (point-buffer point))
      (dylan-forward-top-level-offset point offset)
    (call-next-advice point offset)))


;;----------------------------------------------------------------------------
;; Some hacks to get editor::section-region to work, which is what the
;; editor uses to determine where to place a M-. (amongst other things)
;;----------------------------------------------------------------------------

(defadvice (section-region dylan-advice :around) (&rest args
                                                        &key buffer
                                                        &allow-other-keys)
  (if (infix-dylan-mode-p (or buffer (current-buffer)))
      (apply 'section-dylan-region args)
    (apply #'call-next-advice args)))

(defadvice (section-form dylan-advice :around) (start end)
  (if (infix-dylan-mode-p (point-buffer start))
      (section-dylan-form start end)
    (call-next-advice start end)))

(defun next-word (point)
  (skip-whitespace-internal nil point (point-buffer point))
  (with-point ((start point))
    (word-offset point 1)
    (points-to-string start point)))

(defun dylan-dspec-for-form (form module-name)
  (flet ((resolved-name (name)
           (dylan::%runtime-resolve-name name :module-name module-name)))
    (case (first form)
      (dylan::infix-define-method
        (multiple-value-bind 
            (all-binds vars specs) (dylan::parse-lambda-list (fourth form))
          (declare (ignore all-binds vars))
          `(dylan::define-method ,(resolved-name (third form)) ,specs)))
      (dylan::infix-define-class
       `(dylan::define-class ,(resolved-name (third form))))
      ((dylan::infix-define-constant dylan::infix-define-variable)
       `(dylan::define ,(resolved-name (dylan::variable-name (third form)))))
      (t
        nil))))

(defun section-dylan-form (start end)
  (with-point ((point start))
    (word-offset point 1) ;; skip the 'define '
    (let* ((buffer (point-buffer start))
           (form 
            (handler-case
                (read-dylan-form start end)
              (end-of-file
               (progn :eof)))))
      (if (eq form :eof)
          (values nil :eof)
        (when-let (dspec
                   (and form
                        (dylan-dspec-for-form form (buffer-module buffer))))
          (make-instance 'definition
		         :buffer (point-buffer start)
		         :start (point-to-offset start)
		         :end (point-to-offset end)
		         :dspec dspec))))))

(defun next-dylan-definition (start end)
  (unless (dylan-defining-form-p start)
    (next-dylan-defining-form-start start))
  (with-point ((form-end start))
    (let* ((another-form-p (next-dylan-defining-form-start form-end)))
      (when (point<= form-end end)
        (section-dylan-form start
                            (if another-form-p
		                form-end
	                      (buffer-%end (point-buffer start))))))))

(defun section-dylan-region (&key buffer
                                  (start (buffer-%start buffer))
                                  (end (buffer-%end buffer))
			          (done #'(lambda (x)
				            (make-instance 'definition-group
						           :components x))))
  (declare (ignore buffer))
  (let ((forms
         (let (eof)
           (with-point ((point start))
	     (loop for definition = (multiple-value-bind
                                        (definition end-of-file-p)
                                        (next-dylan-definition point end)
                                      (if end-of-file-p
                                          (setq eof t)
                                        definition))
		   when definition
		   collect definition
		   while (and (not eof)
                              (next-dylan-defining-form-start point)))))))
    (funcall done forms)))

;; eof
