;; Lifted from ~clc/editor/c-mode.lisp

(in-package "EDITOR")

;; for RECORDING-UNDO
(eval-when (:compile-toplevel)
  (system::without-warning-on-redefinition
   (load "DYLAN:emulator;lw;macros")))

;;----------------------------------------------------------------------------
;; Make the syntax for #\. behave properly
;;----------------------------------------------------------------------------

;; First attempt at making . behave properly, but this doesn't seem
;; to work for some reason... I'm investigating.
(setf (character-attribute :word-delimiter #\.) 1)


;;----------------------------------------------------------------------------
;; set up the dylan mode
;;----------------------------------------------------------------------------

(defvar *new-buffer-p* nil)

(defun a-dylan-mode-p (buf)
  (let ((mode (buffer-major-mode buf)))
    (or (eq mode *dylan-mode*)
        (eq mode *infix-dylan-mode*))))

(defun shared-dylan-mode-startup (buffer)
  (declare (ignore buffer)))

(defun start-up-infix-dylan-mode (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Infix-Dylan")
  (shared-dylan-mode-startup buffer))

(defparameter *infix-dylan-syntax-table*
  (create-syntax-table  :string-escape #\\
			:word-character #\#
			:escape #\\
			;; :comment #\/
			:nested t
			;; :end-comment #\newline
			:double-comment #\/
			:second-comment #\*
			:first-close-comment #\*
			:second-close-comment #\/
			:string #\"
			:close '(#\) #\} #\])
			:open '(#\( #\{ #\[)
			;; :special-char '(#\` #\')
			;; :string-within-symbol #\|
			:whitespace '(#\tab #\space
					    #\formfeed
					    #\newline
					    #\return)))

(defmode "Infix-Dylan" 
  :major-p t
  :vars '(("Paren Pause Period" . nil)
          ("Highlight Matching Parens" . t)
	  ("Comment Start" . "//")
	  ("Comment Begin" . "/*")
	  ("Comment End" . "*/")
          ;;	  ("Auto Fill Space Indent" . t) 
	  ("Indent Function" . indent-for-infix-dylan)
	  ("Mark Form Function" . mark-infix-dylan-form)

          ("Module" . 43))
  :syntax-table *infix-dylan-syntax-table*)

(defparameter *infix-dylan-mode* (getstring "Infix-Dylan" *mode-names*))

#+comment
(setf (mode-object-syntax-table (get-mode-object "C")) *c-syntax-table*)

(defcommand "Infix Dylan Mode" (p)
     "Put current buffer in Infix Dylan mode." 
     "Put current buffer in Infix Dylan mode."  
  (declare (ignore p))
  (start-up-infix-dylan-mode (current-buffer) "Infix-Dylan"))

(defun infix-dylan-first-word (point)
  (let ((start (copy-point point)))
    (line-start start)
    (multiple-value-bind (wstart wend)
        (next-word-region start)
      (declare (ignore wend))
      (values (next-word-string start) 
              (point-line-offset wstart)))))

(defun infix-dylan-first-word-above (point)
  (let ((above (copy-point point)))
    (line-offset above -1)
    (infix-dylan-first-word above)))

(defun mark-infix-dylan-form (point)
  (declare (ignore point))
  (format *terminal-io* "Marking for infix dylan!~%")
  nil)

;;  3 Oct 1996 (Jason)
(defun infix-dylan-first-non-whitespace-above (point)
  (let ((above (copy-point point)))
    (line-offset above -1)
    (line-start above)
    (find-attribute above :whitespace 'non-whitespace-or-newline)
    (values nil (point-line-offset above))))

;;----------------------------------------------------------------------------
;; Indentation
;;----------------------------------------------------------------------------

;;  3 Oct 1996 (Jason)
;; Better indenting: undo recording, further tabs force undo

(defvar *default-dylan-indent* 2)
(defvar *forceable-dylan-indent-p* t)
(defvar *move-cursor-on-dylan-indent-p* t)

(defun indent-for-infix-dylan (point)
  (multiple-value-bind (above indent)
      (infix-dylan-first-non-whitespace-above point)
    (declare (ignore above))
    (let ((first (copy-point point))
	  (start (copy-point point))
	  (moved-point-p nil)
	  (inserted-space-p nil))
      (line-start start)
      (line-start first)
      (find-attribute first :whitespace 'non-whitespace-or-newline)
      (recording-for-undo start first
	(when (and *move-cursor-on-dylan-indent-p*
		   (not (point= point first)))
	  (move-point (current-point) first)
	  (setq moved-point-p t))
        (when (or (< (point-line-offset first) indent)
		  moved-point-p)
	  (delete-between-points start first)
	  (insert-spaces first indent)
	  (setq inserted-space-p t))
        (when (and *forceable-dylan-indent-p*
		   (not moved-point-p)
		   (not inserted-space-p))
	  (insert-spaces first *default-dylan-indent*))))))

;;----------------------------------------------------------------------------
;; Marking forms.
;;----------------------------------------------------------------------------

;; None of these are generic in the editor. The way requiring least work 
;; would seem to be defadvice-ing a few primitive lisp point movers to 
;; behave differently based buffer mode.

(defun next-word-region (point)
  (let ((end (copy-point point)))
    (word-offset end 1)
    (let ((start (copy-point end)))
      (word-offset start -1)
      (values start end))))

(defun next-word-string (point)
  (multiple-value-bind (start end) (next-word-region point)
    (values (points-to-string start end) start end)))

(defadvice (start-defun-p infix-dylan-advice :around) (start)
  (if (not (eq (buffer-major-mode (point-buffer start)) *infix-dylan-mode*))
    (call-next-advice start)
    (multiple-value-bind (start end) (next-word-region start)
      (and (start-line-p start)
	   (equalp (points-to-string start end) "define")))))

(defadvice (top-level-offset infix-dylan-advice :around) (point offset)
  "Go forward or backward offset number of top level forms.  Point is
   returned if offset forms exists, otherwise nil."
  (if (not (eq (buffer-major-mode (point-buffer point)) *infix-dylan-mode*))
    (call-next-advice point offset)
    (let ((tpoint (copy-point point)))
      (if (= offset 1) (line-end tpoint) (line-start tpoint))
      (loop (if (start-defun-p tpoint) (return))
	    (unless (line-offset tpoint offset 0)
	      (return-from nil nil)))
      (move-point point tpoint))))

;; andrewa, 26 Sep 1995 - use &rest args since the number of args has
;; changed between LW 3.2 and 3.3
;; andrewa, 01 May 1996 - change the end test to see if there is whitespace
;; at the first character, rather then comparing points since there seems
;; to be a strange bug in the editor with point comparison at bigline
;; boundaries.
(defadvice (form-offset infix-dylan-advice :around) 
    (point n &rest args)
  (if (not (eq (buffer-major-mode (point-buffer point)) *infix-dylan-mode*))
      (apply #'call-next-advice point n args)
    (if (minusp n) 
        (apply #'call-next-advice point n args)
      (multiple-value-bind (string wstart wend)
          (next-word-string point)
        (declare (ignore wend))
        (let ((base (copy-point point)))
          (move-point base wstart)
          (cond
           ((equalp string "define")
            (let ((walk (copy-point base)))
              (loop
	       (unless (line-offset walk 1)
		 (line-offset base 1)
		 (move-point point base)
		 (return point))
	       (line-start walk)
	       (unless (sys::whitespace-char-p (next-character walk))
		 (let ((string (next-word-string walk)))
		   (cond
		    ((equalp string "end")
		     (line-end walk)
		     ;; (word-offset walk 1)
		     (move-point point walk)
		     (return point))
		    ((equalp string "define")
		     (line-offset base 1)
		     (move-point point base)
		     (return point))))))))
           (t
            (apply #'call-next-advice point n args))))))))


;;----------------------------------------------------------------------------
;; Syntax definitions.
;;----------------------------------------------------------------------------

(defparameter *infix-dylan-statements* '())

(defun define-infix-dylan-statement (name &key close-type)
  (push (cons name close-type) *infix-dylan-statements*))

(define-infix-dylan-statement "define" 
  :close-type :end)

(define-infix-dylan-statement "if"
  :close-type :end)

(define-infix-dylan-statement "let" 
  :close-type :exclusive-end)

;; eof
