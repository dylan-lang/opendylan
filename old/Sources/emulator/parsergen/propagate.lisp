;; -*- rcs-header: "$Header: /scm/cvs/fundev/old/Sources/emulator/parsergen/propagate.lisp,v 1.1 2004/03/12 00:41:16 cgay Exp $" -*-

;; #<harlequin copyright marker>

(in-package "PARSERGEN")

(defvar *propagation-table*)
; A mapping state X item -> State X item

(defvar *lookahead-table*)
;   A mapping state -> assoc (item -> terminals)

;; Constructs the lookahead tables for the kernel sets.
(defun propagate-gotos(states)
  (let ((*propagation-table* nil))
    (setq *lookahead-table* (initial-lookahead-table))
    (determine-lookaheads states)
    (propagate-the-lookaheads)))

;;;
;;; Initialise the lookahead and propagations tables 
;;;

(defun determine-lookaheads (states)
  ;;; By definition the end of input token :eoi is spontaneously
  ;;; generated for the initial rule
  (let ((*closure-table* (set-up-closure-table)))
    (declare (special *closure-table*))
    (initialize-lookahead-table states)
    (add-to-lookahead-table 0 ; should be (initial-state)
			    (initial-item)
			    :eoi)
    (dolist (state states)
	    (determine-state-lookaheads state))))

(defun determine-state-lookaheads (state)
  (let ((kernel-set (item-set-of-state state)))
  (dolist (kernel-item kernel-set)
	  (let ((item-closure (get-lr1-closure kernel-item)))
	      (dolist (lr1-item item-closure)
		      (let ((item (car lr1-item))
			    (lookaheads (cdr lr1-item)))
			(dolist (la lookaheads)
				(if (eq la (dummy))
				    (add-to-propagation-table kernel-item
							      state
							      item)
				  (generate-spontaneous-lookahead state
								  item
								  la)))))))))

; Lookaheads should be propagated from the item 'from-item' in
; 'in-state' to shift-dot(to-item) in goto(in-state item-b(from-item))

(defun add-to-propagation-table (from-item in-state to-item)
  (let ((goto-symbol (item-B to-item)))
    (when goto-symbol
	    (let* ((goto-state (goto-state in-state goto-symbol))
		   (goto-item (shift-dot to-item))
		   (propagate-from (cons in-state from-item))
		   (propagate-to (cons goto-state goto-item)))
	      (push (cons propagate-from
			  propagate-to)
		    *propagation-table*)))))

; lookahead is generated spontaneously for shift-dot(to-item) in state
; goto(in-state, item-b(to-item)

(defun generate-spontaneous-lookahead (in-state to-item lookahead)
  (let* ((goto-symbol (item-B to-item)))
    (if goto-symbol
	(let* ((goto-state (goto-state in-state goto-symbol))
	       (goto-item (shift-dot to-item)))
	  (add-to-lookahead-table goto-state
				  goto-item
				  lookahead)))))


;;;
;;; Propagate the lookaheads 
;;;

(defun propagate-the-lookaheads()
  (loop
   (unless (propagate-lookahead-table)
       (return))))

(defun propagate-lookahead-table()
  (let ((result nil))
    (dolist (propagate-entry *propagation-table*)
	    (setq result (or (propagate-a-lookahead propagate-entry)
			     result)))
    result))

(defun propagate-a-lookahead (entry)
  ; entry is "state-item1 -> state-item2"
  ; add lookaheads for state-item1 to those of state-item2, if not
  ; already there.
  ; return nil iff nothing is added.

  (add-list-to-lookahead-table (cdr entry)
			       (get-lookaheads (caar entry)(cdar entry))))

(defun add-list-to-lookahead-table (state-item lalist)
  (let* ((state (car state-item))
	 (item (cdr state-item))
	 (item-lalist (assoc item (aref *lookahead-table* state)))
	 (changed nil))
    (dolist (la lalist changed)
	    (unless (member la (cdr item-lalist))
		    (push la (cdr item-lalist))
		    (setq changed T)))))

(defun initial-lookahead-table ()
  (declare (special *state-numbers*))
  (make-array *state-numbers* :initial-element nil))

(defun initialize-lookahead-table (states)
    (dolist (state states)
	    (setf (aref *lookahead-table* state)
		  (mapcar #'list (item-set-of-state state)))))

(defun add-to-lookahead-table (state item la)
  (let ((item-lalist (assoc item (aref *lookahead-table* state))))
    (unless (member la (cdr item-lalist))
	    (push la (cdr item-lalist)))))

(defun get-lookaheads (state item)
  (cdr (assoc item (aref *lookahead-table* state))))

(defun print-lookaheads()
  (declare (special *state-numbers*))
  (dotimes (state *state-numbers*)
	   (let ((item-lookaheads (aref *lookahead-table* state)))
	       (dolist (item-las item-lookaheads)
		       (let ((item (car item-las))
			     (las (cdr item-las)))
			 (print-LR1-item state item las))))))

(defun print-LR1-item (state item lookaheads)
  (format t "State ~A : ~A -> ~{~A ~}. ~{~A ~}, ~A~{/~A~}~%"
		state
		(item-terminal item)
		(item-alpha item)
		(if (item-B item)
		    (cons (item-B item) (item-beta item))
		  (item-beta item))
		(car lookaheads)
		(cdr lookaheads)))

(defun get-lr1-closure (item)
  (declare (special *closure-table*))
  (gethash item *closure-table*))
  
(defun set-up-closure-table ()
  (let ((result (make-hash-table :rehash-size 2.0)))
    (dotimes (item (array-total-size *item-array*))
		   (setf (gethash item result)
			 (lr1-closure item (dummy))))

    result))

(defparameter *dummy* (gensym))

(defun dummy () *dummy*)
