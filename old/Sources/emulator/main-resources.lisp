;; Load before starting from your .lispworks to set up the Dylan environment
;; hooks. The Dylan system is demand-loaded later.

(unless (find-package :dylan)
  (defpackage :dylan
    (:export dylan-listen 
	     infix-dylan-listen
	     dylan-eval
	     compile-dylan-file 
	     load-dylan-file
	     translate-dylan-expr

             *the-dylan-package*)))

;; We don't want to load the real load-on-demand because it throws away
;; the source-level debugging info. There's a keyword parameter to toggle
;; this feature in images > 3.1

#|
(load-on-demand 
  :functions '(dylan:dylan-listen
               dylan:dylan-eval
               dylan:compile-dylan-file
               dylan:load-dylan-file
               dylan:translate-dylan-expr)
  :pathname 
    (merge-pathnames "demand-loader" (truename *load-pathname*)))
|#

(unless (fboundp 'dylan::dylan-listen)
  (let ((loc (merge-pathnames "demand-loader" (truename *load-pathname*)))
       (flag nil))
    (dolist (fn '(dylan:dylan-listen
                dylan:infix-dylan-listen
  	        dylan:dylan-eval
	        dylan:compile-dylan-file
	        dylan:load-dylan-file
	        dylan:translate-dylan-expr))
    (setf (symbol-function fn)
          #'(lambda (&rest args)
	      (load loc)
	      (apply fn args)))))
)

(in-package :tools)

(define-resources
  (* command-buttons * items)
  '(("Edit")
    ("Lisp")
    ("Env")
    ("Scm")
    ("Man")
    ("OS")
    ("Dylan"))
  (* command-buttons * menus) 
  '((:name top-edit-menu :notify tk::funcall-or-editor-command)
    (:name top-lisp-menu :notify funcall)
    (:name top-environment-menu :notify funcall)
    (:name top-scm-menu :notify funcall)
    (:name top-books-menu  :notify book::viewer-for-volume)
    (:name top-tools-menu :notify tk::funcall-or-editor-command)
    (:name top-dylan-menu :notify funcall))
  )

(defun dfmc-loaded? ()
  (find-package "DYLAN+DYLAN/DFMC-DERIVED-INFORMATION"))

(defun ensure-dylan-environment ()
  (let ((environment-package-name "DYLAN+DYLAN/ENVIRONMENT-TOOLS"))
    (or (find-package environment-package-name)
        (when (tk:confirm-yes-or-no
               "Load the Dylan Environment (will take a while)?")
          (dylan::ensure-library
	   (if (dfmc-loaded?)
	       'dylan::dfmc-environment
	     'dylan::emulator-environment))
          t))))

(defun start-dylan-environment ()
  (when (ensure-dylan-environment)
    (let ((function
	   (find-symbol "START-ENVIRONMENT" 
		        (find-package "DYLAN+DYLAN/ENVIRONMENT-TOOLS"))))
      (if function
          (funcall function)
        (error "Failed to demand load emulator environment -- aborting")))))

(define-resources
  (lispworks * top-dylan-menu * items)
  '(("Infix Listener"  . dylan:infix-dylan-listen)
    ("Prefix Listener" . dylan:dylan-listen)
    ("Remote Listener" . dylan::remote-dylan-listen)
    nil
    ("Start Dylan Environment" . start-dylan-environment)
    nil
    ("Report Dylan Bug" . editor::report-dylan-bug)
    ("Report Dylan Doc Bug" . editor::report-dylan-doc-bug)
    ("Report Dylan Source Changes" . editor::report-dylan-source-changes)
    ("Report Dylan Promotion" . editor::report-dylan-promotion)))

;; A few default colour resources.

(color:define-color-alias 
  :color-language-listener-background :antiquewhite1)

(define-resources
  (lispworks color-root * language-listener * listener-pane background)
  :color-language-listener-background)

;; Dylan

(color:define-color-alias 
  :color-dylan-listener-background :light-red)

(tools::define-resources
  (lispworks color-root * dylan-listener * listener-pane background)
  :color-dylan-listener-background)

;; Infix Dylan 

(color:define-color-alias 
  :color-infix-dylan-listener-background :aliceblue)

(tools::define-resources
  (lispworks color-root * infix-dylan-listener * listener-pane background)
  :color-infix-dylan-listener-background)

;; Indentation of Dylan forms.

(in-package :editor)

(setup-indent "define" 1)
(setup-indent "define-fluid" 1)
(setup-indent "define-class" 2 2 2)
(setup-indent "define-method" 2 2 2)
(setup-indent "define-generic-function" 2 2 2)

(setup-indent "bind-methods" 1 2 2)
(setup-indent "method" 1)
(setup-indent "bind" 1 2)
(setup-indent "fluid-bind" 1 2)

(setup-indent "for" 2 2 4)
(setup-indent "for-each" 2 2 4)

;; eof
