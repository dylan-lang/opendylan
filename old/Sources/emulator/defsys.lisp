;;; Warning: this file can only be loaded interpreted.

;;; Do the package thang:

(unless (find-package :dylan)
  (defpackage :dylan
    (:export infix-dylan-listen
	     dylan-listen 
	     dylan-eval
	     compile-dylan-file 
	     load-dylan-file
	     translate-dylan-expr

             *the-dylan-package*)))

(defparameter dylan::*the-dylan-package* (find-package :dylan))

;;; Add an understanding of Dylan file types to defsystem.

(in-package :scm)

(unless (member '&key (generic-function-lambda-list #'scm::execute-compile))
  (push :old-scm-interface *features*))

;; Default extension:

(defparameter *scm-dylan-source-extension-string* "dylan")

(defclass scm-dylan-file (scm-lisp-file) ())

(defmethod execute-compile 
     #-:old-scm-interface 
     ((f scm-dylan-file) src-path obj-path &key &allow-other-keys)
     #+:old-scm-interface
     ((f scm-dylan-file) src-path obj-path)
  (apply #'dylan::compile-dylan-file 
	 src-path 
	 :output-file    obj-path
	 :target-machine *target-machine*
	 *compile-args*))

(defmethod execute-load 
     #-:old-scm-interface
     ((f scm-dylan-file) path &key &allow-other-keys)
     #+:old-scm-interface
     ((f scm-dylan-file) path)
  (apply #'dylan::load-dylan-file path *load-args*))

(defmethod pathname-extension 
     #-:old-scm-interface
     ((f scm-dylan-file) subject action &key qualifiers)
     #+:old-scm-interface
     ((f scm-dylan-file) subject action)
  (case subject
    (:source *scm-dylan-source-extension-string*)
    (:object (call-next-method))))

(add-file-type :dylan-file "SCM-DYLAN-FILE")

;; Long extension:

(defparameter *scm-long-dylan-source-extension-string* "dylan")
 
(defclass scm-long-dylan-file (scm-dylan-file) ())
 
(defmethod pathname-extension 
     #-:old-scm-interface
     ((f scm-long-dylan-file) subject action &key qualifiers)
     #+:old-scm-interface
     ((f scm-long-dylan-file) subject action)
  (case subject
    (:source *scm-long-dylan-source-extension-string*)
    (:object (call-next-method))))
 
(add-file-type :long-dylan-file "SCM-LONG-DYLAN-FILE")

;; Short extension

(defparameter *scm-short-dylan-source-extension-string* "dyl")

(defclass scm-short-dylan-file (scm-dylan-file) ())

(defmethod pathname-extension 
     #-:old-scm-interface
     ((f scm-short-dylan-file) subject action &key qualifiers)
     #+:old-scm-interface
     ((f scm-short-dylan-file) subject action)
  (case subject
    (:source *scm-short-dylan-source-extension-string*)
    (:object (call-next-method))))

(add-file-type :short-dylan-file "SCM-DYLAN-FILE")

;;;; The translator systems.

(in-package :dylan)

(defun the-local-directory (x) 
  (merge-pathnames x (directory-namestring (pathname *load-pathname*))))

;; Load generic LW environment extension support.

#-:harlequin-pc-lisp
(load (concatenate 'string 
                   (namestring (the-local-directory "generic-env"))
                   "/defsys.lisp"))
#+:harlequin-pc-lisp
(defsystem user::generic-env ())

;; The core systems.

(defsystem dylan-share 
  (:default-pathname "share"
   :optimize ((speed 3) (safety 0) (compilation-speed 0))
   :package :dylan)
		     
  :members ("dylan-builtin"
            "read"
            "modules")

  :rules ((:in-order-to :compile :all
                        (:requires (:load :previous))))

)

(defsystem dylan-translator
  (:default-pathname "trans-time"
   :optimize ((speed 3) (safety 0) (compilation-speed 0))
   :package :dylan)

  :members ((dylan-share :type :system)
            (user::generic-env :type :system)

            ("macros" :source-only t)

            "dylan-errors"
	    "translate"
	    "definers"
	    "define-class"
            "top-level-macros"

            "back-quote"
            "syntax"
            "fluid"
            #-:harlequin-pc-lisp
	    "alien"
            "load-eval-read"
	    "batch"

	    "backend-macros")

  :rules  ((:in-order-to :compile :all 
			 (:requires (:load "macros")
                                    (:load user::generic-env))))

)

(defsystem dylan-runtime
  (:default-pathname "run-time"
   :optimize ((speed 3) 
              (safety #-:harlequin-pc-lisp 0 #+:harlequin-pc-lisp 1)
              (compilation-speed 0))
   :package :dylan)

  :members ((dylan-share :type :system)
            
            "meta-dylan"
            "make-methods"
            "support"
	    "metering")

  :rules ((:in-order-to :compile :all
			(:requires (:load dylan-share))))

)

(defsystem dylan-core-libraries 
  (:package :dylan
   :optimize ((speed 3) (safety 0) (compilation-speed 0))
   :default-pathname "lib"
   :default-type :dylan-file)

  :members ((dylan-translator :type :system)
	    (dylan-runtime    :type :system)
            (user::generic-env :type :system)

	    "primitives"
            "void"
            "functions"
	    "types"
            "classes"
	    "ccc"
	    "numbers"
	    "characters"
	    "symbols"
	    "collections"
	    "sequences"
	    "lists"
	    "arrays"
	    "vectors"
            "stretchy-vectors"
	    "strings"
	    "tables"
	    "new-ranges"
	    "utils"
	    "dll"
	    "deques"
	    "misc"
            "new-iteration"
            "conditions"
	    "optimise"
            "printers"
	    "extensions")

  :rules (#|(:in-order-to :compile :all
                        (:caused-by (:compile dylan-translator))
			(:requires  (:load dylan-translator)))|#
          (:in-order-to :compile :all
	                (:requires  (:load :previous)))
	  #|(:in-order-to :load :all
			(:requires (:load dylan-runtime)))|#
	  #|(:in-order-to :load :all
			(:requires (:load :previous)))|#)

)

(defsystem dylan-core-systems (:package :dylan :default-type :system)

  :members (user::generic-env
            dylan-share
	    dylan-translator
	    dylan-runtime
	    dylan-core-libraries)

)

;; From here on we begin to use Dylan to implement translator behaviour -
;; in particular, syntax support and modules.

(defsystem dylan-syntax-case
  (:default-pathname "dw" :default-type :dylan-file)

  :members ((dylan-core-systems :type :system)

            "general"
            "syntax"
    
	    "macros"

            "header-reader"
            "header-wrapper"

            "block")

  :rules ((:in-order-to :compile :all (:requires (:load :previous))))

)

(defsystem dylan-modules 
  (:default-pathname "modules" :default-type :dylan-file)

  :members ((dylan-core-systems :type :system)
            (dylan-syntax-case :type :system)

            ("utils" :type :lisp-file)

            "union-collection"

            "filter"
            "filtered-ekc"
            "namespace"

            "translator-module"
            "define-module"

            "dylan-module"
	    "dylan-extensions-module"
            "dylan-user-module")

  :rules ((:in-order-to :compile :all (:requires (:load :previous))))

)

(defsystem dylan-numeric-libraries
  (:default-pathname "lib" :default-type :dylan-file)

  :members ((dylan-core-systems :type :system)
            (dylan-modules :type :system)

            "ratio/module-defs"
	    "ratio/primitives"

	    "complex/module-defs"
	    "complex/primitives"

	    ;; AndrewA, June 13 1996 - remove the old transcendental
	    ;; implementation, so that we can pick up
	    ;; lib/transcendentals instead	    
	    #+comment "transcendental/module-defs"
	    #+comment "transcendental/primitives"
	    )

  :rules ((:in-order-to :compile :all (:requires (:load :previous))))

)

(defsystem dylan-bootstrapped-core-systems
  (:package :dylan :default-type :system)

  :members (dylan-core-systems
            dylan-syntax-case
            dylan-modules
            dylan-numeric-libraries)

)

;; Compatability with DW extensions.

(defsystem dylan-dw-libraries
  (:default-pathname "dw" :default-type :dylan-file)

  :members ((dylan-bootstrapped-core-systems :type :system)

            "integer-compatibility"

            "translator-module"
            "syntax-case-module"
            "compatability-module"
            "stream-module"
            
            ("stream-macros" :type :lisp-file)

            #-:harlequin-pc-lisp
            "alien-module"
            #-:harlequin-pc-lisp
            "alien"

            ("translate" :type :lisp-file)

            "mop-module"
            "mop"

            ("memory-management-support" :type :lisp-file)
            "memory-management-module"
            "memory-management"

            "table-module"
	    "table"
	    "table-patch"
            "table-print"
            )

  :rules ((:in-order-to :compile :all (:requires (:load :previous))))

)

(defsystem dylan-syntax 
  (:default-pathname "syntax" :default-type :dylan-file)

  :members ((dylan-translator :type :system)
	    (dylan-runtime    :type :system)
	    (dylan-core-libraries  :type :system)

            "expressions"
            "patterns"
            "dylan-hooks"
            ("lisp-hooks" :type :lisp-file))

  #|
  :rules (#|(:in-order-to :compile :all
                        (:caused-by (:compile dylan-translator))
			(:requires  (:load dylan-translator)))|#
	  #|(:in-order-to :load :all
			(:requires (:load dylan-runtime)))|#
	  #|(:in-order-to :load :all
			(:requires (:load :previous)))|#)
  |#

)

;; Library/registry support

(defsystem dylan-libraries
  (:default-pathname "libraries"
   :default-type :dylan-file)

  :members ((dylan-bootstrapped-core-systems :type :system)
            (dylan-dw-libraries :type :system)

            ("libraries" :type :lisp-file)
            ("lid-files" :type :lisp-file)
            "define-library"
            "core-libraries"
            "required-libraries")

  :rules ((:in-order-to :compile :all (:requires (:load :previous))))

)

;; Environment code.

(load (concatenate 'string 
                   (namestring (the-local-directory "parsergen"))
                   "/defsys.lisp"))

(defsystem dylan-env-lod (:default-pathname "env")
  :members ("dylan-bugs" "dylan-source-changes"
            (sys::parser-runtime :type :system)
            (sys::parsergen :type :system)))

#-:harlequin-pc-lisp
(defsystem dylan-env 
  (:package :dylan
   :default-pathname "env"
   :default-type :lisp-file)

  :members  ((dylan-bootstrapped-core-systems :type :system)
            (dylan-dw-libraries :type :system)
            (dylan-libraries :type :system)

            ("works-module" :type :dylan-file)

            "commands"
	    "dylan"
            "infix"
	    "editor"
            "trace"
            "infix-mode"
            "evaluate"   ;; andrewa, 15 Nov 1995 - separated from infix-mode

            "listen"

            "hope"
            "dylan-bugs-resources"
	    "dylan-source-changes-resources"
	    "dylan-safe-inspector-printing-patch"
            ) 

  :rules ((:in-order-to :compile :all (:requires (:load :previous))))

)

#+:harlequin-pc-lisp
(defsystem dylan-env 
  (:package :dylan
   :default-pathname "env"
   :default-type :lisp-file)

  :members  ((dylan-bootstrapped-core-systems :type :system)
            (dylan-dw-libraries :type :system)
            (dylan-libraries :type :system)

            ("works-module" :type :dylan-file))

  :rules ((:in-order-to :compile :all (:requires (:load :previous))))

)

(defsystem dylan-format-extensions
  (:default-pathname "lib"
   :default-type :dylan-file)

  :members (;; andrewa, 20 Feb 1997 - useful format conversion tool
            "format-conversion")

  :rules ((:in-order-to :compile :all (:requires (:load :previous)))))

;; Libraries loaded by default

(defsystem dylan-default-libraries
  (:default-pathname "libraries"
   :default-type :dylan-file)

  :members ((dylan-bootstrapped-core-systems :type :system)
            (dylan-env :type :system)
	    (dylan-format-extensions :type :system)

            "default-loaded-libraries")

  :rules ((:in-order-to :compile :all (:requires (:load :previous))))

)

(defsystem dylan-lod-systems (:default-type :system)
  :members (dylan-env-lod))

(defsystem dylan-systems 
  (:default-type :system)

  :members (dylan-bootstrapped-core-systems
            dylan-dw-libraries
	    dylan-libraries
	    ;; dylan-syntax
	    dylan-env
            dylan-default-libraries)

)

;; An image saver.

(defvar dylan::*emulator-restart-hooks* '())

(defvar dylan::*start-infix-listener* nil)

(defun dylan::start-infix-listener-p ()
  (or (system::have-line-argument-p "-dylan")
      (and dylan::*start-infix-listener*
	   (not (system::have-line-argument-p "-lisp")))))

(defun dylan::run-emulator-restart-hooks ()
  (format t "~&; Running emulator restart hooks~%")
  (dolist (hook dylan::*emulator-restart-hooks*)
    (funcall hook)))

(defun dylan::start-dylan-emulator ()
  (when (dylan::start-infix-listener-p)
     (dylan::tty-infix-dylan-listen)))

;; This makes sure that the restart hooks get evaluated before any
;; image initialization. In particular, this means that this will
;; happen before the .lispworks is loaded, so users can rely on
;; the registries being already installed before their own
;; initialization code.
(pushnew #'dylan::run-emulator-restart-hooks system::*cleanup-before-entry*)

(defun user::save-emulator-image (name &rest options)
  (format t "~&; Saving emulator image~%")
  (apply #'save-image name
	 :restart-function #'dylan::start-dylan-emulator
	 options))

;; Note the emulator's presence.

(scm:note-private-patch 'dylan-emulator-hacks "Keith Playford" 
  "A number of destructive modifications to CLOS and the environment.")

;; eof
