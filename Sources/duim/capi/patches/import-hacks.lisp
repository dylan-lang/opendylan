;;----------------------------------------------------------------------------
;; import-hacks.lisp
;;
;; Some nasty hacks to get some setf methods through to Dylan
;;----------------------------------------------------------------------------

(in-package "CAPI")

(defmacro define-setter-method (name (value &rest args))
  (let* ((setter-name
          (intern
           (concatenate 'simple-string (symbol-name name) "-SETTER")
           (symbol-package name))))
    `(defmethod ,setter-name (,value ,@args)
      (setf (,name ,@args) ,value))))

(define-setter-method interface-title (title interface))
(define-setter-method interface-menu-bar-items (items menu))
(define-setter-method menu-items (items menu))
(define-setter-method collection-items (items collection))
(define-setter-method button-selected (selected button))
(define-setter-method title-pane-text (text text-input-pane))
(define-setter-method text-input-pane-text (text text-input-pane))
(define-setter-method editor-pane-text (text editor-pane))
(define-setter-method item-text (text item))
(define-setter-method representation (representation object))
(define-setter-method range-start (value object))
(define-setter-method range-end (value object))
(define-setter-method range-slug-start (value object))
(define-setter-method range-slug-end (value object))
(define-setter-method scroll-bar-line-size (value object))
(define-setter-method scroll-bar-page-size (value object))
(define-setter-method simple-pane-enabled (value object))
(define-setter-method menu-object-enabled (value object))
(define-setter-method choice-selection (value object))


(in-package "CLUE")

(defconstant *keyword-pkg* (find-package :keyword))

(defmacro keywordify (x)
  `(if (eq (symbol-package ,x) *keyword-pkg*)
       ,x
       (intern (symbol-name ,x) *keyword-pkg*)))

(defmethod contact-state-setter (state contact)
  (setf (contact-state contact) (keywordify state)))


(in-package "XLIB")

(defmethod window-cursor-setter (cursor window)
  (setf (window-cursor window) cursor))


(in-package "GP")

(defconstant *keyword-pkg* (find-package :keyword))

(defmacro keywordify (x)
  `(if (eq (symbol-package ,x) *keyword-pkg*)
       ,x
       (intern (symbol-name ,x) *keyword-pkg*)))

(defmethod graphics-state-foreground-setter (fg gs)
  (setf (graphics-state-foreground gs) fg))

(defmethod graphics-state-background-setter (bg gs)
  (setf (graphics-state-background gs) bg))

(defmethod graphics-state-fill-style-setter (fill gs)
  (setf (graphics-state-fill-style gs) (and fill (keywordify fill))))

(defmethod graphics-state-operation-setter (op gs)
  (setf (graphics-state-operation gs) op))

(defmethod graphics-state-pattern-setter (pattern gs)
  (setf (graphics-state-pattern gs) pattern))

(defmethod graphics-state-mask-setter (mask gs)
  (setf (graphics-state-mask gs) (if (symbolp mask) (and mask (keywordify mask)) mask)))

(defmethod graphics-state-thickness-setter (thickness gs)
  (setf (graphics-state-thickness gs) thickness))

(defmethod graphics-state-dashed-setter (dashed gs)
  (setf (graphics-state-dashed gs) dashed))

(defmethod graphics-state-dash-setter (dash gs)
  (setf (graphics-state-dash gs) dash))

(defmethod graphics-state-line-end-style-setter (end gs)
  (setf (graphics-state-line-end-style gs) (and end (keywordify end))))

(defmethod graphics-state-line-joint-style-setter (joint gs)
  (setf (graphics-state-line-joint-style gs) (and joint (keywordify joint))))

(defmethod graphics-state-font-setter (font gs)
  (setf (graphics-state-font gs) font))

(defmethod generic-font-size-setter (size font)
  (setf (generic-font-size font) size))


(in-package "GP")

;; For some reason, we need both the GP and the CAPI junk here
(defclass duim-pixmap (capi::library-element
		       pixmap-port)
  ())

(defmethod find-parent-representation ((port duim-pixmap))
  (port-pixmap-representation port))


(in-package "CL-USER")

;; Returns the index of the character or the length of the string
(defun find-character-in-string (string char &key (start 0) (end (length string)))
  (declare (type string string))
  (declare (optimize (speed 3) (safety 1) (fixnum-safety 1)))
  (loop for i from start below end
        do (when (eq (char string i) char)
             (return-from find-character-in-string i)))
  end)
