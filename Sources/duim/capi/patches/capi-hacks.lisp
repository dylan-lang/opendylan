;;----------------------------------------------------------------------------
;; capi-hacks.lisp
;;
;; Some miscellaneous little hacks to the CAPI
;;----------------------------------------------------------------------------

(in-package "CAPI")


(sys::without-warning-on-redefinition

;;----------------------------------------------------------------------------
;; Miscellaneous minor hacks
;;----------------------------------------------------------------------------

;;; capi/elements/scroll-bar

;;; correct the arguments in the call to the toolkit-library method
#|
(defmethod (setf scroll-bar-line-size) :after (value (self scroll-bar))
  (let ((rep (representation self)))
    (when (and rep value)
      (capi-library:update-scroll-bar-line-size (representation self) value))))

(defmethod (setf scroll-bar-page-size) :after (value (self scroll-bar))
  (let ((rep (representation self)))
    (when (and rep value)
      (capi-library:update-scroll-bar-page-size (representation self) value))))
|#

;; andrewa, 06 Dec 1995 - this is needed if you try to update a pop-up menu
;; on its own.
(defmethod update-parent-hierarchy ((self menu))
  (when-let (parent (element-parent self))
    (unless (typep parent 'simple-screen)
      (call-next-method))))

;; andrewa, 08 Dec 1995 - add initarg for menu-object-enabled
(defclass menu-object (callbacks)
  ((enabled-function  :initform nil :initarg :enabled-function)
   ;; (menu-interface :initarg :menu-interface :reader menu-interface)
   (callback-data-function :initform nil :initarg :callback-data-function)
   (enabled :initform t :initarg :enabled :reader menu-object-enabled)
   (print-function :initform nil :initarg :print-function :reader menu-print-function)
   (representation :initform nil :accessor representation) ;; Jules 11/8/92
   (popup-callback :initform nil :initarg :popup-callback
		   :accessor menu-popup-callback)))

;; 12 Apr 1994, AA - implement display-pane height handling
;; 1Dec95 bha - No need for add-to-minimum-height...
(defmethod pane-text-height ((self list-panel))
  (truncate (* (element-font-height self) (count-collection-items self))))

;; andrewa, 23 Jan 1996 - make sure the geometry slots keep in synch
;; with the backend objects
(defmethod change-geometry :around ((self simple-pane) width height x y)
  (with-geometry self
    (setf %x% (or x %x%)
	  %y% (or y %y%)
	  %width% (or width %width%)
	  %height% (or height %height%)))
  (call-next-method))

;; andrewa, 23 Jan 1996 - comment out the withdrawing of the children
;; as DUIM will handle that itself.
(defmethod maybe-install-description ((self layout))
  (with-slots (interface description d-description panes) self
    (unless (and (eq (car d-description) interface) 
		 (eq (cdr d-description) description) )
      (let ((rt (interpret-description self description interface)))
        (unless (and (ignore-errors (list-length rt))
                     (every 'geometry-p rt))
          (error "INTERPRET-DESCRIPTION returned a list containing non geometry-caches for ~S: ~S" self rt))
        (with-geometry self
	  (setf %valid% nil))
        #+comment
        (do-all-children
          (if (pane-geometry-p %geometry%)
              (withdraw-representation %child%)
            (remove-pinboard-object %child%)))
        (setf panes rt))
      (update-parent-hierarchy self)
      (setf d-description (cons interface description))
      (description-installed self)
      t)))

(defmethod element-interface ((self menu-component))
  (when-let (parent (element-parent self))
    (element-interface parent)))

(defmethod calculate-constraints ((self pane-with-layout))
  (let ((layout (ensure-interface-layout self)))
    (multiple-value-bind (lmn-w lmn-h lmx-w lmx-h)
        (if layout (get-constraints layout) (values 0 0 nil nil))
      (with-geometry self
	(if (top-level-interface-p self)
            (when-let (rep (representation self))
	      (let ((height-menu-bar (capi-library:height-menu-bar rep))
		    (width-menu-bar (capi-library:width-menu-bar rep)))
	        (setf %min-width%  (max (+ (or lmn-w 0)) width-menu-bar))
	        (setf %min-height% (+ (or lmn-h 0) height-menu-bar) )
	        (setf %max-width%  (if (or (eq lmx-w nil) (eq lmx-w t))
				       lmx-w
				     (max width-menu-bar lmx-w)))
	        (setf %max-height% (if (or (eq lmx-h nil) (eq lmx-h t))
				       lmx-h
				     (+ lmx-h height-menu-bar)))))
	  (setf  %min-width% (or lmn-w 0)
		 %min-height% (or lmn-h 0)
		 %max-width% lmx-w
		 %max-height% lmx-h))))))

(defmethod run-capi-post-actions (self)
  (let ((capi-tk-lib::*fork-capi-contact* t))
    (dolist (action-pair (sort *post-create-actions* '> :key 'car))
      (let ((action (cdr action-pair)))
        (funcall-apply-or-return action self))))
  (setq *post-create-actions* nil))

(defmethod run-capi-top-level-function ((self interface))
  (let* ((representation (representation self))
         (top-level-function
          (getf (xlib:window-plist representation)
                'capi-tk-lib::top-level-function)))
    (when top-level-function
      (apply-in-pane-process 
       self
       'capi-tk-lib::run-contact-top-level-function
       top-level-function))))


;;----------------------------------------------------------------------------
;; patch capi/0001/0009.lisp, since we don't use (load-all-patches)
;;----------------------------------------------------------------------------
#|
(defmethod create ((self layout))
  (if (or (and  *create-layout*
                (capi-library:NEED-LAYOUT-REPRESENTATION 
                (gp:find-parent-representation self)))
	   (layout-need-layout-representation self))
      (call-next-method)
    (setf (slot-value self 'created) t ))
  (setf (slot-value self 'd-description) nil)
  (redo-layout self)

  ;; 03 Aug 1993, AA - comment this out, as it causes trouble with ELLA
  ;; mirrored layouts... in theory the constraints don't need to be
  ;; calculated yet, but I wouldn't be suprised if this breaks something!
  ;; (get-constraints self)

  (setf (element-changed self) nil))

(defmethod create :after ((self output-pane))
  (let ((rep (representation self))
        (foreground (simple-pane-foreground self))
        (background (simple-pane-background self))
	(font (simple-pane-font self))
        (gstate     (gp:get-graphics-state self))
        )
    (unless rep
      (warn "Representation nil inside (create :after ((output-pane)))"))
    (when rep
      (when (or (null foreground) (eq foreground :foreground))
        (setf (simple-pane-foreground self)
	      (capi-library:representation-default-foreground rep)
	      ))
      (when (or (null background) (eq background :background))
        (setf (simple-pane-background self)
	      (capi-library:representation-default-background rep)
	      ))
      (when (null font)
	(setf (simple-pane-font self) :default-port-font))
      (install-input-model self rep (output-pane-input-model self)))
    (setf (gp:graphics-state-foreground gstate) (simple-pane-foreground self))
    (setf (gp:graphics-state-background gstate) (simple-pane-background self))
    (when (null font)
      (setf (gp:graphics-state-font gstate) :default-port-font))))
|#


;;; Title panes

;; Make subclasses of title-panes behave like title-panes in resources
(define-element title-pane (titled-pane)
  ((text :initarg :text :initform "" :reader title-pane-text)
   (resource-name :initform 'title-pane))
  (:default-initargs
   :title-position :left
   :min-width :text-width
   :max-width t
   :max-height t))

(defmacro map-children-body ((layout &optional child (kind t) panes reverse-p) body)
  (or panes (setf panes `(slot-value ,layout 'panes)))
  (when reverse-p (setf panes `(reverse ,panes)))
  (let ((bbody
         `(with-geometry-values %geometry%
            ,@(if child
                  `((let ((,child %child%))
                      ,@body))
                body))))
    `(dolist (%geometry% ,panes)
       ,(case kind
          (:pane `(when (pane-geometry-p %geometry%) ,bbody))
          (:object `(unless (pane-geometry-p %geometry%) ,bbody))
          (otherwise bbody)))))

(defmacro do-pane-children (&body body)
  `(map-children-body (self nil :pane) ,body))

(defmethod redo-layout ((self layout))
  (when (maybe-install-description self)
    (do-pane-children 
      (unless (representation %child%)
        (create %child%)))
    (invalidate-layout self)
    t))


;;; CAPI menu hacks

(defmethod (setf element-parent) ((parent menu)
				  (self menu-object))
  (push-end self (menu-items parent))
  parent)


) ;; End without-warning-on-redefinition
