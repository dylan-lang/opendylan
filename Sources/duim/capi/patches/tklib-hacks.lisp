(in-package "CAPI-TOOLKIT-LIBRARY")

(export 'representation-default-font)

(in-package "CAPI-TOOLKIT-LIBRARY")


;;----------------------------------------------------------------------------
;; miscellaneous hacks
;;----------------------------------------------------------------------------

(defun capi-shell (rep)
  (do ((shell rep (clue:contact-parent shell)))
      ((typep shell 'clue:top-level-shell)
       shell)))

(defmethod ensure-pane-created (pane)
  nil)

(defmethod ensure-pane-created ((pane simple-pane))
  (unless (representation pane)
    (let ((*fork-capi-contact* nil))
      (capi::create pane))))

(defmethod ensure-pane-created ((pane simple-pane))
  (unless (representation pane)
    (let ((*fork-capi-contact* nil))
      (capi::create pane))))

(defmethod ensure-dialog-created ((pane interface))
  (unless (representation pane)
    (let ((*fork-capi-contact* nil)
          (grab-mouse t))
      (capi::create-dialog pane (convert-to-screen) grab-mouse))))

(defmethod create-dialog-representation (self (parent clue:root) grab-mouse
                                              &rest args
                                              &key
                                              top-level-class-name
					      pop-up-from
                                              &allow-other-keys)
   (apply 'make-capi-contact self 
	 (or top-level-class-name 'capi-dialog)
	 :parent parent
	 :override-redirect (if grab-mouse :on :off)
	 :window-title (when-let (title (interface-title self))
		         (when (> (length title) 0)
		           title))
         :title-font (interface-title-font self)
	 :message (interface-message self)
	 :pop-up-from (if (and (typep pop-up-from 'clue:contact)
                               (clue:mapped-p pop-up-from))
                          pop-up-from
			(representation (capi::default-owner)))
	 args))


;;----------------------------------------------------------------------------
;; popup menu handling
;;----------------------------------------------------------------------------

(defmethod representation-popup-menu ((parent clue:contact) menu owner x y)
  (let* ((popup-menu
          (or (representation menu)
              (apply 'clue:make-contact 'capi-popup-menu
		     :capi-object menu
                     :title (menu-title menu)
                     :parent (clue:contact-root parent)
                     (create-toolkit-style-menu-items menu parent))))
         (owner-rep (when owner (representation owner))))
    (let ((menu-item
	   (tk:popup-motif-prompt popup-menu
				  :reset-pointer-pos nil
				  :position :pointer
				  :pop-up-from owner-rep
				  :x x
				  :y y)))
      (if menu-item
	  (values menu-item t)
	(values nil nil)))))

(defmethod do-force-output ((x xlib::display))
  (declare (optimize (debug 1)))
  (if (not (xlib::display-dead x))
      (xlib::display-force-output x)
    (progn
      (setq capi::*objects-to-force-output*
	    (remove x capi::*objects-to-force-output*))
      (format *terminal-io* "Discarding dead display ~S" x))))


;;----------------------------------------------------------------------------
;; change-representation-geometry
;;----------------------------------------------------------------------------

;; andrewa, 17 Apr 1996 - only call clue:change-geometry if necessary
(defmethod change-representation-geometry ((self clue:contact) width height x y)
  (let ((pane (external-contact self)))
    (setq width (max width 1))
    (setf height (max height 1))
    (unless (and (eq x (clue:contact-x self))
                 (eq y (clue:contact-y self))
                 (eq width (clue:contact-width self))
                 (eq height (clue:contact-height self)))
      #+comment
      (format t "~&Moving ~A by ~A,~A, extending by ~A, ~A"
              pane
              (when x (- (clue:contact-x self) x))
              (when y (- (clue:contact-y self) y))
              (when width (- (clue:contact-width self) width))
              (when height (- (clue:contact-height self) height)))
      (clue:change-geometry pane 
                            :x x :y y :width width :height height
                            :accept-p t))))


;;----------------------------------------------------------------------------
;; Font handling
;;----------------------------------------------------------------------------

(defmethod representation-default-font ((self capi-contact))
  (when-let (font (contact-font self))
    (when-let (font-name 
               (and (typep font 'xlib:font)
                    (xlib:font-name font)))
      font-name)))

;; andrewa, 02 May 1996 - make editor-panes the same color as everything else
;; andrewa, 10 Jan 1997 - switch off motif borders for CAPI output panes
;; andrewa, 28 May 1997 - make list boxes be blue
(clue:define-resources
  (lispworks color-root * "<capi-top-level-mirror>" *
             editor-pane background) :color-background
  (lispworks * capi-output-pane has-motif-border) :no
  (lispworks color-root * text-menu * background) :color-background)


;;----------------------------------------------------------------------------
;; Focus handling
;;----------------------------------------------------------------------------

(defgeneric distribute-focus-event (pane active))

(defmethod tk::keyboard-change :after ((self capi-contact)
				       active
				       &optional focus-out)
  (declare (ignore focus-out))
  (when-let (pane (capi-object self))
    (distribute-focus-event pane active)))

(defmethod distribute-focus-event (pane active)
  nil)

;;--------------- Exposure ------------------

(defun tk-output-pane-exposure (contact)
  (clue:with-event (clue:x clue:y clue:width clue:height)
    (let ((object (capi-object contact)))
      (when object
        (when (capi-output-pane-drawn-p contact)
          (let ((bgc (or (capi-output-pane-bgc contact)
                         (setf (capi-output-pane-bgc contact)
                               (tk::find-gcontext contact)))))
	    (when-let (background (simple-pane-background object))
              (xlib:with-gcontext (bgc 
				   :foreground (color:convert-color
						contact background))
                (xlib:draw-rectangle contact bgc
				     clue:x clue:y clue:width clue:height
				     t)))))
	(output-pane-display object clue:x clue:y clue:width clue:height)
	(setf (capi-output-pane-drawn-p contact) nil)
        ))))

(defun main-contact ()
  (or (when-let (interface (main-interface))
        (representation interface))
      (tk:display-property :application-shell)))
