(in-package "TOOLKIT")

(export '(contact-enabled))

;; 26 Aug 1994, AA - a default method is useful
(defmethod contact-enabled ((self contact))
  t)

(defcontact lw-widget (contact)
  ((foreground :accessor contact-foreground)
   (hi-gc   :accessor contact-hi-gc :initform nil)
   (low-gc  :accessor contact-low-gc :initform nil)
   (internal-border :initform 2)
   (has-motif-border :accessor bordered-motif-contact)
   (enabled :initform t :initarg :enabled :accessor contact-enabled))
  (:resources 
   (horizontal-scroll :initform :none)
   (vertical-scroll :initform :none)
   (scroll :initform :no)
   (can-scroll :initform :no)

   (foreground :type pixel)
   (border-width :initform 0)
   (has-motif-border :type (member :yes :no :recessed :moat)
                     :initform :recessed))
  (:documentation
   "The contact lw-widget is the base contact class for the toolkit.
It has two definitions, one for Motif & one for Open Look

The resource has-motif-border determines the nature of the contact border in the Motif look and feel.

There are also resources which control the automatic creation of scroll
bars for scrolling the contact.  These are SCROLL which makes a contact scrollable, 
HORIZONTAL-SCROLL and VERTICAL-SCROLL which determine the presence and
position of the individual scroll bars and CAN-SCROLL which determines 
wether a contact requires a SCROLL-FRAME or if it can respond to the 
SCROLL method directly.
"))


;;----------------------------------------------------------------------------
;; Text input pane enabling
;;----------------------------------------------------------------------------

(defmethod display ((self text-item) &optional x y width height &key)
  (declare (ignore x y width height))
  (keep-caret-visible self)
  (with-slots (font title-font x-pos y-pos first-char foreground background
		    last-char title) self
    (gp:with-graphics-state (self :foreground foreground
                                  :background background)
      (let ((stipple
             (unless (text-item-enabled-p self)
	       (gp:find-stipple-for-port self 0.5))))
        (gp:draw-string self title 5 y-pos :font title-font :stipple stipple)
        (gp:draw-string self
                        (print-contents self first-char last-char)
                        x-pos y-pos
                        :font font
                        :stipple stipple))
      (when (/= first-char 0)
        (draw-overshoot-marker self))
      (display-caret self))))

(defmethod set-text-item-enabled ((self text-item) on)
  (with-slots (disabled) self
    (if on
        (progn
	  (setf disabled nil)
          (display self)
          (display-caret self)
          )
      (progn
        (display-caret self :clear t)
	(setf disabled t)
        (display self)))
    on))

;; 15 Nov 1994, AA - grey the overshoot marker when disabled
(defmethod draw-overshoot-marker ((contact text-item)
                                  &key (clear (zerop (slot-value
                                                      contact
                                                      'first-char))))
  (with-slots (x-pos y-pos first-char height font foreground
                     background) contact
    (let* ((ascent (max-char-ascent font))
	   (descent (max-char-descent font))
	   (width (max-char-width font))
	   (x (- x-pos 5))
	   (y y-pos))
      (gp:draw-polygon* contact
		       (list
                        x (+ y descent)
			x (- y ascent)
			(- x width) (- y (floor (- ascent  descent) 2))
			x (+ y descent)
                        )
                       :foreground (if clear
                                       background
                                     foreground)
		       :stipple (unless (text-item-enabled-p contact)
                                  (gp:find-stipple-for-port contact 0.5))
		       :filled t))))


;;----------------------------------------------------------------------------
;; Message enabling
;;----------------------------------------------------------------------------

(defcontact message (lw-widget)
  ((message :type string :initform "")
   (title :type string :initform "")
   (gcontext)
   (title-gcontext)
   (inactive-gcontext)
   (xpos)
   (ypos)
   (margin)
   (best-height :reader contact-best-height :reader contact-min-height)
   (best-width :reader contact-best-width :reader contact-min-width)
   )
  (:resources
   (font :type font)
   (title-font :type font :class font)
   (message)
   (title)
   (margin :type fixnum :initform 5)
   )
  (:documentation "A simple message with a title.  The appearance of this contact
is broadly the same as that of a text item however it has no input behaviour.")
)

;; 10/10/91 - AA changed first parameter of find-gcontext to be a root
;; instead of a display.

(defmethod initialize-instance :after ((self message) &key font title-font)
  (with-slots (gcontext inactive-gcontext title-gcontext
                        background foreground) self
    (let ((root (contact-root self)))
      (setf gcontext (find-gcontext
                      root
		      :foreground foreground
		      :background background
		      :font font))
      (setf title-gcontext (find-gcontext
			    root
			    :foreground foreground
			    :background background
			    :font title-font))
      (setf inactive-gcontext
            (find-gcontext root
                           :foreground foreground
                           :background background
                           :line-width 3
                           :font font
			   :tile (clue:get-pixmap self "50%gray"
				                  :foreground foreground
				                  :background background)
			   :fill-style :tiled))
      (set-dimensions self))))

;; 26 Aug 1994, AA - draw the text greyed if disabled.
(defmethod display ((self message) &optional x y width height &key)
  (declare (ignore x y width height))
  (with-slots (xpos ypos gcontext title-gcontext inactive-gcontext
	            title message margin) self
    (clear-area self :x x :y y :width width :height height)  ;; nick 17.11.92
    (draw-image*-glyphs self title-gcontext
		       margin ypos title)
    (if (contact-enabled self)
        (draw-image*-glyphs self gcontext (+ xpos margin) ypos message)
      (draw-glyphs self inactive-gcontext (+ xpos margin) ypos message))))

(defmethod (setf contact-enabled) :after ((enabled t) (self message))
  (display self))


;;----------------------------------------------------------------------------
;; Scroll bar enabling
;;----------------------------------------------------------------------------

(defcontact simple-scroll-bar (lw-widget)
  ((slug-gcontext)
   (has-scroll-buttons :initform :yes)
   (has-slug-notch :initform :no)
   (min-bar-width :initform 20)
   (background-pixel :reader contact-background)
   (button1-pm :initform nil)
   (button2-pm :initform nil)
   (button-pixmaps)
   (button-images)
   (button-height :initform 10)
   (button-width :initform 10)
   (margin :initform 2)
   (pix-extent :initform '(0 0))
   (button-pressed :initform nil)
   (scroll-offset :initform 0)
   (slug-hi-gc :initform nil)
   (slug-low-gc :initform nil)
   (drag-start :initform 0) ;MJS 18/09/92: keep offset of click from start of drag
   )
  (:resources
   (has-scroll-buttons :initform :yes)
   (has-slug-notch :initform :no)
   (button-images))
  )

(defmacro on-arrows-p (x)
  `(fixnump ,x))

;; 28 Aug 1995, AA - move in Martin's patched version of this function
;; from ~ext/patches/toolkit/0001/0004.lisp
(defmethod mark-button-up ((self simple-scroll-bar))
  (when (contact-enabled self)
    (with-slots (button-pressed scroll-object width height button1-pm button2-pm
			        button-pixmaps) self
      (cancel-scroll-repeat-timer)
      (when (on-arrows-p button-pressed)                 ;;; on one of the arraows.Y 11-oct-93
        (setf button1-pm (car button-pixmaps))
        (setf button2-pm (cadr button-pixmaps))
        (display self 0 0 width height :bar nil))
      (setf button-pressed nil))))

;; 28 Aug 1995, AA - move in Martin's patched version of this function
;; from ~ext/patches/toolkit/0001/0004.lisp
(defmethod mark-button-press ((self simple-scroll-bar) &optional (button 1))
  (when (contact-enabled self)
    (with-event (x y)
      (with-slots (internal-border button1-pm button2-pm button-pixmaps
	  			   width height button-pressed
				   scroll-extent scroll-limits scroll-offset
				   has-scroll-buttons pix-extent
				   drag-start) self
        (let* ((o (scroll-offset self x y))
	       (l (scroll-bar-length self))
	       (has-scroll-buttons-p (not (eq has-scroll-buttons :no)))
	       (on-slug-p (<= (car pix-extent) o (second pix-extent)))
	       )
	  (setf drag-start
	        (if pix-extent
		    (if on-slug-p
		        (- o (car pix-extent))
		      (floor (- (second pix-extent) (car pix-extent)) 2))
		  0))
	  (setf scroll-offset o)
	  (let ((bl (+ (button-length self) internal-border))
	        (scroll-timer-p t)
	        )
	    (cond
	     ((and has-scroll-buttons-p (< o bl))
	      (setf button1-pm (third button-pixmaps))
	      (display self 0 0 width height :button2 nil :bar nil)
	      (setf button-pressed -1)
	      )
	     ((and has-scroll-buttons-p (> o (- l bl)))
	      (setf button2-pm (fourth button-pixmaps))
	      (display self 0 0 width height :button1 nil :bar nil)
	      (setf button-pressed 1)
	      )
	     ((or (eq *scrolling-style* :old) (eq button 2))
	      (if on-slug-p
		  (setf button-pressed :on-slug)
	        (scroll-bar-scroll-to self (- scroll-offset drag-start)))
	      (setf scroll-timer-p nil)
	      )
	     (on-slug-p
	      (setf button-pressed :on-slug)
	      (setf scroll-timer-p nil)))
	    (when scroll-timer-p
	      (scroll-by-and-repeat self button-pressed *scrollbar-delay*))))))))

;; YY 11-oct-93. Now check that the button-press is on-slug.
;; 26 Aug 1994, AA - do nothing when the scrollbar is disabled
;; 28 Aug 1995, AA - move in Martin's patched version of this function
;; from ~ext/patches/toolkit/0001/0004.lisp
(defmethod scroll-to ((self simple-scroll-bar))
  (when (contact-enabled self)
    (with-slots (button-pressed scroll-object drag-start) self
      (unless (or (not (eq button-pressed :on-slug)) (not scroll-object))
        (cancel-scroll-repeat-timer)
        (with-event (x y)
	  (scroll-bar-scroll-to self (- (scroll-offset self x y) drag-start)))))))


;;----------------------------------------------------------------------------
;; Slider enabling
;;----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (do-demand-pre-loads :slider))

(defcontact slider (scroll-pane lw-widget)
  ((value :initform 0 :initarg :value)
   (extent :initform (list nil nil))
   (limits :initarg :limits :type list :initform (list 0 100))
   (displayer :initform 'princ-to-string :initarg :displayer)
   (half-slug-size :initform 20))
  (:resources
   (orientation :type (member :horizontal :vertical) :initform :horizontal)
   (vertical-scroll :initform :none)
   (horizontal-scroll :initform :none)
   (displayer :initform #'(lambda (x) (format nil " ~A " x)))
   (has-scroll-buttons :initform :no)
   (has-slug-notch :initform :yes)
   (slug-size :initform 40)))

;; 26 Aug 1994, AA - do nothing if the slider is disabled.
(defmethod scroll ((self scale) scroll-bar &key to by-page by-line)
  (declare (ignore by-page by-line))
  (when (contact-enabled self)
    (with-slots (scroll-type) self
      (with-slots (value limits half-slug-size extent displayer) scroll-type
        (let* ((slug-size (* half-slug-size 2))
               (to (cond
                    (to (+ to half-slug-size))
                    (by-page (+ value (* by-page slug-size)))
                    (by-line (+ to 1))))
               (end (second limits)))
	  (when (< to 0)
            (setq to (first limits)))
	  (when (< end to)
	    (setq to end))
	  (unless (= value to)
	    (setq value to)
	    (apply-callback scroll-type :notify value)
	    (setf (slot-value self 'message) (funcall displayer to))
	    (setf extent (value-to-extent self value))
	    (when (realized-p self) (d-display self))
	    )
          extent)))))

;; 6 Jan 94 -- Jason: frigged initial contents to be large enough
;;             (should really take proper account of scroll bars instead of using magic number 2)
;; 6 Jan 94 -- Jason: removed translate-value
(defmethod initialize-instance :after ((self slider) &rest args &key slug-size)
  (with-slots (half-slug-size value limits h-bar v-bar displayer) self
    (setf half-slug-size (floor slug-size 2))
    (let* ((sc (apply 'make-contact
		      'scale
		      :parent self
		      :scroll-type self
                      :enabled (contact-enabled self)
		      args)) ; pass on the args to the message area
	   (max-dummy-string (format nil "~V,'0D" (+ 2 (loop for x in limits maximizing (length (prin1-to-string x)))) 0)))
      (set-contents sc max-dummy-string)
      (setf (slot-value sc 'message) (funcall displayer value))))
    self)

;; 26 Aug 1994, AA - enabled/disable all of the children of the slider.
(defmethod (setf contact-enabled) :after ((enabled t) (self slider))
  (dolist (child (clue:composite-children self))
    (when (typep child 'lw-widget)
      (setf (contact-enabled child) enabled))))


;;----------------------------------------------------------------------------
;; List panel enabling
;;----------------------------------------------------------------------------

(defmethod (setf contact-enabled) :after ((enabled t) (self text-type))
  (display self))

(defmethod draw-item ((self motif-box-menu-type) item
		      item-no x y width height 
		      cleared)
  (declare (ignore cleared))
  (with-slots (gcontext justification item-print-fn item-font-fn
                        box-gcontext
		        inv-gcontext
		        inactive-gcontext
		        highlight-items
		        highlight-selected-items
		        hi-gc low-gc internal-border
		        submenu-indicator
		        item-separator-fn
                        text-type-x-border
                        text-type-y-border) self
    (if (not (funcall item-separator-fn item))
        (let* ((text (funcall item-print-fn item))
	       (font (determine-font item-font-fn item gcontext))
	       (dx (if (eq justification :left)
                       text-type-x-border
	             (floor (- width (fast-text-width font text))
		            (if (eq justification :centre) 2 1))))
	       (yt (+ y (centre-font font height)))
	       (selected (menu-is-it self item-no 'menu-selected))
	       (pressed (eq item-no (menu-pressed self)))
               (pressed-or-selected (or pressed selected))
  	       (inactive
                (or (null (contact-enabled self))
                    (menu-is-it self item-no 'menu-inactive)))
               (icon-x (+ x (- width *submenu-indicator-size* text-type-x-border)))
               (icon-y (+ y (floor (- height *submenu-indicator-size*) 2)))
               (icon-y-half (+ icon-y (floor *submenu-indicator-size* 2)))
               )
	  (draw-text*-in-rectangle
           self text font
	   (if inactive
	       inactive-gcontext
	     gcontext)
	   inv-gcontext
	   :x x
           :x-offset dx
	   :y yt
	   :width width ;; Oct 25 1993 (Jason Trenouth) -- pass all width
           :inactive inactive
	   :inverse (and pressed-or-selected
			 (not (or highlight-selected-items
				  highlight-items))))
	  (when (and submenu-indicator
                     (menu-item-has-submenu-p self item-no))
	    (draw-lines self gcontext
			(list icon-x icon-y
			      (+ x (- width text-type-x-border))  icon-y-half
			      icon-x (+ icon-y *submenu-indicator-size*)
			      icon-x icon-y)
			:fill-p t))
          (when (or highlight-items highlight-selected-items)
	    (draw-highlight-rectangle self
                                      (if highlight-items
                                          (if pressed-or-selected
                                              low-gc
                                            hi-gc)
                                        (if pressed-or-selected
					    hi-gc
                                          inv-gcontext))
				      (if highlight-items
					  (if pressed-or-selected
                                              hi-gc
                                            low-gc)
                                        (if pressed-or-selected
                                            low-gc
                                          inv-gcontext))
				      x y
				      width height))
          )
      (draw-separator self x y width height (grid-col-order self)))))

(defmethod double-press-item ((self menu-interaction) type)
  (when (contact-enabled self)
    (with-slots (last-pressed items item-data-fn) self
      (when last-pressed
        (apply-callback self type
		        (funcall item-data-fn 
			         (elt items last-pressed))))
      (setf last-pressed nil))))

(defaction press-item ((self menu-interaction))
  (when (contact-enabled self)
    (with-event (x y window)
      (let ((item (and (eq window self) (item-no self x y)))
            (inactive (menu-inactive self)))
        (do-press-item self (and (not (ccl::memq item inactive)) item))
        item))))

(defmethod select-item-extra ((self menu-interaction))
  (when (contact-enabled self)
    (with-slots (interaction last-pressed items item-data-fn) self
      (case interaction 
        ((:single-selection :spring-loaded :toggle)
         (launch-select-item self))
        ((:multiple-selection)
         (extend-selection self)))
      (when (and last-pressed items) ;DL 24/6/91 check have some items
        (apply-callback self :extend-callback
                        (funcall item-data-fn 
			         (elt (menu-items self) last-pressed)))))))

(defaction set-default ((self menu-interaction))
  (when (contact-enabled self)
    (with-event (x y window)
      (let ((item (and (eq window self) (item-no self x y))))
        (and item
             (setf (menu-default self) item))))))

(defmacro redisplaying-items (menu &body body)
  `(unwind-protect
       (progn ,@body)
     (maybe-do-redisplay-items ,menu)))

(defaction select-item :around ((self menu-interaction))
  (when (contact-enabled self)
    (do-unpress self)
    (redisplaying-items self
                        (call-next-method))))
