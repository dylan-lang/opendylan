;;----------------------------------------------------------------------------
;; 13 Sep 1994, AA - add three color scroll-bars (for LGM)
;;----------------------------------------------------------------------------


(in-package "CLUE")


;;----------------------------------------------------------------------------
;; Scroll bar fixes
;;----------------------------------------------------------------------------

(defmacro with-contact-withdrawn (contact &rest body)
  (let ((ct (gensym "contact"))
        (actual-state (gensym "actual-state")))
  `(let* ((,ct ,contact)
          (,actual-state (contact-state ,ct)))
     (setf (contact-state ,ct) :withdrawn)
     (unwind-protect
         (progn ,@body)
       (setf (contact-state ,ct) ,actual-state)))))

(in-package "TOOLKIT")

(defun add-scrollbars (contact &key vertical horizontal can-scroll)
  (when contact
    (with-slots (parent x y height width border-width
                        clue::scroll-pane) contact
      (let ((background
             (contact-background parent))
            (foreground
             (when (typep parent 'lw-widget)
               (contact-foreground parent))))
        (clue::with-contact-withdrawn contact
          (when clue::scroll-pane
            (remove-scrollbars contact t))
          (when (or vertical horizontal)
            (let* ((sp (make-contact
                        'scroll-pane
                        :where contact
                        :name nil :parent parent
                        :foreground foreground
                        :background background
                        :x x :y y :width width :height height
                        :horizontal-scroll (or (unless (eq horizontal :none)
                                                 horizontal)
                                               (clue::find-contact-resource
                                                contact 'horizontal-scroll)
                                               :none)
                        :vertical-scroll (or (unless (eq vertical :none)
                                               vertical)
                                             (clue::find-contact-resource
                                              contact 'vertical-scroll)
                                             :none)))
                   (in-sp 
                    (if (or (eq can-scroll :yes)
                            (eq (can-scroll-p contact) :yes))
                        sp
                      (make-contact 'scroll-frame
                                    :name nil 
                                    :foreground foreground
                                    :background background
                                    :parent  sp))))
              (let ((realized (realized-p contact)))
                (when realized
                  (clue::delete-child parent contact)
                  (update-window-state in-sp))
                (setf (clue::contact-parent contact) in-sp)
                (setf (contact-scroll-pane contact) sp)
                  
                (when (and realized (not (eq sp in-sp)))
                  (move contact 0 0))))))
        contact))))

;;----------------------------------------------------------------------------
;; scroll-pane fixes
;;----------------------------------------------------------------------------

(define-resources
  (* scroll-pane has-motif-border) :no
  (* scroll-frame has-motif-border) :no)

;;; Changed popup scroll bars to default to :off - 30/5/91 KM
;;; Changed side to :right for vertical bars - 30/5/91 KM
;; 31/03/93 AA - add :WITHOUT-BAR option for horizontal/vertical scrolling
;; 29 Aug 1994, AA - make scroll-pane an lw-widget so that it has
;; a foreground slot
(defcontact scroll-pane (composite lw-widget)
  ((h-bar :initform nil)
   (v-bar :initform nil)
   (s-frame :initform nil)
   (horizontal-scroll)
   (vertical-scroll)
   (popup-scroll)
   (gap :initform 4)
   (motif-border :initform nil)
   (low-gc :initform nil)
   (hi-gc :initform nil)
   internal-width
   internal-height
   internal-x
   internal-y
   )
  (:resources
   (has-scroll-buttons :initform :yes)
   (has-slug-notch :initform :no)
   (event-mask :initform #.(make-event-mask :exposure))
   (horizontal-scroll :type (member :top :bottom :none :without-bar)
                      :initform :bottom)
   (vertical-scroll :type (member :left :right :none :without-bar)
                    :initform :right)
   (popup-scroll :type (member :on :off) :initform :off)
;   (background :initform :parent-relative)
   (h-class :initform 'simple-horizontal-scroll-bar)
   (v-class :initform 'simple-vertical-scroll-bar)
   )
  (:documentation 
   "This composite lays out a scrollable contact with optional horizontal
and vertical scroll bars which it creates automatically.  The
HORIZONTAL-SCROLL resource takes the values :top, :bottom or :none
which specifies whether there is a horizontal scroll bar and whether it
is laid out above or below the scrollable contact.  Similarly the
VERTICAL-SCROLL resource can take the values :left, :right or :none.

The POPUP-SCROLL resource may take the values :on or :off.  If it is
:off, the scroll bars are permanently mapped along with their parent.
Otherwise they are only visible when the pointer is within the
scroll-pane.
"))

(defun get-scroll-bar-color (color)
  (declare (optimize (fixnum-safety 3)))
  (* color 0.9))

(defun compute-scroll-bar-background (contact background)
  (let* ((screen (contact-screen contact))
         (colourmap (xlib::screen-default-colormap screen))
         (color
          (car (query-colors colourmap (list background))))
         (red   (color-red   color))
	 (green (color-green color))
	 (blue  (color-blue  color)))
    (convert contact (list (get-scroll-bar-color red)
		           (get-scroll-bar-color green)
		           (get-scroll-bar-color blue))
	     'pixel)))

;; 29 Aug 1994, AA - make the scroll-bars the color of the scroll-pane
;; 13 Sep 1994, AA - use compute-scroll-bar-background to get a reasonable
;; Motif-style background color for the scroll-bar
(defmethod initialize-instance :after ((self scroll-pane)
                                       &key
                                       h-class v-class
                                       has-scroll-buttons has-slug-notch)
  (with-slots (h-bar v-bar horizontal-scroll vertical-scroll popup-scroll
                     width height) self
    (let* ((foreground
            (when (typep self 'lw-widget)
              (contact-foreground self)))
           (background (clue:contact-background self))
           (scroll-bar-background
            (compute-scroll-bar-background self background)))
      (when (eq popup-scroll :off)
        (setf popup-scroll nil))
      (case horizontal-scroll
        (:none nil)
        (:without-bar (setq h-bar :without-bar))
        (t
         (setf h-bar (make-contact h-class
			           :parent self
			           :name 'horizontal-scroll-bar
                                   :foreground foreground
                                   :background scroll-bar-background
                                   :slug-foreground background
			           :scrollable-object self
                                   :has-scroll-buttons has-scroll-buttons
                                   :has-slug-notch has-slug-notch
			           :state (if (eq popup-scroll :on)
                                              :managed 
                                            :mapped)))))
      (case vertical-scroll
        (:none nil)
        (:without-bar (setq v-bar :without-bar))
        (t
         (setf v-bar (make-contact v-class
			           :parent self
			           :name 'vertical-scroll-bar
                                   :foreground foreground
                                   :background scroll-bar-background
                                   :slug-foreground background
			           :scrollable-object self
                                   :has-scroll-buttons has-scroll-buttons
                                   :has-slug-notch has-slug-notch
			           :state (if (eq popup-scroll :on)
                                              :managed
                                            :mapped)))))
      (when popup-scroll
        (add-event self :enter-notify '(activate-scrollbars t))
        (add-event self :leave-notify '(activate-scrollbars nil))))))


;;----------------------------------------------------------------------------
;; simple-scroll-bar stuff
;;----------------------------------------------------------------------------

;; 26 Aug 1994, AA - make this an lw-widget
;; 13 Sep 1994, AA - add slug-foreground for use in three color scroll-bars
(defcontact simple-scroll-bar (lw-widget)
  ((slug-gcontext)
   (slug-foreground :initarg :slug-foreground :initform nil)
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

;; 29 Aug 1994, AA - add this so we can choose how to display scroll-bars.
;; If t, this will be more Motif-like, and will work better with CLIM.
(defvar *scroll-bars-use-background-color* t)

;; 29 Aug 1994, AA - use *scroll-bars-use-background-color*
(defmethod initialize-instance :after ((self simple-scroll-bar) &key)
  (with-slots (width height foreground background slug-gcontext background-pixel 
                     copy-gcontext  button-images button-pixmaps
                     button-width button-height button1-pm button2-pm
                     slug-foreground slug-hi-gc slug-low-gc hi-gc low-gc
                     has-scroll-buttons) self
    (setf background-pixel background)
    (let ((monochrome (= (contact-depth (contact-root self)) 1))
          (root (contact-root self)))
      (if monochrome
	  (setf background
	        (cluei::get-pixmap self "12%gray"
				   :foreground foreground
				   :background background-pixel)
	        slug-gcontext
	        (find-gcontext root
			       :foreground foreground
			       :background background
			       :tile (clue::get-pixmap self "50%gray"
						       :foreground foreground
						       :background background-pixel)
			       :fill-style :tiled))
        (setf slug-gcontext
	      (find-gcontext root
			     :foreground (if *scroll-bars-use-background-color*
                                             (or slug-foreground background)
                                           foreground))))
    (if (eq has-scroll-buttons :no)
        (progn
          (setf button-width 0)
          (setf button-height 0))
      (progn
	(reset-button-pixmaps self background-pixel)
	(when button-pixmaps
	  (rotatef (second button-pixmaps) (third button-pixmaps))
	  (setf button-width (drawable-width (car button-pixmaps)))
	  (setf button-height (drawable-height (car button-pixmaps)))
	  (setf button1-pm (car button-pixmaps))
	  (setf button2-pm (cadr button-pixmaps)))))
    (when (zerop width)
      (setf width (contact-min-width self)))
    (when (zerop height)
      (setf height (contact-min-height self)))
    (scroll-extent-from-size self))))

;; 29 Aug 1994, AA - use *scroll-bars-use-background-color*
;; 13 Sep 1994, AA - implement Motif-style three color scroll bars
(defmethod initialize-instance :around ((self simple-scroll-bar) &key)
  (with-slots (slug-hi-gc slug-low-gc slug-foreground hi-gc low-gc
                          foreground background) self
    (prog1
        (call-next-method)
      (if (and (null slug-foreground) *scroll-bars-use-background-color*)
          (setf slug-hi-gc hi-gc
                slug-low-gc low-gc)
        (multiple-value-bind
            (gc1 gc2)
            (make-motif-gcontexts self
                                  (or slug-foreground foreground)
                                  background)
          (setf slug-hi-gc gc1
                slug-low-gc gc2)
          (when *scroll-bars-use-background-color*
            (setf hi-gc gc1
                  low-gc gc2)))))))

;; extracted this from the initialize-instance after method so that it can get
;; called by the setf contact-background method. - DHD 13/5/91
;; 29 Aug 1994, AA - use *scroll-bars-use-background-color*
(defmethod reset-button-pixmaps ((self simple-scroll-bar) background-pixel)
  (with-slots (button-pixmaps button-images slug-foreground 
                              foreground background) self
    (setf button-pixmaps
          (let ((all-pixmaps
                 (make-motif-scroll-buttons
                  (contact-root self)
		  (if *scroll-bars-use-background-color*
                      (or slug-foreground background)
                    foreground)
                  background-pixel)))
            (apply 'subseq all-pixmaps button-images)))))
