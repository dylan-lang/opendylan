(in-package "TOOLKIT")

(sys::without-warning-on-redefinition

;;----------------------------------------------------------------------------
;; dialog hacks
;;----------------------------------------------------------------------------

(defmethod title-height ((self form))
  (with-slots (margin title gcontext ) self
    (if title
        (+ margin (+ margin (max-char-ascent (gcontext-font gcontext))))
      0)))

;; andrewa, 16 May 1996 - allow the use of window-title as well as title
;; so that it is possible to specify one without the other
(defmethod initialize-instance :after ((self popup)
                                       &key override-redirect window-title)
  (with-slots (title) self
    (unless (eq override-redirect :on)
      (setf (wm-title (contact-parent self)) (or window-title title)))))

;; andrewa, 16 May 1996 - fix up the code to redisplay text-items that
;; is broken in patch TOOLKIT-0008.
(defmethod set-contents ((self text-item) new-contents
                         &key (caret-position :end))
  (with-slots (contents) self
    (let ((displayed-p (realized-p self)))
      (when displayed-p
	(display-caret self :clear t))
      (setf contents new-contents)
      (when caret-position
	(setf (caret-position self) caret-position))
      (setf (mark-position self) nil)
      (keep-caret-visible self)
      (when displayed-p
        (text-item-update-text self)
        (display-caret self)
	(draw-overshoot-marker self)))))

) ;; End without-warning-on-redefinition
