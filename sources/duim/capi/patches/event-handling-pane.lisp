(in-package "CAPI-TOOLKIT-LIBRARY")


;;----------------------------------------------------------------------------
;; event-handling-pane
;;
;; This is a CAPI window that knows how to distribute events
;;----------------------------------------------------------------------------

(clue:defcontact capi-event-handling-contact (capi-output-pane)
  ()
  ;; Compressing exposure events makes for better damage repaint behavior
  (:default-initargs :compress-exposures :on))

(defclass event-handling-pane (pinboard-layout)
  ())

(defmethod create-contact ((self event-handling-pane) parent)
  (let ((output-pane (call-next-method)))
    (install-contact-event-handlers output-pane)
    output-pane))


;;----------------------------------------------------------------------------
;; event handling
;;----------------------------------------------------------------------------

(defmethod install-contact-event-handlers ((contact clue:contact))
  (let ((display (clue:contact-display contact)))
    (clue:add-event contact :button-press
		    `(contact-button :press ,display))
    (clue:add-event contact '(:double-button-press)	;list, to get around Clue buglet...
		    `(contact-button :double-click ,display))
    (clue:add-event contact :button-release
		    `(contact-button :release ,display))
    (clue:add-event contact :motion-notify
		    `(contact-motion :motion ,display))
    (clue:add-event contact :enter-notify
		    `(contact-entry-exit :enter ,display))
    (clue:add-event contact :leave-notify
		    `(contact-entry-exit :exit ,display))
    (clue:add-event contact :key-press
		    `(contact-key :press ,display))
    (clue:add-event contact :key-release
		    `(contact-key :release ,display))
    (clue:add-event contact :exposure
		    `(contact-exposure))
    (clue:add-event contact :map-notify
		    `(contact-abled t))
    (clue:add-event contact :unmap-notify
		    `(contact-abled nil))
    ))


;;----------------------------------------------------------------------------
;; event handling
;;----------------------------------------------------------------------------

(defvar *modifier-map* nil)

(defun state-mask->duim-modifier-state (state-mask display)
  (unless *modifier-map* 
    (setq *modifier-map* (make-duim-modifier-map display)))
  (aref *modifier-map* (logand state-mask 255)))

(defun logior-all-mask-bits (array logior mask)
  (loop for index from 0 to (length array)
        do
	(when (logtest index mask)
	  (setf (aref array index)
		(logior (aref array index) logior)))))

(defun make-duim-modifier-map (display)
  (let ((array (make-array 256 :initial-element 0))
        (map (multiple-value-list (xlib:modifier-mapping display))))
    (logior-all-mask-bits array 1			;$shift-key
                          (xlib:make-state-mask :shift))
    (logior-all-mask-bits array 2			;$control-key
                          (xlib:make-state-mask :control))
    (loop for codes in map
          for mod in '(:shift :lock :control :mod-1 :mod-2 :mod-3 :mod-4 :mod-5)
          do
          (dolist (code codes)
            (let ((keysym (xlib:keycode->keysym display code 0)))
	      (cond ((or (= keysym xlib::left-meta-keysym)
		         (= keysym xlib::left-alt-keysym)
		         (= keysym xlib::right-meta-keysym)
		         (= keysym xlib::right-alt-keysym))
                     (logior-all-mask-bits array 4	;$meta-key
                                           (xlib:make-state-mask mod))
                     (return))
		    ((or (= keysym xlib::left-super-keysym)
		         (= keysym xlib::right-super-keysym))
                     (logior-all-mask-bits array 8	;$super-key
                                           (xlib:make-state-mask mod))
                     (return))
		    ((or (= keysym xlib::left-hyper-keysym)
		         (= keysym xlib::right-hyper-keysym))
                     (logior-all-mask-bits array 16	;$hyper-key
                                           (xlib:make-state-mask mod))
                     (return))))))
    array))


;;----------------------------------------------------------------------------
;; event handling
;;----------------------------------------------------------------------------

(defgeneric distribute-abled-event (pane state))
(defgeneric distribute-exposure-event (pane x y width height))
(defgeneric distribute-button-event (pane type x y modifier-state button))
(defgeneric distribute-motion-event (pane type x y modifier-state))
(defgeneric distribute-entry-exit-event (pane type x y modifier-state mode kind))
(defgeneric distribute-key-event (pane type modifier-state keysym char))

;;; Called by MAP-NOTIFY (state = t) and UNMAP-NOTIFY (state = nil) events.
(defun contact-abled (event-window state)
  (let ((pane (capi-object event-window)))
    (distribute-abled-event pane state)))

(defun contact-exposure (event-window)
  (clue:with-event (clue:x clue:y clue:width clue:height)
    (let ((pane (capi-object event-window)))
      (distribute-exposure-event
       pane
       clue:x clue:y clue:width clue:height))))

(defun contact-button (event-window type display)
  (clue:with-event (clue:x clue:y clue:state clue:code)
    (let ((pane (capi-object event-window))
	  (button clue:code)
	  click-type)
      (when button
        (distribute-button-event
	 pane type
	 clue:x clue:y
	 (state-mask->duim-modifier-state clue:state display)
	 button)))))

(defun contact-motion (event-window type display)
  (clue:with-event (clue:x clue:y clue:state)
    (let ((pane (capi-object event-window)))
      (distribute-motion-event
       pane type
       clue:x clue:y
       (state-mask->duim-modifier-state clue:state display)))))

(defun contact-entry-exit (event-window type display)
  (clue:with-event (clue:x clue:y clue:state clue:mode clue:kind)
    (let ((pane (capi-object event-window)))
      (distribute-entry-exit-event
       pane type
       clue:x clue:y
       (state-mask->duim-modifier-state clue:state display)
       clue:mode
       clue:kind))))

(defun contact-key (event-window type display)
  (clue:with-event (clue:state clue:code)
    (let* ((pane (capi-object event-window))
           (char (xlib:keycode->character display clue:code (logand clue:state 1)))
	   (keysym
            (when (characterp char)
              (cond 
	        ;; Intern things in the Dylan package, since we want #"delete", etc
                ((eq char #\rubout)    (intern "BACKSPACE" "DYLAN"))
		((eq char #\backspace) (intern "DELETE" "DYLAN"))
		(t (intern (string-upcase (format nil "~:C" char)) "DYLAN"))))))
      (when keysym
        (distribute-key-event
         pane type
         (state-mask->duim-modifier-state clue:state display)
         keysym
         (if (and (characterp char)
                  (standard-char-p char))
             char
           dylan::*dylan-canonical-false*))))))
