;;----------------------------------------------------------------------------
;; Export the new symbols
;;----------------------------------------------------------------------------

(in-package "CAPI-LIBRARY")

(export '(update-representation-enabled-state))

(defgeneric update-representation-enabled-state (representation enabled)
  (:documentation
   "This updates the representation to be either enabled or disabled."))


(in-package "CAPI")

(export '(range-pane-enabled
          button-panel-enabled
          list-panel-enabled
          simple-pane-enabled))


;;----------------------------------------------------------------------------
;; Button panel enabling
;;----------------------------------------------------------------------------

(define-element button-panel (button-group choice)
  ((enabled :initform t :initarg :enabled :accessor button-panel-enabled))
  (:default-initargs :max-width t :max-height t))

#+lispworks3.2
(defun set-button-group-buttons (button-group)
  (with-slots (foreground background font enabled) button-group
    (let* ((interaction (button-group-button-interaction button-group))
           (callbacks (slot-value button-group 'callbacks))
           (images (slot-value button-group 'images))
           (old (button-group-buttons button-group))
           (new (map-collection-items
                 button-group
		 #'(lambda(x)
		     (if (typep x 'user-button)
			 x
		       (let* ((callback (when callbacks
					  (pop callbacks)))
                              (image (when images
                                       (pop images))))
			 (make-instance  
			  (if callback 'button  'simple-button)
                          #+comment
		          (if (eq interaction :multiple-selection) 
			      :extend-callback 
			    :selection-callback)
                          :selection-callback callback
                          :print-function nil
                          :max-width nil
                          :image image
                          :collection button-group
			  :interaction interaction
			  :data x
                          :enabled enabled
			  :foreground foreground
			  :background background
			  :font font
			  :button-group button-group))))
		 t)))
      (when  (eq interaction :single-selection)
	(when (and (> (length (collection-items button-group)) 0)
		   (null (choice-selection button-group)))
	  (setf (choice-selection button-group) 0)))
      (setf (slot-value button-group 'changed) :items)
      (setf  (button-group-buttons button-group)  new)
      (let ((layout (pane-layout button-group)))
        (when layout
          (setf (layout-description layout)
                new)))
      (dolist (b old)
        (unless (member b new)
          (destroy b))))))

#-lispworks3.2
(defun set-button-group-buttons (button-group &key mnemonic-items items
					      mnemonics (mnemonic-escape #\&)
					      default-button cancel-button)
  (with-slots (foreground background font enabled) button-group
    (when mnemonic-items
      (when (or mnemonics items)
	(error ":MNEMONIC-ITEMS cannot be specified with an explicit list of items or mnemonics"))
      (loop with mnemonic and text
	    for mnemonic-text in mnemonic-items
	    do
	    (multiple-value-setq
	      (text mnemonic)
	      (parse-mnemonic-text mnemonic-text mnemonic-escape))
	    collect mnemonic into mnemonic-indices
	    collect text into item-titles
	    finally (setf (slot-value button-group 'items) item-titles
			  mnemonics mnemonic-indices)))
    (let* ((interaction (button-group-button-interaction button-group))
           (callbacks (slot-value button-group 'callbacks))
           (images (slot-value button-group 'images))
           (old (button-group-buttons button-group))
           (new (map-collection-items
                 button-group
		 #'(lambda(x)
		     (if (or (null x) (typep x 'user-button))
			 (progn
                           (setf (slot-value x 'button-group) button-group)
                           x)
		       (let* ((callback (when callbacks
					  (pop callbacks)))
                              (image (when images
                                       (pop images))))
                         (make-instance 
                          (or (button-panel-button-class button-group)
			      (if callback 'button  'simple-button))
                          #+comment
		          (if (eq interaction :multiple-selection) 
			      :extend-callback 
			    :selection-callback)
                          :selection-callback callback
                          :print-function nil
                          :max-width nil
                          :image image
                          :collection button-group
			  :interaction interaction
			  :data x
			  :foreground foreground
			  :background background
			  :font font
                          :enabled enabled
			  :button-group button-group
			  :mnemonic (pop mnemonics)
			  :default-p (and default-button (collection-items-eq button-group x default-button))
			  :cancel-p (and cancel-button (collection-items-eq button-group x cancel-button))))))
		 
		 t)))
      (when  (eq interaction :single-selection)
	(when (and (> (length (collection-items button-group)) 0)
		   (null (choice-selection button-group)))
	  (setf (choice-selection button-group) 0)))
      (setf (slot-value button-group 'changed) :items)
      (setf  (button-group-buttons button-group)  new)
      (let ((layout (pane-layout button-group)))
        (when layout
          (setf (layout-description layout)
                new)))
      (dolist (b old)
        (unless (member b new)
          (destroy b))))))

(defmethod (setf button-panel-enabled) :after ((enabled t)
                                               (self button-panel))
  (dolist (button (button-group-buttons self))
    (setf (button-enabled button) enabled)))


;;----------------------------------------------------------------------------
;; Range panes
;;----------------------------------------------------------------------------

(defclass range-pane ()
  ((orientation :initarg :orientation :initform :horizontal
                :reader range-orientation)
   (start       :initarg :start       :initform 0   :accessor range-start)
   (end         :initarg :end         :initform 100 :accessor range-end)
   (slug-start  :initarg :slug-start  :initform 0   :accessor range-slug-start)
   (slug-end    :initarg :slug-end    :initform 10  :accessor range-slug-end)
   (callback    :initarg :callback    :initform nil :accessor range-callback)
   (enabled     :initarg :enabled     :initform nil :accessor range-pane-enabled)))

(defmethod create :after ((self range-pane))
  (setf (range-pane-enabled self) (range-pane-enabled self)))

(defmethod (setf range-pane-enabled) :after ((enabled t) (self range-pane))
  (when-let (representation (representation self))
    (capi-library:update-representation-enabled-state representation enabled)))


;;----------------------------------------------------------------------------
;; List-panels
;;----------------------------------------------------------------------------

(define-element list-panel (choice titled-pane)
  ((enabled :initform t :initarg :enabled :accessor list-panel-enabled))
  (:default-initargs
   :horizontal-scroll t
   :vertical-scroll t))

(defmethod create :after ((self list-panel))
  (setf (list-panel-enabled self) (list-panel-enabled self)))

(defmethod (setf list-panel-enabled) :after ((enabled t) (self list-panel))
  (when-let (representation (representation self))
    (capi-library:update-representation-enabled-state representation enabled)))


;;----------------------------------------------------------------------------
;; The generic method
;;----------------------------------------------------------------------------

(defmethod simple-pane-enabled ((self simple-pane))
  t)

(defmethod simple-pane-enabled ((self button))
  (button-enabled self))

(defmethod simple-pane-enabled ((self button-panel))
  (button-panel-enabled self))

(defmethod simple-pane-enabled ((self editor-pane))
  (editor-pane-enabled self))

(defmethod simple-pane-enabled ((self list-panel))
  (list-panel-enabled self))

(defmethod simple-pane-enabled ((self option-pane))
  (option-pane-enabled self))

(defmethod simple-pane-enabled ((self range-pane))
  (range-pane-enabled self))

(defmethod simple-pane-enabled ((self text-input-pane))
  (text-input-pane-enabled self))


;;----------------------------------------------------------------------------
;; (setf simple-pane-enabled)
;;----------------------------------------------------------------------------

(defmethod (setf simple-pane-enabled) ((enabled t) (self simple-pane))
  enabled)

(defmethod (setf simple-pane-enabled) ((enabled t) (self button))
  (setf (button-enabled self) enabled))

(defmethod (setf simple-pane-enabled) ((enabled t) (self button-panel))
  (setf (button-panel-enabled self) enabled))

(defmethod (setf simple-pane-enabled) ((enabled t) (self editor-pane))
  (setf (editor-pane-enabled self) enabled))

(defmethod (setf simple-pane-enabled) ((enabled t) (self list-panel))
  (setf (list-panel-enabled self) enabled))

(defmethod (setf simple-pane-enabled) ((enabled t) (self option-pane))
  (setf (option-pane-enabled self) enabled))

(defmethod (setf simple-pane-enabled) ((enabled t) (self range-pane))
  (setf (range-pane-enabled self) enabled))

(defmethod (setf simple-pane-enabled) ((enabled t) (self text-input-pane))
  (setf (text-input-pane-enabled self) enabled))
