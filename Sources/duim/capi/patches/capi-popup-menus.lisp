(in-package "CAPI-LIBRARY")

(export '(
          representation-popup-menu
          ))


(in-package "CAPI-TK-LIB")

;;----------------------------------------------------------------------------
;; Popup menu stuff
;;----------------------------------------------------------------------------

(clue:defcontact capi-popup-menu (capi-command-menu)
  ())

(defmethod representation-popup-menu ((parent clue:contact) menu owner x y)
  (let* ((popup-menu
          (or (representation menu)
              (apply 'clue:make-contact 'capi-popup-menu
                     :title (menu-title menu)
		     :capi-object menu
                     :parent (clue:contact-root parent)
                     (create-toolkit-style-menu-items menu parent))))
         (menu-item
          (tk:popup-motif-prompt popup-menu
                                 :reset-pointer-pos nil
                                 :position :pointer
                                 :pop-up-from (representation owner)
                                 :x x :y y)))
    (if menu-item
        (values menu-item t)
      (values nil nil))))

(clue:define-resources
  (lispworks color-root * popup-menu background)   :color-background
  (lispworks color-root * popup-menu * background) :color-background
  )

(in-package "CAPI")

(export '(
          display-popup-menu
          ))

;;----------------------------------------------------------------------------
;; popup-menu
;;----------------------------------------------------------------------------

(defun default-owner ()
  (or *top-level-interface* (main-interface)))

;; 13 May 1994, AA - new (needed for CLIM)
(defun display-popup-menu (menu &key owner x y)
  (let* ((owner
          (or owner (default-owner)))
	 (interface
	  (or (when owner
		(top-level-interface owner))
	      (menu-interface menu)
	      (element-interface menu)
	      (default-owner)))
         (rep
          (representation (convert-to-screen interface))))
    (unless owner
      (error "No owner specified for menu ~S" menu))
    (setf (slot-value menu 'parent) interface)
    (setf (menu-interface menu) interface)
    (update-parent-hierarchy menu)
    (capi-library:representation-popup-menu rep menu owner x y)))

