(in-package :capi-tk-lib)

;;; It's ok - this is the only method.  We need to kill it because we're
;;; changing the arglist.

(FMAKUNBOUND (QUOTE CREATE-DIALOG-REPRESENTATION))

;;; This function seems to be shaken out of some people's images
(defun tk-dialog-pop-up-from ()
  (let ((top-level-p (eq *top-level-owns-dialog* t)))
    (or (and top-level-p
             capi::*top-level-interface*
	     (capi:representation capi::*top-level-interface*))
        (and (or (eq *top-level-owns-dialog* :echo)
                 top-level-p)
	     (let ((echo (editor::find-echo-contact
		          nil)))
	       (and echo
                    (if (typep echo 'clue:contact)
                        echo
                      (capi:representation echo))))))))

(defmethod create-dialog-representation (self (parent clue::root) grab-mouse
                                         &rest args
                                         &key top-level-class-name
                                              pop-up-from)
  (apply 'make-capi-contact self 
	 (or top-level-class-name 'capi-dialog)
	 :parent parent
	 :override-redirect (if grab-mouse :on :off)
	 :title (when (and (stringp (interface-title self))
			   (not (string= (interface-title self) "")))
                  (interface-title self))
	 :message (interface-message self)
	 :pop-up-from (or pop-up-from (tk-dialog-pop-up-from))
         args))

(defmethod display-dialog-representation ((representation capi-dialog) grab-mouse
                                          &rest rest-args
                                          &key immediate-return
                                          &allow-other-keys)
  (setf (slot-value representation 'tk::grab-mouse) (if grab-mouse :yes :no))
  (multiple-value-prog1
      (apply 'tk::popup-prompt representation rest-args)
    (unless immediate-return
      (clue:destroy representation))))

(defmethod hide-representation ((contact capi-dialog) iconify)
  (let ((shell (tk::contact-root-shell contact)))
    (if iconify
        (setf (clue:contact-state shell) :iconic)
      (setf (clue:contact-state shell) :withdrawn))))

(defmethod representation-min-width ((contact capi-dialog))
  (with-slots ((title tk::title) (gcontext tk::gcontext) (margin tk::margin))
      contact
    (+ (if title
           (tk::text-width gcontext title)
           0)
       margin margin)))

(defmethod representation-visible-p ((self capi-dialog))
  (let ((contact (clue:contact-parent self)))
    (and (clue:realized-p contact)
         (clue:mapped-p   contact))))

