;;----------------------------------------------------------------------------
;; Sort out the library
;;----------------------------------------------------------------------------

(in-package "CAPI-TK-LIB")

(defmethod update-representation-enabled-state ((representation capi-contact)
                                                enabled)
  (setf (tk:contact-enabled representation) enabled))

(defmethod tk:scroll ((self capi-scroll-bar-mixin) bar &key to by-page by-line)
  (declare (ignore bar))
  (when (tk:contact-enabled self)
    (let ((object (capi-object self)))
      (let (how
	    where)
        (cond (to (setq how :move where to))
	      (by-page (setq how :page where by-page))
	      (by-line (setq how :line where by-line)))
        (perform-scroll-bar-callback object how where)
        nil))))
