#||
;;; Superseded by patch clue-color/0005.lisp

(in-package "COLOR")

(defmethod %unconvert-color ((screen xlib:screen) datum)
  (let* ((xcolor (first (xlib::query-colors
                         (xlib::screen-default-colormap screen)
			 (list datum)))))
    (make-rgb (xlib::color-red xcolor)
              (xlib::color-green xcolor)
              (xlib::color-blue xcolor))))
         
(defmethod %unconvert-color ((colormap xlib:colormap) datum)
  (let* ((xcolor (first (xlib::query-colors colormap (list datum)))))
    (make-rgb (xlib::color-red xcolor)
              (xlib::color-green xcolor)
              (xlib::color-blue xcolor))))
||#


(in-package "TK")

(sys::without-warning-on-redefinition

(defevent grid :exposure display-region)

) ;; End without-warning-on-redefinition


(in-package "GP")

;; Unbind it first so we don't get any redefinition with different argument problems
(fmakunbound 'get-string-extent)
(fmakunbound '%get-string-extent)

(eval-when (compile)
  (defmacro font-device-string-trampoline (library-function string &rest other-args)
    `(let ((device (find-font-device port)))
       (,library-function device ,string (do-lookup-font port device font) ,@other-args)))
)

(sys::without-warning-on-redefinition

(defmethod %get-string-extent
    ((port compatible-port) string font &optional (start 0) end)
  (let ((font  (ensure-get-port-font port font)))
    (multiple-value-bind (w as de left right)
	(xlib::text-extents font string :start start :end end)
      (values left
              (- (xlib::max-char-ascent font))
              (+ left w)
              (xlib::max-char-descent font)))))

(defmethod %get-string-extent
    ((port xlib:display) string font &optional (start 0) end)
  (let ((font  (ensure-get-port-font port font)))
    (multiple-value-bind (w as de left right)
	(xlib:text-extents font string :start start :end end)
        (declare (ignore as de right))
      (values left 
              (- (xlib:max-char-ascent font))
              (+ left w)
              (xlib:max-char-descent font)))))

#||
(defmethod %get-string-extent
    ((device capi-xt-lib::screen-widget) string font &optional (start 0) end)
  (let ((overall (make-x-char-struct)))
    (multiple-value-bind (direction ascent descent)
        (x-text-extents (xfont-struct font) string (length string) 0 0 0 overall)
      (let ((left (x-char-struct->lbearing overall)))
        (values left
                (- ascent)
                (+ left (x-char-struct->width overall))
                descent)))))
||#

#||
(defmethod %get-string-extent
    ((port record-graphics-ports) string font &optional (start 0) end)
  (when (and font (not (typep font 'generic-font)))
    (error "Recording should only deal with generic font ~s" font))
  (if font 
      (%get-string-extent (record-compatible-port port) string font)
    (%get-string-extent (record-compatible-port port) string)))             
||#

(defun get-string-extent (port string &optional font (start 0) end)
  (font-device-string-trampoline %get-string-extent string start end))

) ;; End without-warning-on-redefinition


#||
(in-package "PS-GP")

(sys::without-warning-on-redefinition

(defmethod gp::%get-string-extent
    ((port ps-port-font-mixin) string font &optional (start 0) end) 
  (unless font (setq font (gp::get-port-font port)))
  (let ((fm (ps-font-metrics font))
        (size (ps-font-size font))
	(ascent (ps-font-ascent font))
	(descent (ps-font-descent font)))
    (values 0
	    (- ascent)
	    (scale-font-dim (get-absolute-string-width string fm) size)
	    (- descent))))

) ;; End without-warning-on-redefinition
||#
