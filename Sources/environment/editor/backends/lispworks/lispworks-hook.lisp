(in-package "CL-USER")

;; Taken from ~ext/editor/grep-browser.lisp
(defun lispworks-editor-goto-line (buffer line)
  (editor::with-point ((po (editor::buffer-%start buffer)))
    (if (editor::line-offset po line)
        (let ((point (editor::buffer-point buffer)))
	  (editor::move-point point po))
      (editor::editor-error "Line Number ~a does not exist in the current buffer" line))))

(defmethod lispworks-editor-show-location
    ((file string) from-line from-col)
  (let* ((truename (truename file))
         (buffer (editor:find-file-command nil truename)))
    (editor:goto-buffer buffer (editor:current-window))
    (when from-line
;; I'd like to use this simple call but I'm having problems with LW errors.
;      (editor:goto-line-command from-line))
      (lispworks-editor-goto-line buffer from-line))
;; Movement to a column is disabled for now, because of LW error problems.
;    (when from-col
;      (progn
;        (editor:beginning-of-line-command nil)
;        (editor:forward-character-command from-col)))
    (when-let (window (first (editor:buffer-windows buffer)))
      (editor:update-buffer-window window))
    (cond
     ((and from-line from-col)
      (editor::quiet-message "Editing ~A at line ~D, column ~D"
			     file from-line from-col))
     (from-line
      (editor::quiet-message "Editing ~A at line ~D" file from-line))
     (from-col
      (editor::quiet-message "Editing ~A at column ~D" file from-col))
     (t
      (editor::quiet-message "Editing ~A" file)))))

;; Always opens a new buffer.
(defmethod lispworks-editor-open-editor ()
  (let
      ((suitable-buffer
	(find-if
	 '(lambda (buf)
	   (not (editor:buffer-flag buf)))
         editor:*buffer-list*))
       (suitable-buffer-with-window
	(find-if
	 '(lambda (buf)
	   (and (not (editor:buffer-flag buf))
		(first (editor:buffer-windows buf))))
         editor:*buffer-list*)))
  (cond
     ; If we have an ordinary editor buffer with a window, use that.
     (suitable-buffer-with-window
      (editor:goto-buffer suitable-buffer-with-window nil))
     ; Otherwise, if we have any ordinary buffers, make a window for one.
     (suitable-buffer
      (editor:goto-buffer suitable-buffer nil))
     ; If not, make a brand-new buffer.
     (t
      (editor:new-buffer-command)))))
