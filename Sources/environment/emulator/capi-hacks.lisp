(in-package "CL-USER")

;;; CAPI hacks

(defmethod editor-pane-text-setter ((text string) (pane capi:editor-pane))
  (setf (capi:editor-pane-text pane) text))
