(defun unclass-while-visiting ()
  "Temporary \"major mode\" used for .class files, to dejava the contents."
  (if (and (not (null buffer-file-name))
	   (string-match "\\.class$" buffer-file-name))
      (set-visited-file-name
       (concat (substring buffer-file-name 0 (match-beginning 0))
	       ".list")))
  (message "dejavaing...")
  (let ((buffer-read-only nil))
    (shell-command-on-region (point-min) (point-max) "dejava -q" t))
  (goto-char (point-min))
  (message "dejavaing...done")
  (set-buffer-modified-p nil)
  (normal-mode))

(setq auto-mode-alist
      (cons '("\\.class$" . unclass-while-visiting) auto-mode-alist))

