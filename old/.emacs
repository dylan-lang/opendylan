;; (setq debug-on-error t)

(display-time)
(line-number-mode 1)

(define-key esc-map "o" 'kill-rectangle)	 ;no previous binding
(define-key esc-map "p" 'yank-rectangle)	 ;no previous binding 
(define-key esc-map "]" 'forward-sexp)
(define-key esc-map "[" 'backward-sexp)

;;--- andrewa: These are too non-standard, so I've removed them.
;;(define-key ctl-x-map "f" 'find-file)			;was set-fill-column
;;(define-key ctl-x-map "\^f" 'find-file-other-window)  ;was find-file

(global-set-key "\C-h" 'delete-backward-char)


;;; ----------------------------------------------------------------------
;;; Dylan mode
;;; ----------------------------------------------------------------------

(defun add-load-path (new-path)
  (if (not (member new-path load-path))
      (setq load-path (cons new-path load-path))))
  
(defun add-auto-mode (extension mode)
  (if (not (assoc extension auto-mode-alist))
      (setq auto-mode-alist (cons (cons extension mode) auto-mode-alist))))

;; Dylan stuff
(setq dylan-mode-hook
      '(lambda ()
	 (local-set-key "("    'auto-display-method-params)))

(add-auto-mode "\\.dylan\\'" 'dylan-mode)
(add-auto-mode "\\.lid\\'"   'dylan-mode)

(add-load-path (expand-file-name "~dylan/tools/gnuemacs"))

(setq tilde-dylan-directory    "~dylan")

(load-library "dylanomatic")
