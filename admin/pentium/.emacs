;always BCC myself
(setq mail-self-blind t)

;inhibit GNU init message
(setq inhibit-startup-message t)

(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(setq lisp-indent-offset nil)

(display-time)

(fset 'file-away
   "oood")

(defun 80-columns ()
  (interactive)
  (send-string-to-terminal "\033[?3l")
  (set-screen-width 80))
(defun 132-columns ()
  (interactive)
  (send-string-to-terminal "\033[?3h")
  (set-screen-width 132))

(defun shell-expand-file-name (); Added 8/26/86 by Mark Ardis
  "Expand the file name before point."
  (interactive)
  ; Local Variables
  (let (place start path stop name full-name completions)
    ; Body
    (message "Completing...")
    (setq place (point))
    (beginning-of-line)
    (setq stop (point))
    (goto-char place)
    (if (re-search-backward "[ \"']" stop t)
      (progn
	(forward-char)
	(setq start (point))
	(goto-char place)
	(if (search-backward "/" start t)
	      (progn
		    (forward-char)
		        (setq path (buffer-substring start (point)))
			  ) ; progn
	  ; else
	    (progn
	          (setq path "")
		      (goto-char start)
		        ) ; progn
	    ) ; if (search-backward "/" start t)
        (setq name (buffer-substring (point) place))
        (setq full-name (file-name-completion name
					            (concat default-directory path)))
	(if (not full-name)
	      (progn
		    (goto-char place)
		        (error "Cannot complete this!")
			    ) ; progn
	    ) ; if
	(if (not (equal full-name t))
	        (progn
		        (delete-region (point) place)
			      (insert full-name)
			            (setq completions (file-name-all-completions
						          name (concat default-directory path)))
				          (if (> (length completions)
						      1)
					        (progn
						      (ding)
						          (message "Completed, but not unique!" completions)
							      ;;(comint-dynamic-list-filename-completions)
							      ) ; progn
					    (message "Completed.")
					    ) ; if
					        ) ; progn
	    ; else
	    (progn
	          (goto-char place)
		      (message "Completed---already complete.")
		          ) ; progn
	      ) ; if
      ) ; progn
    ; else
      (progn
	(goto-char place)
	(error "Nothing to complete here!")
	) ; progn
    ) ; if (search-backward " " stop t)
  ) ; let
) ; defun shell-expand-file-name

;;; Bind this to tab in shell modes

(require 'shell)

(define-key shell-mode-map "\C-I" 'shell-expand-file-name)
;(define-key telnet-mode-map "\C-I" 'shell-expand-file-name)

;(require 'inferior-lisp-mode)

;(define-key inferior-lisp-mode-map "\C-I" 'shell-expand-file-name)
;(define-key lisp-mode-map "\C-C\C-I" 'shell-expand-file-name)

(define-key minibuffer-local-map "\C-I" 'shell-expand-file-name)

;; to use ansi mode on QUME PCT terminal
;; remap bs to delete and ESC-^H to help

;(define-key global-map "\C-H" 'delete-backward-char)

;(define-key global-map "\C-[\C-H" 'help-command)

(setq text-mode-hook 'turn-on-auto-fill)

(setq lisp-mode-hook 'turn-on-auto-fill)

;; Miscellaneous bindings
(global-set-key "\C-x!" 'goto-line)
(global-set-key "\C-xw" 'what-line)
; scroll-to-top
(global-set-key "\C-xt" "\C-u0\C-l")
; scroll-to-bottom
;(global-set-key "\C-xb" "\C-u-1\C-l")
; scroll-up-top
;(global-set-key "\C-|" "\C-p\C-xt")
; scroll-down-bottom

(global-set-key "\C-z" "\C-xb\C-m")
(global-set-key "\C-xq" 'bury-buffer)

(autoload 'enscript-buffer "enscript" nil t)
(autoload 'enscript-region "enscript" nil t)
(autoload 'fax "fax" nil t)

(define-key global-map "\C-x\C-b" 'buffer-menu)

;;;;;;;;;;; DYLAN ;;;;;;;;;;;

;; (load-library "~dylan/tools/gnuemacs/dylan-mode")
;; ;(load-library "dylan-params")



(setq mail-yank-prefix ">   ")
(setq mail-yank-ignored-headers "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^remailed\\|^received:\\|^message-id:\\|^summary-line:\\|^cc:\\|^subject:\\|^in-reply-to:\\|^return-path:")

(setq shell-pushd-regexp "pd")

(setq shell-popd-regexp "qd")

;; (setq shell-file-name "/bin/csh")

(load-file "/home/nosa/emacs-macros")

;; Startup State

(find-file "~/.emacs")
(find-file "~/emacs-macros")

(find-file "~/dylan/admin")
(beginning-of-buffer)
(dired-sort-toggle-or-edit)

(find-file "~/dylan/logs")
(beginning-of-buffer)
(dired-sort-toggle-or-edit)

(find-file "~/mail")
(beginning-of-buffer)
(dired-sort-toggle-or-edit)

(find-file "~/scripts")
;; (find-file "~/dylan/dfmc/harp-cg")

;; (switch-to-buffer "*scratch*")
;; (cd "~/hope")
;; (shell)
;; (rename-buffer "hope")

;; (switch-to-buffer "*scratch*")
;; (cd "~/dylan/dfmc")
;; (shell)
;; (rename-buffer "dfmc")

(switch-to-buffer "*scratch*")
(cd "~/dylan/")
(shell)
(rename-buffer "shell")

(switch-to-buffer "*scratch*")
(shell)
(rename-buffer "runtime")
(setq last-kbd-macro
   "config -b /home/nosa/dylan\C-mcd /home/nosa/dylan/sources/lib/run-time/pentium-linux\C-m")
(call-last-kbd-macro)

;; (switch-to-buffer "*scratch*")
;; (shell)
;; (rename-buffer "disass")
;; (insert "config-dylan")
;; (comint-send-input)
;; (insert "cd /home/nosa/dylan/build/dylan-test-suite-app")
;; (comint-send-input)

(switch-to-buffer "*scratch*")
(shell)
(write-file (concat "~/dylan/logs/dylan" (format-time-string "-%b-%d-%H-%M")))
(rename-buffer "dylan")
(insert "config-dylan")
(comint-send-input)
(insert "cd /home/nosa/dylan/build/build")
(comint-send-input)

(switch-to-buffer "*scratch*")
(shell)
(write-file (concat "~/dylan/logs/dylan-1" (format-time-string "-%b-%d-%H-%M")))
(rename-buffer "1")
(insert "config -rcs /home/nosa/dylan -b /home/nosa/dylan/b1")
(comint-send-input)
(insert "cd /home/nosa/dylan/b1")
(comint-send-input)

;; (switch-to-buffer "*scratch*")
;; (shell)
;; (write-file (concat "~/dylan/logs/dylan-2" (format-time-string "-%b-%d-%H-%M")))
;; (rename-buffer "2")
;; (insert "config -rc /home/nosa/dylan/b1 -s /home/nosa/dylan -b /home/nosa/dylan/b2")
;; (comint-send-input)
;; (insert "cd /home/nosa/dylan/b2")
;; (comint-send-input)

;; (switch-to-buffer "*scratch*")
;; (shell)
;; (write-file (concat "~/dylan/logs/dylan-3" (format-time-string "-%b-%d-%H-%M")))
;; (rename-buffer "3")
;; (insert "config -rc /home/nosa/dylan/b2 -s /home/nosa/dylan -b /home/nosa/dylan/b3")
;; (comint-send-input)
;; (insert "cd /home/nosa/dylan/b3")
;; (comint-send-input)

;; (switch-to-buffer "*scratch*")
;; (shell)
;; (write-file (concat "~/dylan/logs/dylan-4" (format-time-string "-%b-%d-%H-%M")))
;; (rename-buffer "4")
;; (insert "config -rc /home/nosa/dylan/b3 -s /home/nosa/dylan -b /home/nosa/dylan/b4")
;; (comint-send-input)
;; (insert "cd /home/nosa/dylan/b4")
;; (comint-send-input)

;; (find-file "~/dylan/build")
;; (beginning-of-buffer)
;; (dired-sort-toggle-or-edit)

(find-file "~/dylan/lib")
(beginning-of-buffer)
(dired-sort-toggle-or-edit)

(find-file "~/dylan/sources/lib/run-time/pentium-linux")
(beginning-of-buffer)
(dired-sort-toggle-or-edit)

(find-file "~/dylan/sources/lib")
(beginning-of-buffer)
(dired-sort-toggle-or-edit)

(find-file "~/info")

;; (pcmail)
;; (find-file-read-only "~/unix-mailbox")


(put 'downcase-region 'disabled nil)


;; (load-file "/home/nosa/tasks.el")
;; (find-file "~/tasks.el")

;; (find-file "/home/nosa/tasks")
;; (beginning-of-buffer)
;; (dired-sort-toggle-or-edit)

(setq grep-command "grep -in ")
