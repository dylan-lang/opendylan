;; report-dylan-bug.el
;;
;; emacs support for sending mail to dylan-bugs
;; a rather dumb version of the lispwork version
;; this makes no attempt to get information from 
;; a running lisp image. 
;;
;; options for use:
;;  a) load this file by hand
;;  b) have your .emacs load this file
;;     (you may include a test that the file can be found)
;;  c) have your .emacs load this file and then bind
;;     report-dylan-bug to your favorite keystroke.
;;
;;  once loaded you can use meta-x report-dylan-bug
;;  or use the key binding if you set it up.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note: hardwired ../../doc/org/email-bug-template.txt
;; relative from this file. Will fail if not found. 

(defvar dylan-bug-location nil)

;; unfortunately load-file-name does not exist in 19.28.1
(setq dylan-bug-root-location
      (if (or (string-match "i386" system-configuration)
	      (string-match "i686" system-configuration)
	      (string-match "powerpc" system-configuration))
	  (file-name-directory
	   (directory-file-name 
	    (file-name-directory
	     (directory-file-name
	      (file-name-directory load-file-name)))))
	"/u/dylan/"))

(defun report-dylan-bug ()
  "Report a dylan bug to dylan-bugs"
  (interactive)
  (mail nil "dylan-bugs" nil "dylan-bugs-discussion")
  (mail-text)
  (insert-file-contents (concat dylan-bug-root-location "doc/org/email-bug-template.txt"))
  (goto-char (point-min))
  (while (search-forward "People to CC:" nil t)
    (replace-match (concat  "People to CC: " (user-login-name)) nil t))   
  (goto-char (point-min))
  (while (search-forward "Site:" nil t)
    (replace-match "Site: Harlequin" nil t))    
  (goto-char (point-min))
  (while (search-forward "In-reply-to: " nil t)
    (replace-match (concat "Reply-To: " (user-login-name) ", ") nil t))    
  (mail-subject))

(if (string-match "GNU" (emacs-version))
    (if dylan-mode-map
	(define-key dylan-mode-map [menu-bar dylan-misc dylan-bug] '("Report Bug" . report-dylan-bug))))

(provide 'dylan-bug)
