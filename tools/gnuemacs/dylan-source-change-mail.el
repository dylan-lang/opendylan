;; report-dylan-source-change.el
;;
;; emacs support for sending mail to dylan-source-changes
;;             and dylan-promotion
;; options for use:
;;  a) load this file by hand
;;  b) have your .emacs load this file
;;     (you may include a test that the file can be found)
;;  c) have your .emacs load this file and then bind
;;     report-dylan-source-change to your favorite keystroke.
;;
;;  once loaded you can use meta-x report-dylan-source-change
;;  or use the key binding if you set it up.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note: hardwired ../../doc/org/source-change-template.txt
;; relative from this file. Will fail if not found. 

(defvar dylan-source-change-root-location nil)

;; unfortunately load-file-name does not exist in 19.28.1
(setq dylan-source-change-root-location
      (if (or (string-match "i386" system-configuration)
	      (string-match "i686" system-configuration)
	      (string-match "powerpc" system-configuration))
	  (file-name-directory
	   (directory-file-name 
	    (file-name-directory
	     (directory-file-name
	      (file-name-directory load-file-name)))))
	"/u/dylan/"))

(defun report-dylan-source-change ()
  "Report a Dylan source change"
  (interactive)
  (mail nil "Developers@FunctionalObjects.COM")
  (progn
    (mail-to)
    (insert-string "\nReply-to: Developers@FunctionalObjects.COM"))
  (mail-text)
  (insert-file-contents (concat dylan-source-change-root-location "doc/org/source-change-template.txt"))
  (mail-subject))

(defun report-dylan-promotion ()
  "Report a promotion to the D-kan branch"
  (interactive)
  (mail nil "Developers@FunctionalObjects.COM")
  (progn
    (mail-to)
    (insert-string "\nReply-to: Developers@FunctionalObjects.COM"))
  (mail-text)
  (insert-file-contents (concat dylan-source-change-root-location "doc/org/source-change-template.txt"))
  (mail-subject))

(if (string-match "GNU" (emacs-version))
    (if dylan-mode-map
	(progn
	  (define-key dylan-mode-map [menu-bar dylan-misc dylan-promotion] '("Report Promotion" . report-dylan-promotion))
	  (define-key dylan-mode-map [menu-bar dylan-misc dylan-source-change] '("Report Source Change" . report-dylan-source-change)))))

(provide 'dylan-source-change-mail)
