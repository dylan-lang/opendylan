;;;; Module: Online Doc for Interim Environment
;;;; Author: Jason Trenouth
;;;; Copyright: Copyright 1997 The Harlequin Group Limited.  All rights reserved.

(require 'cl)
(require 'dylan-mode)
(require 'dylan-debug)

(defstruct dylan-doc
  name
  index
  url
  patterns)

(defvar dylan-doc-alist nil)

(defun add-dylan-doc (name &rest init-args)
  (let ((new-value (apply 'make-dylan-doc :name name init-args))
	(old-entry (assoc name dylan-doc-alist)))
    (if old-entry
	(rplacd old-entry new-value)
      (setq dylan-doc-alist (acons name new-value dylan-doc-alist)))))

(add-dylan-doc 'drm
	       :index "/public-html/dylan-framed/drm/drm-120.html"
	       :url "http://webhost/~dylan/dylan-framed/drm/"
	       :patterns '("<A +HREF=\"\\(drm-[0-9]+.html\\(#MARKER-[0-9]+-[0-9]+\\)?\\)\">\\(" "\\)</A>"))

(defvar dylan-doc-temp "c:/temp/dylan-doc.url")

(defvar dylan-doc-web-browser-program "start") ; uses file type associations

(defvar dylan-doc-lookup-state nil)

(defun dylan-doc-lookup-next ()
  (interactive)
  (dylan-doc-lookup 4))

(defun dylan-doc-lookup (next-p)
  (interactive "p")
  (when (or (= next-p 1) (null dylan-doc-lookup-state))
    (setq dylan-doc-lookup-state dylan-doc-alist))
  (let ((doc (cdar dylan-doc-alist))
	(word (dylan-doc-escape-html-chars (current-word))))
    (if doc
	(multiple-value-bind (found link anchor)
	    (dylan-doc-parse-index doc next-p word)
	  (if found
	      (progn
		(dylan-doc-write-shortcut doc link)
		(start-process-shell-command "web-browser"
					     "*Web Browser*"
					     (format "%s %s"
						     dylan-doc-web-browser-program
						     dylan-doc-temp))
		(message (concat "Found match: " anchor " at " link)))
	    (progn
	      (setq dylan-doc-lookup-state (cdr dylan-doc-lookup-state))
	      (if dylan-doc-lookup-state
		  (dylan-doc-lookup 4)
		(message (concat "No more matches for \"" word "\""))))))
      (message "No online Dylan doc registered"))))

(defun dylan-doc-parse-index (doc next-p word)
  (save-excursion
    (let ((pattern (concat (first (dylan-doc-patterns doc))
			   word
			   (second (dylan-doc-patterns doc))))
	  (index (find-file-noselect (tilde-dylan-file-name (dylan-doc-index doc)))))
      (set-buffer index)
      (when (= next-p 1)
	(goto-char (point-min)))
      (let ((found (re-search-forward pattern nil t)))
	(if found
	    (values found (match-string 1) (match-string 3))
	  (values nil nil nil))))))

(defun dylan-doc-write-shortcut (doc link)
  (save-excursion
    (let ((temp (find-file-noselect dylan-doc-temp)))
      (set-buffer temp)
      (goto-char (point-min))
      (kill-line 2)
      (insert "[InternetShortcut]")
      (newline)
      (insert "URL=")
      (insert (dylan-doc-url doc))
      (insert link)
      (newline)
      (save-buffer))))

(defun dylan-doc-escape-html-chars (str)
  (reduce 'concat str :initial-value "" :key 'dylan-doc-escape-1-html-char))

(defun dylan-doc-escape-1-html-char (char)
  (let ((special (assoc char dylan-doc-html-escaped-chars-alist)))
    (if special
	(cdr special)
      (char-to-string char))))

(defvar dylan-doc-html-escaped-chars-alist
  (list (cons (string-to-char "<") "&lt")
	(cons (string-to-char ">") "&gt")))

(if dylan-mode-map
    (progn
      (define-key dylan-mode-map "\^c\^h" 'dylan-doc-lookup)
      (define-key dylan-mode-map [menu-bar dylan-misc dylan-doc-next] '("Next Help" . dylan-doc-lookup-next))
      (define-key dylan-mode-map [menu-bar dylan-misc dylan-doc] '("Find Help On Binding" . dylan-doc-lookup))))

(provide 'dylan-doc)

