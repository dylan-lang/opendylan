;; Interface to optimization coloring.

(defvar color-not-all-methods-known            "Magenta")
(defvar color-failed-to-select-where-all-known "Red")
(defvar color-lambda-call                      "Blue")
(defvar color-inlining                         "DimGray")
(defvar color-slot-accessor-fixed-offset       "ForestGreen")
(defvar color-eliminated                       "Pink")
(defvar color-dynamic-extent                   "DarkTurquoise")
(defvar color-program-notes                    "Yellow")
(defvar color-background                       "white")

(defvar color-bogus-upgrade                    "Orange")

(defun find-dylan-library ()
  (let ((lid-files (find-dylan-lid-files)))
    (save-excursion
      (if lid-files
	(let ((try-lid (car lid-files)))
	  (set-buffer (find-file-noselect try-lid))
	  (goto-char (point-min))
	  (let ((found 
		  (re-search-forward "[Ll]ibrary:[ \t]*\\([-a-z0-9]*\\)")))
	    (if found
	      (buffer-substring 
	        (match-beginning 1)
		(match-end 1)))))))))

(defun find-dylan-lid-files ()
  (directory-files "." t ".*\\.lid" t))

(defun dylan-color-file ()
  (let* ((path (buffer-file-name))
	 (name (file-name-nondirectory path))
	 (stem (substring name 0 (string-match "\\.[^.]*$" name)))
	 (library (find-dylan-library)))
    (expand-file-name
      (personal-dylan-build-file-name 
       (concat "/" library "/" stem ".el")))))

(defun color-foregrounds (color l)
  (save-excursion
    (while (not (null l))
      (let* ((spec (car l))
           (sl (car spec)) (sc (car (cdr spec)))
         (el (car (cdr (cdr spec)))) (ec (car (cdr (cdr (cdr spec))))))
        (goto-char 1) (forward-line (- sl 1)) (forward-char sc)
        (set-mark (point))
        (goto-char 1) (forward-line (- el 1)) (forward-char ec)
        (facemenu-set-foreground color))
      (setq l (cdr l)))))

(defun color-backgrounds (color l)
  (save-excursion
    (while (not (null l))
      (let* ((spec (car l))
           (sl (car spec)) (sc (car (cdr spec)))
         (el (car (cdr (cdr spec)))) (ec (car (cdr (cdr (cdr spec))))))
        (goto-char 1) (forward-line (- sl 1)) (forward-char sc)
        (set-mark (point))
        (goto-char 1) (forward-line (- el 1)) (forward-char ec)
        (facemenu-set-background color))
      (setq l (cdr l)))))

(defun color-optimizations ()
  "Color the current Dylan file with recorded optimization information"
  (interactive)
  (let ((file (dylan-color-file)))
    (setq old-buffer-read-only buffer-read-only)
    (setq buffer-read-only nil)
    (point-to-register 1)
    (end-of-buffer) 
    (facemenu-set-foreground "black" 1)
    (facemenu-set-background color-background 1)
    (register-to-point 1)

    (message "Using color file: %s" file)
    (load-file file)
    (message "Used color file: %s" file)

    (cond
      (old-buffer-read-only
        (setq buffer-read-only old-buffer-read-only)
        (set-buffer-modified-p nil))) ))

(if dylan-mode-map
    (progn
      (define-key dylan-mode-map [menu-bar dylan-misc dylan-color-optimizations] '("Color Optimizations" . color-optimizations))))

;; eof
