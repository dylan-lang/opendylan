;;;; Module: DylanWorks Emacs Interface
;;;; Author: Jason Trenouth
;;;; Copyright: Copyright 1997 The Harlequin Group Limited.  All rights reserved.

(require 'dylan-mode)

;;; Assoc list of command symbols and format strings.
(defvar *dw-special-dde-command-formats*
  '())

;;; Default command format string.
;;; Need to put single quotes in to prevent unix shell from
;;; interpreting square brackets etc.
(defvar *dw-default-dde-command-format*
  "\'[%s(\"%s\",\"%s\")]\'")

;;; Builds command string out of symbolic command name,
;;; current file, and current word, and sends it.
(defun dw-dde-command (command-name)
  (dw-maybe-save-buffer)
  (let* ((filename (buffer-file-name))
	 (identifier (current-word))
	 (special-format (assoc command-name *dw-special-dde-command-formats*))
	 (command (if special-format
		      (format special-format filename identifier)
		    (format *dw-default-dde-command-format* command-name filename identifier))))
    (dw-send-dde-command command)))

(defun dw-maybe-save-buffer ()
  (if (and (buffer-modified-p)
	   (or (eq *dw-modified-buffer-handler* 'save)
	       (and (eq *dw-modified-buffer-handler* 'ask)
		    (y-or-n-p "Save modified buffer first?"))))
	  (save-buffer)))

;;; This program needs to be registered with the OS
;;; somehow so that Emacs can find it.
(defvar *dw-dde-program-name* "ddeclient")

(defvar *dw-dde-service-name* "dylanworks")

(defvar *dw-dde-topic-name* "dylanworks")

;;; Sends given command string to ddeclient program
;;; after attaching DDE rubric.
(defun dw-send-dde-command (command)
  (start-process-shell-command
   "ddeclient"
   "*DylanWorks DDE Client Output*"
   *dw-dde-program-name*
   *dw-dde-service-name*
   *dw-dde-topic-name*
   command))

(defvar *dw-auto-describe-p* nil
  "* If non-NIL automatically does DW-DESCRIBE
while moving around a buffer in DW-DYLAN-MODE.")

(defvar *dw-modified-buffer-handler* 'ask
  "* Before calling DylanWorks operation that deals with the whole file,
this variable is checked. The values and their effects are:
	ASK -> ask user
	SAVE -> always save without asking
	otherwise -> don't save buffer
")

(defun dw-describe ()
  "Briefly describe the current Dylan word. Eg show its parameters
if the identifier is bound to a generic function. Also see *DW-AUTO-DESCRIBE*."
  (interactive)
  (dw-dde-command 'describe))

(defun dw-find-definition ()
  "Find the definition of the current Dylan word."
  (interactive)
  (dw-dde-command 'find-definition))

(defun dw-browse ()
  "Browse the value bound to the current Dylan word."
  (interactive)
  (dw-dde-command 'browse))

(defun dw-compile-file ()
  "Compile the current Dylan file. May save file first. See *DW-MODIFIED-BUFFER-HANDLER*."
  (interactive)
  (dw-dde-command 'compile-file))

(defun dw-complete ()
  "Complete the current Dylan word."
  (interactive)
  (dw-dde-command 'complete))

(defun dw-find-uses ()
  "Find the other uses of the current Dylan word."
  (interactive)
  (dw-dde-command 'find-uses))

(defun dw-find-documentation ()
  "Find the documentation associated with the current Dylan word."
  (interactive)
  (dw-dde-command 'find-documentation))

(defun dw-view-project ()
  "Find and raise the Project Controller."
  (interactive)
  (dw-dde-command 'view-project))

(defun dw-augment-key-map ()
  (define-key dylan-mode-map "\C-c\C-h" 'dw-describe)
  (define-key dylan-mode-map "\M-." 'dw-find-definition)
  (define-key dylan-mode-map "\C-c\C-b" 'dw-browse)
  (define-key dylan-mode-map "\C-c\C-c" 'dw-compile-file)
  (define-key dylan-mode-map "\e\C-i" 'dw-complete)
  (define-key dylan-mode-map "\C-c\C-u" 'dw-find-uses)
  (define-key dylan-mode-map "\C-c\C-d" 'dw-find-documentation)
  (define-key dylan-mode-map "\C-c\C-p" 'dw-view-project)
)

(defun dw-augment-menu-bar ()
  (define-key dylan-mode-map [menu-bar dylanworks]
    (cons "DylanWorks" (make-sparse-keymap)))
  (define-key dylan-mode-map [menu-bar dylanworks view-project]
    '("View Project" . dw-view-project))
  (define-key dylan-mode-map [menu-bar dylanworks find-documentation]
    '("Find Documentation" . dw-find-documentation))
  (define-key dylan-mode-map [menu-bar dylanworks find-uses]
    '("Find Uses" . dw-find-uses))
  (define-key dylan-mode-map [menu-bar dylanworks find-definition]
    '("Find Definition" . dw-find-definition))
  (define-key dylan-mode-map [menu-bar dylanworks compile-file]
    '("Compile File" . dw-compile-file))
  (define-key dylan-mode-map [menu-bar dylanworks complete]
    '("Complete" . dw-complete))
  (define-key dylan-mode-map [menu-bar dylanworks describe]
    '("Describe" . dw-describe))
  (define-key dylan-mode-map [menu-bar dylanworks browse]
    '("Browse" . dw-browse))
)

(dw-augment-key-map)
(dw-augment-menu-bar)
