;;;; Module: Top Level Load File for Dylan-O-Matic
;;;; Author: Jason Trenouth
;;;; Copyright: Copyright 1997 The Harlequin Group Limited.  All rights reserved.

(require 'cl)

;;; ----------------------------------------------------------------------
;;; Default customization variables
;;; ----------------------------------------------------------------------

(defun maybe-pc-file-name (file)
  (if (memq (framep (selected-frame)) '(pc win32))
      (substitute (string-to-char "\\") (string-to-char "/") file)
    file))

(defvar tilde-dylan-drive nil)

(defun setup-tilde-dylan-directory ()
  (or (and tilde-dylan-drive
	   (concat (char-to-string tilde-dylan-drive) ":"))
      (call-interactively 'tilde-dylan-ask-directory)))

(defun strip-trailing-slashes (name)
  (substring name 0 (string-match "/*$" name)))

(defun tilde-dylan-ask-directory (directory)
  (interactive "DWhere is ~dylan mounted on your PC? ")
  (strip-trailing-slashes directory))

(defvar tilde-dylan-directory (setup-tilde-dylan-directory))

(defvar personal-dylan-directory 
  (strip-trailing-slashes
   (or (getenv "WEBSTER_PERSONAL_ROOT")
       (maybe-pc-file-name (expand-file-name "~/dylan")))))

(defvar dylan-release-name "pentium-kan")

(defvar dylan-admin-release-name dylan-release-name)

(defvar dylan-platform
  (or (getenv "WEBSTER_PLATFORM_NAME") "x86-win32"))

(defun tilde-dylan-file-name (file)
  (concat tilde-dylan-directory file))

(defun personal-dylan-file-name (file)
  (concat personal-dylan-directory file))

(defun get-dylan-release-directory ()
  (strip-trailing-slashes
   (or (getenv "DYLAN_RELEASE_ROOT")
       (getenv "WEBSTER_SYSTEM_ROOT")
       (tilde-dylan-file-name
	(concat "/releases/" dylan-release-name)))))

(defvar dylan-release-directory (get-dylan-release-directory))

(defun dylan-release-file-name (file)
  (concat dylan-release-directory file))

(defun tilde-dylan-admin-file-name (file)
  (tilde-dylan-file-name (concat "/releases/" dylan-admin-release-name "/admin/" file)))

(defun tilde-dylan-build-file-name (file)
  (dylan-release-file-name (concat "/build/" dylan-platform file)))

(defun tilde-dylan-install-file-name (file)
  (dylan-release-file-name (concat "/install/" dylan-platform file)))

(defun personal-dylan-build-file-name (file)
  (personal-dylan-file-name (concat "/build/" dylan-platform file)))

(defun personal-dylan-install-file-name (file)
  (personal-dylan-file-name (concat "/install/" dylan-platform file)))

;;; ----------------------------------------------------------------------
;;; Load the Dylan support libraries
;;; ----------------------------------------------------------------------

(require 'dylan-mode)
(require 'dylan-mode-ex)
(require 'dylan-bug)
(require 'dylan-source-change-mail)
(load-library "dylan-params") ;; not provided yet
(load-library "dylan-optimization-coloring") ;; not provided yet
(load-library "dylan-tempo") ;; not provided yet

(if (or (string-match "i386" system-configuration)
	(string-match "i686" system-configuration)
	(string-match "powerpc" system-configuration))
    (progn
      (define-key menu-bar-tools-menu [dylan-divider] '("--"))
      (require 'dylan-debug)
      (require 'dylan-compile)
      (require 'dylan-doc)))

(provide 'dylanomatic)
