;;;; Module: Example Config File for Dylan-O-Matic
;;;; Author: Jason Trenouth
;;;; Copyright: Copyright 1997 The Harlequin Group Limited.  All rights reserved.

;;; ----------------------------------------------------------------------
;;; UTILITIES
;;; ----------------------------------------------------------------------

(defun add-load-path (new-path)
  (if (not (member new-path load-path))
      (setq load-path (cons new-path load-path))))
  
(defun add-auto-mode (extension mode)
  (if (not (assoc extension auto-mode-alist))
      (setq auto-mode-alist (cons (cons extension mode) auto-mode-alist))))

;;; ----------------------------------------------------------------------
;;; CUSTOMIZATION
;;; ----------------------------------------------------------------------

(setq tilde-dylan-directory "e:")

;;; ----------------------------------------------------------------------
;;; HOPE
;;; ----------------------------------------------------------------------

(setq hope-source-directories
  '(("e:/" "D" "trunk")))

(setq hope-work-directories
  '(("c:/users/jason/dylan" "D" "trunk")
    ("c:/users/jason/dylan/emacs-lisp" "HOPEemacs-lisp" "trunk")))
  
(add-load-path "e:/tools/hope/")

(autoload 'hope-mode "hope" "" t)

(add-hook 'dylan-mode-hook 'hope-mode)

;;; ----------------------------------------------------------------------
;;; DYLAN
;;; ----------------------------------------------------------------------

(add-load-path "e:/tools/gnuemacs/")

(add-auto-mode "\\.dylan\\'" 'dylan-mode)

(add-auto-mode "\\.lid\\'" 'dylan-mode)

(autoload 'dylan-mode "dylanomatic" "" t)

;;; ----------------------------------------------------------------------
;;; GNU SERVER
;;; ----------------------------------------------------------------------

(add-load-path "c:/hqbin/win32/gnuserv")

(require 'gnuserv)

(gnuserv-start)

;; (setq gnuserv-frame (selected-frame)) ;; use this to re-use initial Emacs window for edits

;;; ----------------------------------------------------------------------
;;; SMTP MAIL
;;; ----------------------------------------------------------------------

(add-load-path "e:/tools/gnuemacs/")

(setq send-mail-function 'smtpmail-send-it)
(setq smtpmail-default-smtp-server "mailhost")
(setq smtpmail-smtp-service "smtp")
(setq smtpmail-local-domain "long")
(setq smtpmail-debug-info t)
(setq smtpmail-code-conv-from nil)

(autoload 'smtpmail-send-it "smtpmail" "" t)

;;; ----------------------------------------------------------------------
;;; DYLAN PARAMS
;;; ----------------------------------------------------------------------

(add-hook 'dylan-mode-hook
	  '(lambda ()
	     (local-set-key "(" 'auto-display-method-params)))

