(in-package editor)

(eval-when (:compile-toplevel)
  (do-demand-pre-loads :mail))

(defconstant *dylan-source-change-template*
   "
Author: 

Description of changes: 

Related Tasks: 

Bugs Fixed: 

New Files: 

Files Changed: 

Files Removed: 

Reviewed by: 

Testing: 

QA Impact: 

Doc Impact: 

User Impact: 

Build Impact:

Additional Issues Raised: 
")

(defun report-dylan-source-changes ()
  (do-demand-pre-loads :mail)
  (when-let (subject 
	     (editor:prompt-for-string :prompt "A short name for your changes:"))
    (let ((sender (get-user-name)))
      (mail::find-a-mail-tool
       :to "dylan-source-changes"
       :mail-self-blind sender
       :subject subject
       :contents *dylan-source-change-template*
       :other-headers `(,(format nil "Reply-To:  dylan-group, ~A" sender))
       :leave-point-regexp (format nil "~%~%Author")
       ))))

(defun report-dylan-promotion ()
  (do-demand-pre-loads :mail)
  (when-let (subject 
	     (editor:prompt-for-string :prompt "A short name for your changes:"))
    (let ((sender (get-user-name)))
      (mail::find-a-mail-tool
       :to "dylan-promotions"
       :mail-self-blind sender
       :subject subject
       :contents *dylan-source-change-template*
       :other-headers `(,(format nil "Reply-To:  dylan-group, ~A" sender))
       :leave-point-regexp (format nil "~%~%Author")
       ))))
 
