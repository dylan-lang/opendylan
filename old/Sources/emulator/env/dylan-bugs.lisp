(in-package editor)


(eval-when (:compile-toplevel)
  (do-demand-pre-loads :mail))

(system::without-warning-on-redefinition

 (defconstant *dylan-bug-report-format*
   "Site: ~@[~A~]~%~@
    LW Version: ~A~%~@
    Machine & OS: ~A~@[ (~A)~]~@[, ~A~]~@[ ~A~]~%~@
    Display & window manager: ~A:~D~@[, ~A~]~%~@
    People to CC: ~@[~A~]~%~@
    Description: <a more detailed description>~%~@
    Category: ~%~@
    Visibility: ~%~@
    Difficulty of workaround: ~%~@
    Repeat by: ~%~@
    Test Suite Entry: ~%~@
    Backtrace:")

 (defun get-lw-version-data ()
   (with-output-to-string (str-out)
     ;; LW Version & save history			 
     (format str-out "~{~A~%~}~12T~A" 
	     (mapcar #'sys::product-title (sys::products))    
	     (subseq ccl::*entry-message*                     
		     (search "Saved by" ccl::*entry-message*)))
     ;; Demand load features
     (let ((feats '()) (*package* (find-package "SYS")))
       (sys:map-lod-features
        #'(lambda (name feature)
	    (when (sys::lod-feature-loaded-p feature)
	      (push name feats))))
       (format str-out "~@[~%Demand-loaded features: ~{~<~% ~1:; ~S~>~^,~}~%~]" feats))     
     (scm::print-patch-banner :stream str-out)
     (format str-out "~2%Foreign modules: ~{~%~2T~A~}"
	     (loop for module-name being each hash-key in foreign::*defined-modules*
		   collect module-name))))
 

 (defun report-dylan-bug ()
   (do-demand-pre-loads :mail)
   (when-let (subject (editor:prompt-for-string :prompt "A short bug description:"))
     (mail::find-a-mail-tool
      :contents (copy-seq ; because it needs a simple string :-(
		 (let ((report (make-array 500 :element-type 'base-character
					   :fill-pointer 0 :adjustable t)))
		   (format report *dylan-bug-report-format*
			   (long-site-name)                                ;; Site
                           (get-lw-version-data)                           ;; LW Version
			   (machine-instance) (machine-type)               
			   (software-type) (software-version)
			   (xlib::display-host clue::*display*)
			   (xlib::display-display clue::*display*)
			   (nth-value 1 (tk:window-manager))
			   (get-user-name) ;; put user in CC field
			   )
		   (with-output-to-string (*debug-io* report)
		     (unless (dbg:bug-backtrace nil)
		       (format *debug-io* 
			       " <please use (dbg:bug-backtrace nil)>~%")))
		   report))
      :subject subject
      :leave-point-regexp "<a more detailed description>"
      :to "dylan-bugs"   
      :other-headers 
      `(,(format nil "Reply-To: dylan-bugs-discussion, ~A" (get-user-name))))))
 
 

 (defconstant *dylan-doc-bug-template*
   "
Document title: 

Edition: 

Revision: 

Page(s): 

Problem: 

Fixed by: 

")


 (defun report-dylan-doc-bug ()
   (do-demand-pre-loads :mail)
   (when-let (subject (editor:prompt-for-string :prompt "A short bug description:"))
     (let ((sender (get-user-name)))
       (mail::find-a-mail-tool
	:to "dylan-doc-bugs"  
	:mail-self-blind sender
	:subject subject
	:contents *dylan-doc-bug-template*
	:other-headers `(,(format nil "Reply-To:  dylan-doc, ~A" sender))
	:leave-point-regexp (format nil "~%~%Edition")
	))))
 )

;; eof
