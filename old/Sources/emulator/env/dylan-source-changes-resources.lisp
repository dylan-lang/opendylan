(in-package :editor)

(system::without-warning-on-redefinition
 
 (let ((file (merge-pathnames "dylan-source-changes" *load-pathname*)))
   (defun lod-dylan-source-changes (f args)
     (do-demand-pre-loads :mail)
     (load file)))

 (load-on-demand :keyword :dylan-source-change-commands
		 :functions '(report-dylan-source-changes)
		 :function 'lod-dylan-source-changes)

 (defcommand "Send Source Change Mail" (p)
   "Send a report on changes to dylan sources"
   "Send a report on changes to dylan sources"
   (declare (ignore p))
   ( report-dylan-source-changes))
)

;;eof
