(in-package :editor)

(system::without-warning-on-redefinition

  (let ((file (merge-pathnames "dylan-bugs" *load-pathname*)))
    (defun load-the-mailer-for-dylan (f args)
      (unless (find-package :mail)
        (do-demand-pre-loads :mail))
      (load file)))

   (load-on-demand :keyword :dylan-bug-commands
                  :functions '(report-dylan-bug report-dylan-doc-bug)
                  :function 'load-the-mailer-for-dylan)

  (defcommand "Report Dylan Bug" (p)
       "Send a DylanWorks bug report"
       "Send a DylanWorks bug report"
    (declare (ignore p))
    (report-dylan-bug))

  (defcommand "Report Dylan Doc Bug" (p)
       "Send a DylanWorks environment bug report"
       "Send a DylanWorks environment bug report"
    (declare (ignore p))
    (report-dylan-doc-bug))

)

;; eof
