; copyright: 1996 Functional Objects, Inc. All rights reserved.
(in-package :dylan)

(defun room-report ()
  (let* ((room-report (with-output-to-string (*standard-output*) (room)))
         ;;
         (begin-total (+ 10 (search "Total Size " room-report :test #'char= :from-end t)))
         (end-total   (search "K"                 room-report :test #'char= :start2 begin-total))
         ;;
         (begin-free  (+ 5  (search "Free "       room-report :test #'char= :start2 end-total)))
         (end-free    (search "K"                 room-report :test #'char= :start2 begin-free)))
    ;; now we've got bounds on the 2 interesting integers
    (values (read-from-string room-report t :eof :start begin-total :end end-total)
            (read-from-string room-report t :eof :start begin-free  :end end-free))))

(defun lisp/block-promotion ()
  (setq lw::*block-promotion* t))


