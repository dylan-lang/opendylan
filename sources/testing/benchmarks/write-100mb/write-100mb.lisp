(in-package :cl-user)

(defun write-10000mb ()
  ;; (declare (optimize (speed 3) (debug 0)))
  (let ((string (make-string 100 :initial-element #\x)))
    (loop repeat 100
          do (with-open-file (stream "/tmp/100mb.sbcl.txt" :direction :output :if-exists :supersede)
               (loop repeat (* 1024 1024)
                     do (write-string string stream))))))

;;; sbcl --noinform --no-sysinit --no-userinit --eval '(load "write-100mb.lisp")' --eval '(sb-ext:save-lisp-and-die #P"/tmp/clwrite100mb" :executable t :toplevel 'cl-user::write-10000mb)'
