(in-package :cl-user)

(defun write-100mb ()
  (let ((string (make-string 100 :initial-element #\x)))
    (with-open-file (stream "/tmp/100mb.sbcl.txt" :direction :output :if-exists :supersede)
      (loop repeat (* 1024 1024)
         do (write-string string stream)))))

(write-100mb)
