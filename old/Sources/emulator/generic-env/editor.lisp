(in-package :editor)

;;   These result in a form being planted into the listener's input to be 
;;   trapped by the evaluator.

(defun language-listener-load ()
  (listener-load-or-compile :load-request
                            "File To Load : "
                            "Loading ~s"))

(defun language-listener-compile ()
  (listener-load-or-compile :compile-request
                            "File To Compile : "
                            "Compiling ~a"))

(defun language-listener-compile-and-load ()
  (listener-load-or-compile :compile-load-request
                            "File To Compile and Load : "
                            "Compiling and Loading ~a"))

(defun language-listener-inspect-star ()
  (conditions::un-bind-handler error
    (system::cond-throw-to-tag 'system::top-level-read (list :inspect-request *) t)
    (error "Could't throw to SYSTEM::TOP-LEVEL-READ")))

;; eof
