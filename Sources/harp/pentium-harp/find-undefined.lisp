(in-package "USER")

;;; Find all the pentium instructions which do not have valid code generator
;;; templates.

(defun code-gen-fn (op)
  (dylan+dylan/harp-op::op-code-gen-fn op))

(defun op-name (op)
  (dylan+dylan/harp-op::op-name op))

(defun find-undefined ()
  (let* ((insts dylan+dylan/pentium-harp::pentium-instructions)
         (undef dylan+dylan/harp-op::no-template)
         (ops (clos::standard-instance-static-slots insts))
         (undef-ops '()))
    (loop for op across ops
          do (let ((cgfn (code-gen-fn op)))
               (when (eq cgfn undef)
                 (push op undef-ops))))
    (values undef-ops (length ops) (length undef-ops))))

(defun list-ops ()
  (let* ((insts dylan+dylan/pentium-harp::pentium-instructions)
         (ops (clos::standard-instance-static-slots insts)))
    (loop for op across ops
          collect op)))

