(in-package :dylan)



;;;; HARP defines some macros which end up creating reserved words in 
;;;; the emulator. This code removes them.

(defvar *reserved-words-to-lose*
    '(pattern options))

(defun lose-reserved-words ()
  (loop for word in *reserved-words-to-lose*
        do (remhash word *dylan-infix-syntax-table*)))


;;;; HARP also defines macros which the emulator places in a global namespace.
;;;; These clash with functions in other libraries. This code removes them.


(defvar *macros-to-lose*
    '(emit))

(defun lose-macros ()
  (loop for word in *macros-to-lose*
        do (remhash word *dylan-translators*)))


;; (lose-reserved-words)
(lose-macros)
