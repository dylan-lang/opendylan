;; Filename: build-prolog.lisp
;;   Author: keith, rthor, amit
;; Synopsis: This file loads the lisp prolog shared by all
;;           images. It also loads patches using dodds
;;           "smart" patch loader.
;; Revision: By: amit. On: Dec05 1996. Rea: Add functions
;;           and documentation stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package cl-user)

;; See redefinition below.
;; dbg:bug-backtrace not available until after patches are loaded
(defun handle-build-error (condition)
  (format t "Caught build error: ~a~%" condition)
  (bye 1))

(defmacro with-build-handler (&body body)
  `(handler-bind ((error #'handle-build-error))
     ,@body))

(setq system::*complain-about-init-file-loaded* nil)
(extend-current-stack 600)

(with-build-handler
 (let ((*packages-for-warn-on-redefinition* nil))
   (format t "~&Loading Dodds' patch tools...~%")
   (SCM:LOAD-PATCH-SEQUENCE 'SYS::PATCH-ITSELF)))
 
(with-build-handler
 ;; these limits lifted from webmaker image-build.lisp
 (format t "~&declaring patch limits")
 (scm:declare-patch-sequence-limit 'sys::patch-itself 2 27)
 (scm:declare-patch-sequence-limit 'sys::patch-itself-full 2 37) ;35
 (scm:declare-patch-sequence-limit 'sys::system 1 169) ;122
 (scm:declare-patch-sequence-limit 'sys::clos 1 21) ;15
 (scm:declare-patch-sequence-limit :clx 1 10)
 (scm:declare-patch-sequence-limit 'sys::printer 1 7)
 (scm:declare-patch-sequence-limit 'sys::link-load 1 30) ;28
 (scm:declare-patch-sequence-limit 'sys::numbers 1 10) ;8
 (scm:declare-patch-sequence-limit 'sys::ffi 1 17) ;15
 (scm:declare-patch-sequence-limit 'sys::compiler 1 64)
 (scm:declare-patch-sequence-limit 'sys::mp 1 7) ;6
 (scm:declare-patch-sequence-limit :tools 1 9)
 (scm:declare-patch-sequence-limit :toolkit 1 22) ;10
 (scm:declare-patch-sequence-limit :comms 1 7) ;4
 (scm:declare-patch-sequence-limit :clue-color 1 5) ;4
 (scm:declare-patch-sequence-limit :clue-graphics-ports 1 13) ;9
 (scm:declare-patch-sequence-limit :clue 1 12) ;9
 (scm:declare-patch-sequence-limit :capi 1 53) ;25
 (scm:declare-patch-sequence-limit :di-graphics-ports 1 3)
 (scm:declare-patch-sequence-limit :delivery 1 25) ;13
 (scm:declare-patch-sequence-limit :compound-array-types 1 1)
 (scm:declare-patch-sequence-limit :slider 1 2) ;0
 (scm:declare-patch-sequence-limit :full-editor 1 6) 
 (scm:declare-patch-sequence-limit :ffi-inspect 1 0) 
 (scm:declare-patch-sequence-limit :wdebug 1 1) 
 (scm:declare-patch-sequence-limit :window-inspector 1 1) 
 (scm:declare-patch-sequence-limit :defsystem 1 5) 

 ;; ...not limited...
 ;; FORMATTER 1.0
 ;; FFI-INSPECT 1.0
 ;; C++ 1.0
 ;; SQL 1.0
 ;; SYSTEM-GRAPH 1.0
 
 ;; Load all patches now
 (SCM:LOAD-PATCHES :LIMIT :DECLARED-ONLY))

;; with-debugger-stack isn't available until after patch loading.
(defun handle-build-error (condition)
  (format t "Caught build error: ~a~%" condition)
  (dbg:with-debugger-stack ()
    (dbg:bug-backtrace nil))
  (bye 1))
                          
;; eof
