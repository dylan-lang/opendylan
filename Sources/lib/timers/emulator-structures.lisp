; copyright: 1996 Functional Objects, Inc. All rights reserved.
(in-package 'ccl)

(defstruct cpu-times
  (user-secs      0 :type fixnum)
  (user-micros    0 :type fixnum)
  (system-secs    0 :type fixnum)
  (system-micros  0 :type fixnum)
)

(defun get-cpu-times-user-secs(tm)     (cpu-times-user-secs     tm))
(defun get-cpu-times-user-micros(tm)   (cpu-times-user-micros   tm))
(defun get-cpu-times-system-secs(tm)   (cpu-times-system-secs   tm))
(defun get-cpu-times-system-micros(tm) (cpu-times-system-micros tm))
