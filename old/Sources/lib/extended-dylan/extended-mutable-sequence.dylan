module:    extended-library
language:  prefix-dylan
author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-method quick-sort! ((sequence <mutable-sequence>) #key (test <))
  (bind ((size-1 (- (size sequence) 1)))
    (bind-methods
      ((buddy-sort! ((l <integer>) (r <integer>))
	 (when (> r l)
	   (bind (((v <object>) (element sequence r))
		  ((i <integer>) (- l 1))
		  ((j <integer>) r)
		  ((t <object>) 0))
	     (iterate grovel ()
	       (iterate up ()
		 (set! i (+ i 1))
		 (when (and (< i size-1) (test (element sequence i) v)) (up)))
	       (iterate down ()
		 (set! j (- j 1))
		 (when (and (/= j 0) (not (test (element sequence j) v)))
		   (down)))
	       (set! t (element sequence i))
	       (set! (element sequence i) (element sequence j))
	       (set! (element sequence j) t)
	       (when (> j i) (grovel)))
	     (set! (element sequence j) (element sequence i))
	     (set! (element sequence i) (element sequence r))
	     (set! (element sequence r) t)
	     (buddy-sort! l (- i 1))
	     (buddy-sort! (+ i 1) r)))))
      (buddy-sort! 0 size-1)))
  sequence)

