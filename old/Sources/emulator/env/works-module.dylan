Module:    dylan-user
Language:  prefix-dylan
Synopsis:  A default working module importing useful things
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-module dylanworks
  (use dylan
    export: all)
  (use stream
    export: all)
  (use ratio
    export: all)
  (use complex
    export: all)
  ;; AndrewA, June 13 1996 - use SWM's transcendentals library instead
  ;;  (use transcendentals
  ;;    export: all)
  (use mop
    export: all)
  (use internal
    import: (unsupplied unsupplied? supplied?
             unfound unfound? found?)
    export: all)
  ;; Because hygiene's broken
  (use infix-reader
    export: (register-defining-macro
             register-statement-0-macro
             register-statement-1-macro
             register-statement-2-macro))

  )

(define-module dylanworks-user
  (use dylanworks))

;; eof
