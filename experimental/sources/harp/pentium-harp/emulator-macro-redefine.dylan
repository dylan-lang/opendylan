module:    base-harp
Synopsis:  Fixups for HARP macros which must be undefined after loading this library
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// This file will reverse the effect of file emulator-parser-fixup.lisp
//// which is called after loading each backend, and deletes some of the macros



define macro emit
  { emit(?backend:expression, ?emissions:*) }
    => { begin
           let $backend$ = ?backend;
           ?emissions
         end }

emissions:
  { } => { }
  { ?word:expression, ... } => { emit-1($backend$, ?word); ... }

end macro;

