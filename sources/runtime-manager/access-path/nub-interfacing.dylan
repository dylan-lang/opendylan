module:        access-path-implementation
synopsis:      FFI declarations to allow access-path to call the debugger
               nub on demand
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro proxy-C-function-definer
  { define proxy-C-function ?dylan-name:name ?specs end }
    => { define C-function "proxy-" ## ?dylan-name ?specs end }
specs:
  { } => { }
  { ?spec:* ; ...} => { ?spec ; ...}
end macro;

define macro debugger-nub-interface-definer
  { define debugger-nub-interface ?dylan-name:name ?specs end }
    => { define C-function ?dylan-name ?specs end;
         define proxy-C-function ?dylan-name ?specs end }
specs:
  { } => { }
  { ?spec:* ; ...} => { ?spec ; ...}
end macro;
