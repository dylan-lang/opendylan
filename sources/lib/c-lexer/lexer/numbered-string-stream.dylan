Module: C-lexer-internal
Author: Toby Weinberg
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// So that current-line can be called when lexing from a string stream.

define class <numbered-string-stream> (<string-stream>)
  slot current-line :: <integer>, required-init-keyword: current-line: ;
end class;
