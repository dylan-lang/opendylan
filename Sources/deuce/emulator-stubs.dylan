Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Stubs for Deuce in the emulator

define macro without-bounds-checks
  { without-bounds-checks ?:body end }
    => { begin ?body end }
end macro without-bounds-checks;

define constant element-no-bounds-check = element;
define constant element-no-bounds-check-setter = element-setter;

define function element-range-error
    (collection :: <collection>, key) => ()
  error(make(<simple-error>,
             format-string: "ELEMENT outside of range: %=",
             format-arguments: list(key)))
end function element-range-error;
