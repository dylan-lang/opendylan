Module:    Dylan-User
Synopsis:  Tests utility functions in the Win32-common library.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library win32-util-test
  use functional-dylan;
  use Win32-common; 
  use C-FFI;
  use testworks;
end library;

define module win32-util-test
  use functional-dylan;
  use simple-format;
  use machine-words;
  use Dylan-extensions,
    import: { <abstract-integer>, <big-integer>, <double-integer> };
  use Win32-common;
  use C-FFI;
  use testworks;
end module;
