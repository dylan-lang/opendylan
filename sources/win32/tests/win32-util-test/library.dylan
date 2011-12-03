Module:    Dylan-User
Synopsis:  Tests utility functions in the Win32-common library.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library win32-util-test
  use common-dylan;
  use Win32-common; 
  use C-FFI;
  use testworks;
end library;

define module win32-util-test
  use common-dylan;
  use simple-format;
  use machine-words;
  use Dylan-extensions,
    import: { <abstract-integer>, <big-integer>, <double-integer> };
  use Win32-common;
  use C-FFI;
  use testworks;
end module;
