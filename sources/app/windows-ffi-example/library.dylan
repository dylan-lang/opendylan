Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//  This library is an application program which serves as a simple
//  example of a Windows program implemented in Dylan using the low-level
//  interface libraries to the Win32 API.

define library windows-ffi-example
  use Dylan;
  use Win32-common;
  use Win32-user;
  use Win32-GDI;
  use Win32-kernel;
end;

define module windows-ffi-example
  use Dylan;
  use Win32-common;
  use Win32-user;
  use Win32-GDI;
  use Win32-kernel;
end module;
