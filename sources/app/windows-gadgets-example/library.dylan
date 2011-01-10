Module:    dylan-user
Synopsis:  An example demonstrating the use of Win32 gadgets
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//  This library is an application program which serves as a simple
//  example of a Windows program implemented in Dylan using the low-level
//  interface libraries to the Win32 API.

define library windows-gadgets-example
  use Dylan;
  use Win32-common;
  use Win32-user;
  use Win32-GDI;
  use Win32-kernel;
  use Win32-dialog;
end library;

define module windows-gadgets-example
  use Dylan;
  use Win32-common;
  use Win32-user;
  use Win32-GDI;
  use Win32-kernel;
  use Win32-dialog;
end module;
