Module:    Dylan-User
Synopsis:  Demonstration of a simple OLE Automation controller application.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library select-viewer-controller
  use Functional-Dylan;
  use OLE-Automation;
  use Duim;
/*
  use Win32-common;
  use Win32-kernel;
  use Win32-user;
*/
  use Format;
end;

define module select-viewer-controller
  use Functional-Dylan;
  use Threads;
  use OLE-Automation, rename: { Release => COM/Release };
  use Duim;
/*
  use Win32-common;
  use Win32-GDI;
  use Win32-kernel;
  use Win32-user;
  use Win32-Dialog; // for ChooseColor
*/
  use Format;
end;
