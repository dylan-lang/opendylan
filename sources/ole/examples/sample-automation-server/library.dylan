Module:    Dylan-User
Synopsis:  Demonstration of a simple OLE Automation server application.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library sample-automation-server
  use functional-dylan;
  use OLE-Automation;
  use Win32-common;
  use Win32-GDI;
  use Win32-kernel;
  use Win32-user;
end;

define module sample-automation-server
  use functional-dylan;
  use OLE-Automation;
  use Win32-common;
  use Win32-GDI;
  use Win32-kernel;
  use Win32-user;
end;
