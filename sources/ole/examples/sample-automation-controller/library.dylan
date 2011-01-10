Module:    Dylan-User
Synopsis:  Demonstration of a simple OLE Automation controller application.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library sample-automation-controller
  use functional-dylan;
  use OLE-Automation;
  use Win32-common;
  use Win32-GDI;
  use Win32-kernel;
  use Win32-user;
  use Win32-Dialog; // for ChooseColor
end;

define module sample-automation-controller
  use functional-dylan;
  use OLE-Automation;
  use Win32-common;
  use Win32-GDI;
  use Win32-kernel;
  use Win32-user;
  use Win32-Dialog; // for ChooseColor
end;
