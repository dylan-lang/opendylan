Module:    Dylan-User
Synopsis:  Simple OLE Automation server interface for KeithD's select viewer.
Author:    Hugh Greene, David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library select-viewer-server
  use Functional-Dylan;
  use Threads;
  use OLE-Automation;
  use SQL-ODBC;
  use Win32-common;
//  use Win32-GDI;
  use Win32-kernel;
  use Win32-user;
end;

define module select-viewer-server
  use Functional-Dylan;
  use Simple-Format;
  use Threads, exclude: { release };
  use OLE-Automation;
  use SQL-ODBC, exclude: { text };
  use Win32-common;
//  use Win32-GDI;
  use Win32-kernel, exclude: { sleep };
  use Win32-user;
end;
