Module:    Dylan-User
Synopsis:  Example of a simple OLE server application using the
	   OLE-Server library and Win32 API.  See the README file.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library win32-ole-server
  use functional-dylan;
  use Win32-common;
  use Win32-kernel;
  use Win32-GDI;
  use Win32-user;
  use C-FFI;
  use OLE-Server;

  export sample-OLE-server;
end library;

define module sample-OLE-server
  use functional-dylan;
  use Win32-common;
  use Win32-kernel;
  use Win32-GDI;
  use Win32-user;
  use C-FFI;
  use OLE-Server;
end module;
