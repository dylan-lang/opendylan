Module:    Dylan-User
Synopsis:  This library is a complete application program which provides a
	   very simple demonstration of using the low-level OLE API for
	   creating a graphic object that can be embedded in compound
	   documents.  
	   This program was translated from a C++ program in the Microsoft
	   Win32 SDK, in directory "\MSTOOLS\samples\ole\simpsvr", where it
	   is described as:
		This sample is the simplest OLE 2.0 object that can be
		written and still support the visual editing feature.  The
		object that this server supports is a colored square with a
		black border.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library sample-OLE-server
  use functional-dylan;
  use C-FFI;
  use OLE;
  use Win32-common;
  use Win32-GDI;
  use Win32-kernel;
  use Win32-user;
end;

define module sample-OLE-server
  use functional-dylan;
  use simple-format;
  use C-FFI;
  use OLE;
  use Win32-common;
  use Win32-GDI;
  use Win32-kernel;
  use Win32-user;
end;
