Module:    Dylan-User
Synopsis:  This library is a complete application program which provides a
	   simple demonstration of using the low-level OLE API for
	   creating a window in which OLE objects can be embedded.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library sample-OLE-container
  use dylan;
  use common-dylan;
  use OLE;
  use OLE-Dialogs;
  use Win32-common;
  use Win32-GDI;
  use Win32-kernel;
  use Win32-user;
  use C-FFI;
end;

define module sample-OLE-container
  use dylan;
  use common-dylan;
  use OLE;
  use OLE-Dialogs;
  use Win32-common;
  use Win32-GDI;
  use Win32-kernel;
  use Win32-user;
  use C-FFI;
end;
