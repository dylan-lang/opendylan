Module:    Dylan-User
Synopsis:  Preliminary experiment for using "mini-duim" as an OLE server part.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library DUIM-OLE-server
  use Dylan;
  use OLE;
  use OLE-std-util;
  use simple-format;
  use Win32-common;
  use Win32-GDI;
  use Win32-kernel;
  use Win32-user;
  use mini-duim;
  use functional-extensions;

  export DUIM-OLE-server;
end;

// Use this module name internally just to avoid having to edit the
// "Module:" heading on all of the source files.
define module OLE-Server-Framework
  use Dylan;
  use OLE,
    export: { make-GUID, <REFGUID>, <REFCLSID>,
	     // Export names that may be needed by application methods on 
	     // save-frame-to-storage and load-frame-from-storage.
	     <Interface>, <LPSTREAM>, <HRESULT>, IStream/Read-integer,
	     IStream/Write-integer, IStream/Read, IStream/Write,
	     IStream/Seek, SUCCEEDED?, FAILED? };
  use OLE-std-util;
  use simple-format;
  use Win32-common;
  use Win32-GDI;
  use Win32-kernel;  // need OutputDebugString, <PFILETIME>, GetLastError,
		     //	     GlobalUnlock, GlobalLock, GlobalAlloc, $GHND
  use Win32-user;
  use mini-duim, prefix: "duim/";

  export start-ole-server, save-frame-to-storage, load-frame-from-storage;
  export IStream/Write-int16, IStream/Read-int16,
    istream-read-integer, istream-read-int16,
    istream-write-integer, istream-write-int16;
  export <ole-server-frame>, ole-data-changed;
end;

define module DUIM-OLE-server
  use OLE-Server-Framework, export: all;
end;
