Module:    Dylan-User
Synopsis:  Library for using DUIM as an OLE Control.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library DUIM-OLE-Control
  use common-dylan;
  use Win32-common;
  use Win32-User;
  use Win32-GDI;
  use DUIM;
  use Win32-DUIM;
  use DUIM-extended-geometry;
  use DUIM-OLE-Server;
  use OLE-Control-Framework;

  export DUIM-OLE-Control;
end;

define module DUIM-OLE-Control
  use Dylan;
  use common-extensions, import: { false-or, $unsupplied, ignore };
  use Win32-common;
  use Win32-User, prefix: "w/";
  use Win32-GDI, prefix: "w/";
  use DUIM, prefix: "duim/";
  use DUIM-internals, prefix: "duim/";
  use Win32-DUIM, prefix: "duim/";
  use DUIM-extended-geometry, prefix: "duim/"; // for make-transform method
  use DUIM-OLE-Server, export: all;
  use DUIM-OLE-server-internals;
  use OLE-Control-Framework,
    // Temporary hack to avoid name conflict; need better long-term solution???
    rename: { \initialize-ole-control => \initialize-simple-ole-control },
    export: all;
  use Win32-default-handler; // for *error-module-handle*

  // documented classes:
  export <ocx-frame>, <ocx-dispatch>;

  // functions the user can call:
  export ocx-frame, \initialize-ole-control;

  // functions for which the user can add methods:
  export max-storage-size;

  // internal implementation exported to allow low-level customization:
  export <DUIM-OCX>;

end;
