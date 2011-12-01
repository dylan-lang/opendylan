Module:    Dylan-User
Synopsis:  Utility library for creating OLE container applications 
	   using DUIM for the user interface.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library DUIM-OLE-Container
  use common-dylan;
  use OLE-Container;
  use C-FFI;
  use Win32-common;
  use Win32-GDI;
  use Win32-kernel;
  use Win32-user;
  use DUIM;
  use Win32-DUIM;

  export DUIM-OLE-Container;
end library;

define module DUIM-OLE-Container
  use common-dylan;
  use OLE-Container,
    export: { document-ole-object, document-class-id, document-file-name };
  use C-FFI;
  use Win32-common, prefix: "w/";
  use Win32-GDI, prefix: "w/";
  use Win32-kernel, prefix: "w/";
  use Win32-user, prefix: "w/";
  use DUIM, prefix: "duim/";
  use DUIM-internals, prefix: "duim/";
  use Win32-DUIM, prefix: "duim/";

  export <ole-gadget>;
  export <container-frame-mixin>;
  export <container-sheet-mixin>;

  // for users to call:
  export insert-from-dialog, insert-by-class, insert-from-file;
  export gadget-ole-class-id, gadget-ole-file-name;

  // for user to add a method:
  export frame-active-container-menus;

  // for extensibility, hooks to lower level libraries;
  export frame-container-app, sheet-contained-object;
  export <duim-contained-object>;
  export object-sheet;
  export <duim-container-app>, app-frame;

end module;
