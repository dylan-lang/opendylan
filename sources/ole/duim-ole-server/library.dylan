Module:    Dylan-User
Synopsis:  Library for using "DUIM" as an OLE server part.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library DUIM-OLE-server
  use functional-dylan;
  use Win32-common;
  use Win32-kernel;
  use Win32-user;
  use DUIM;
  use Win32-DUIM;
  use OLE-Server;

  export DUIM-OLE-server, DUIM-OLE-server-internals;
end;

// The following module is for use by the `DUIM-OLE-control' library:
define module DUIM-OLE-server-internals
  create ole-frame-class-id, ole-frame-class-id-setter, ole-frame-usage,
    frame-registration-parameters,
    <DUIM-server-doc>, doc-sheet, doc-window, get-doc, get-doc-setter,
    ole-frame-doc-obj, ole-frame-doc-obj-setter,
    ole-frame-object-title, ole-frame-object-title-setter;
  create invent-prog-id;
  create <ole-main-sheet>;
  create *duim-user-count*;
end;

define module DUIM-OLE-server
  use functional-dylan;
  use Win32-common;  // $NULL-HWND, <HDC>, <HMENU>, <HWND>, etc.
  use Win32-kernel;  // need OutputDebugString, <PFILETIME>, GetLastError,
		     //	     GlobalUnlock, GlobalLock, GlobalAlloc, $GHND
  use Win32-user; // uses <LPMSG>, DestroyMenu, DispatchMessage, GetMessage,
		  // InsertMenu, PostMessage, TranslateMessage, etc.
  use DUIM, prefix: "duim/";
  use DUIM-internals, prefix: "duim/";
  use Win32-DUIM, prefix: "duim/";
  use OLE-Server, export: all;
  use DUIM-OLE-server-internals;

  // documented classes:
  export <embeddable-frame>;

  // functions the user can call:
  export note-embedded-data-changed, note-embedded-image-changed,
    embedded-data-changed?, embedded-data-changed?-setter;
  export frame-in-place-active?, frame-embedded-size, frame-container-name;

  // functions for which the user can add methods:
  export save-frame-to-storage, load-frame-from-storage, frame-init-new-object;
  export frame-container-menus;
  export note-in-place-activation, note-in-place-deactivation;
  export frame-embedded-sheet, frame-embedded-size-requested;

  // internal implementation exported to allow low-level customization:
  export <DUIM-server-app>, <DUIM-OLE-Server>, <basic-DUIM-OLE-server>;
  export frame-ole-object-class, frame-ole-object, app-frame, app-frame-setter;
  export ole-event-loop;
  export frame-register-server, frame-unregister-server;

end;

