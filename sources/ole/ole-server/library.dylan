Module:    Dylan-User
Synopsis:  High-level support for OLE servers.
	   See "~dylan/doc/lib/ole-utilities.text" for user documentation.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library OLE-Server
  use common-dylan;
  use OLE;
  use Win32-common;
  use Win32-GDI;
  use Win32-kernel;
  use Win32-user;
  use Win32-registry;
  use C-FFI;

  export OLE-Server;
end;

define module OLE-Server
  use Dylan;
  use common-extensions,
    import: { \assert, false-or, \function-definer, ignore };
  use OLE,
    export: { make-GUID, <REFGUID>, <REFCLSID>,
	     // Export names that may be needed by application methods on 
	     // save-frame-to-storage and load-frame-from-storage.
	     <Interface>, <LPSTREAM>, <storage-istream>,
	     <HRESULT>, IStream/Read-integer,
	     IStream/Write-integer, IStream/Read, IStream/Write,
	     IStream/Seek, $null-istream, $STREAM-SEEK-SET,
	     SUCCEEDED?, FAILED?, $S-OK,
	     <LPSTORAGE>,
	     // class factory support:
	     <class-factory>, revoke-registration, with-class-factory,
	     $REGCLS-SINGLEUSE, $REGCLS-MULTIPLEUSE, $REGCLS-MULTI-SEPARATE,
	     $CLSCTX-INPROC-SERVER, $CLSCTX-INPROC-HANDLER,
	     $CLSCTX-LOCAL-SERVER,
	     // other utilities:
	     <lib-init-mixin>, 
	     <ole-error>, ole-error, check-ole-status, ole-error-status,
	     ole-error-context, ole-error-instance, ole-error-args, 
	     OLE-util-in-process-startup?,
	     OLE-util-started-by-OLE?, OLE-util-register-only?,
	     OLE-util-unregister?, OLE-util-file-arg,
	     // documented error codes from OLE-util-set-status-text:
	     $S-TRUNCATED, $E-FAIL, $OLE-E-NOT-INPLACEACTIVE, 
	     // have to export this so methods can be added:
	     terminate,
	     // other miscellaneous things that might be useful without
	     // wanting to use the whole OLE library:
	     $NULL-interface, <IUnknown>, AddRef, Release, null?,
	     \COM-interface-definer, <LPOLESTR>, OLESTR, $NULL-OLESTR,
	     <ffi-integer>,
	     $E-UNEXPECTED, $E-NOTIMPL, $E-INVALIDARG, $E-POINTER,
	     $E-HANDLE, $E-ABORT, $E-ACCESSDENIED, $E-NOINTERFACE,
	     // internal debug aid:
	     Output-Debug-String
	     };
  use simple-format; // need format-to-string
  use Win32-common, export: { pointer-cast };
  use Win32-kernel;  // need OutputDebugString, <PFILETIME>, GetLastError
  use Win32-user;
  use Win32-GDI; // for metafile manipulation
  use Win32-registry;
  use C-FFI; // need C types for storage read/write utility functions

  // low-level utilities:
  export pixels-to-himetric, himetric-to-pixels, ole-util-init,
    create-hatch-window, set-hatch-window-size;

  export \debug-out, Output-Debug-Boolean;

  export <basic-ole-server>;
  export <ole-server-framework>, <CDataObject>, <COleInPlaceActiveObject>,
    <COleInPlaceObject>, <COleObject>, <CPersistStorage>,
    <CExternalConnection>; 
  export Release-Streams-And-Storage; // to add method in ole-control-framework
  export <ole-in-process-server>, query-cache-interface;

  export \returning-error-status;

  export OLE-util-Create-Stream, OLE-util-open-stream,
    OLE-util-set-status-text,
    OLE-util-in-place-active?, OLE-util-UI-active?,
    OLE-util-part-hidden,
    OLE-util-data-changed, OLE-util-view-changed,
    OLE-util-translate-accelerator, OLE-util-close-server,
    OLE-util-container-name, OLE-util-current-size,
    OLE-util-HIMETRIC-size;
  export OLE-util-send-view-change, OLE-util-defer-view-change,
    OLE-util-flush-view-change;
 
  export in-place-frame-info, container-IOleInPlaceFrame,
    server-IOleObject, server-IOleObject-setter,
    container-parent-window, get-hatch-window, obj-class-ID;
  export container-IOleClientSite, container-IOleAdviseHolder,
    container-IDataAdviseHolder, container-IOleInPlaceUIWindow,
    container-IOleInPlaceSite;
  export copy-class-ID;

  export register-ole-server, unregister-ole-server,
    OLE-util-maybe-just-register;
  export \initialize-ole-server;

  export OLE-part-formats-for-get, OLE-part-formats-for-set, <FORMATETC-info>;

  export IStream/Write-int16, IStream/Read-int16,
    istream-read-integer, istream-read-int16,
    istream-write-integer, istream-write-int16, istream-rewrite,
    istream-write-float, istream-read-float, 
    istream-write-string, istream-read-string;

  // Generic functions to be implemented by the application-specific code:
  export OLE-part-Create-Streams, OLE-part-dirty?, OLE-part-dirty?-setter,
    OLE-part-toolbar-window,
    OLE-part-init-new, OLE-part-draw,
    OLE-part-doc-window, OLE-part-draw-metafile, OLE-part-show-window,
    OLE-part-requested-size, OLE-part-change-size,
    OLE-part-get-data, OLE-part-set-data, OLE-part-hide, 
    OLE-part-in-place-activated, OLE-part-in-place-deactivated,
    OLE-part-UI-activated, OLE-part-UI-deactivated,
    OLE-part-insert-menus, OLE-part-release-menu, OLE-part-accelerators,
    OLE-part-Load-From-Storage, OLE-part-open-out, OLE-part-position-window,
    OLE-part-Open-Streams, OLE-part-Release-Streams, OLE-part-Save-To-Storage,
    OLE-part-set-focus, OLE-part-title, OLE-part-command-window,
    OLE-part-enable-dialog;
  
  // overridden for OLE Controls:
  export hatch-when-UI-active?, after-initialize;
 end;
