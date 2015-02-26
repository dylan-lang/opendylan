Module:    Dylan-User
Synopsis:  Utility library for creating OLE container applications 
	   using the Win32 API for the user interface.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library OLE-Container
  use common-dylan;
  use OLE;
  use OLE-Dialogs;
  use Win32-common;
  use Win32-GDI;
  use Win32-kernel;
  use Win32-user;
  use C-FFI;
  export OLE-Container;
end;

define module OLE-Container
  use common-dylan;
  use simple-format;
  use OLE,
    export: {$OLEIVERB-PRIMARY, $OLEIVERB-SHOW, $OLEIVERB-OPEN,
	     $OLEIVERB-HIDE, $OLEIVERB-UIACTIVATE,
	     $OLEIVERB-INPLACEACTIVATE, $OLEIVERB-DISCARDUNDOSTATE,
	     COM-interface-definer, AddRef, Release, terminate,
	     <HRESULT>, <SCODE>, $S-OK, $S-TRUNCATED, $E-FAIL,
	     SUCCEEDED?, FAILED?,
	     <ole-error>, ole-error-status, ole-error-instance,
	     ole-error-context, ole-error-args,
	     <REFGUID>, make-GUID, <REFCLSID>,
	     $DVASPECT-CONTENT, $DVASPECT-THUMBNAIL, $DVASPECT-ICON,
	     $DVASPECT-DOCPRINT,
	     $OLERENDER-NONE, $OLERENDER-DRAW, $OLERENDER-FORMAT,
	     $OLERENDER-ASIS,
	     
	     <LPFORMATETC>, cfFormat-value, cfFormat-value-setter,
	     ptd-value, ptd-value-setter,
	     dwAspect-value, dwAspect-value-setter, lindex-value,
	     lindex-value-setter, tymed-value, tymed-value-setter,
	     
	     $USERCLASSTYPE-FULL, $USERCLASSTYPE-SHORT,
	     $USERCLASSTYPE-APPNAME  
	       };
  use OLE-Dialogs, // using OleUIInsertObject and OleUIAddVerbMenu
    export: { $IOF-SHOWHELP, $IOF-SELECTCREATENEW, $IOF-SELECTCREATEFROMFILE,
	     $IOF-CHECKLINK, $IOF-CHECKDISPLAYASICON, $IOF-CREATENEWOBJECT,
	     $IOF-CREATEFILEOBJECT, $IOF-CREATELINKOBJECT, $IOF-DISABLELINK,
	     $IOF-VERIFYSERVERSEXIST, $IOF-DISABLEDISPLAYASICON,
	     $IOF-HIDECHANGEICON, $IOF-SHOWINSERTCONTROL,
	     $IOF-SELECTCREATECONTROL };
  use Win32-common;
  use Win32-GDI;
  use Win32-kernel;
  use Win32-user;
  use C-FFI;

  // classes that the application can reference:
  export <contained-object>;

  // classes that the application should subclass:
  export <container-app>;

  // functions for the application to call:
  export container-do-verb, container-add-verbs, 
    container-remove-verbs, container-UI-deactivate;
  export container-palette-changed, container-activate-application;
  export container-size-changed;
  export insert-object-from-dialog, insert-object-by-class,
    insert-object-from-file;
  export container-context-help, container-handle-accelerators;
  export close-doc-object, container-destroy-documents;
  export paint-contained-document, container-query-new-palette;
  export document-edges, document-rectangle;
  export document-move;
  export get-server-name;

  // functions that the application should add a method to:
  export container-host-names, container-insert-menus,
    container-set-border-space;

  // functions the application can optionally add a method to:
  export container-request-border-space;
  export container-release-border-space, container-add-frame-ui;
  export container-main-menu;
  export container-document-class;
  export container-set-status-text;
  export note-data-change, note-view-change,
    note-document-save, note-document-close, note-active-object-name,
    note-enable-modeless;

  // accessors for application to call:
  export document-application, document-container-window;
  export document-in-place-active?, document-open-out-of-place?;
  export container-frame-window, container-frame-window-setter;
  export document-class-id, document-file-name;

  // accessors not used by the initial application, but might be needed later:
  export document-sub-storage, document-ui-active-window, document-client-site,
    document-advise-sink, document-in-place-site,
    document-draw-aspect, document-olemisc;
  export in-place-active-object, active-object-name, active-object-window;
  export container-accelerator-table, container-accelerator-table-setter;
  export containing-document-name, document-ole-object,
    document-in-place-ole-object;

  // the following should not be documented, but are being exported
  // for the sake of extensibility.
  export create-doc-object, init-object-in-site, abort-doc;
  export himetric-to-pixels, pixels-to-himetric;
end;
