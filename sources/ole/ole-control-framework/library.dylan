Module:    Dylan-User
Synopsis:  High-level support for OLE controls.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library OLE-Control-Framework
  use functional-dylan;
  use COM;
  use OLE;
  use OLE-Server;
  use OLE-Automation;
  use OLE-Controls;
  use Win32-common;
  use Win32-GDI;
  use Win32-user;
  use Win32-registry;
  use Win32-kernel;
  use C-FFI;

  export OLE-Control-Framework;
end library;

define module OLE-Control-Framework
  use functional-dylan;
  use OLE,
    export: {$OLEMISC-RECOMPOSEONRESIZE, $OLEMISC-ONLYICONIC,
	     $OLEMISC-INSERTNOTREPLACE, $OLEMISC-STATIC,
	     $OLEMISC-CANTLINKINSIDE, $OLEMISC-CANLINKBYOLE1,
	     $OLEMISC-ISLINKOBJECT, $OLEMISC-INSIDEOUT,
	     $OLEMISC-ACTIVATEWHENVISIBLE,
	     $OLEMISC-RENDERINGISDEVICEINDEPENDENT};
  use OLE-Server, export: all;
  use OLE-Automation, export: all;
  use OLE-Controls,
    export: { $OLEIVERB-PROPERTIES,

	     $OLEMISC-INVISIBLEATRUNTIME, $OLEMISC-ALWAYSRUN,
	     $OLEMISC-ACTSLIKEBUTTON, $OLEMISC-ACTSLIKELABEL,
	     $OLEMISC-NOUIACTIVATE, $OLEMISC-ALIGNABLE, $OLEMISC-SIMPLEFRAME,
	     $OLEMISC-SETCLIENTSITEFIRST, $OLEMISC-IMEMODE,
	     $OLEMISC-IGNOREACTIVATEWHENVISIBLE, $OLEMISC-NOTMINIMUMNOTIFY,
	     $OLEMISC-SUPPORTSMULTILEVELUNDO, $OLEMISC-WANTSTOMENUMERGE,

	     $DISPID-AUTOSIZE, $DISPID-BACKCOLOR, $DISPID-BACKSTYLE,
	     $DISPID-BORDERCOLOR, $DISPID-BORDERSTYLE, $DISPID-BORDERWIDTH,
	     $DISPID-DRAWMODE, $DISPID-DRAWSTYLE, $DISPID-DRAWWIDTH,
	     $DISPID-FILLCOLOR, $DISPID-FILLSTYLE, $DISPID-FONT,
	     $DISPID-FORECOLOR, $DISPID-ENABLED, $DISPID-HWND,
	     $DISPID-TABSTOP, $DISPID-TEXT, $DISPID-CAPTION,
	     $DISPID-BORDERVISIBLE, $DISPID-APPEARANCE, $DISPID-READYSTATE,
	     $DISPID-REFRESH, $DISPID-DOCLICK, $DISPID-ABOUTBOX,
	     $DISPID-CLICK, $DISPID-DBLCLICK, $DISPID-KEYDOWN,
	     $DISPID-KEYPRESS, $DISPID-KEYUP, $DISPID-MOUSEDOWN,
	     $DISPID-MOUSEMOVE, $DISPID-MOUSEUP, $DISPID-ERROREVENT,
	     $DISPID-READYSTATECHANGE,

	     $DISPID-AMBIENT-BACKCOLOR, $DISPID-AMBIENT-DISPLAYNAME,
	     $DISPID-AMBIENT-FONT, $DISPID-AMBIENT-FORECOLOR,
	     $DISPID-AMBIENT-LOCALEID, $DISPID-AMBIENT-MESSAGEREFLECT,
	     $DISPID-AMBIENT-SCALEUNITS, $DISPID-AMBIENT-TEXTALIGN,
	     $DISPID-AMBIENT-USERMODE, $DISPID-AMBIENT-UIDEAD,
	     $DISPID-AMBIENT-SHOWGRABHANDLES, $DISPID-AMBIENT-SHOWHATCHING,
	     $DISPID-AMBIENT-DISPLAYASDEFAULT,
	     $DISPID-AMBIENT-SUPPORTSMNEMONICS, $DISPID-AMBIENT-AUTOCLIP,
	     $DISPID-AMBIENT-APPEARANCE,

	     $DISPID-Name, $DISPID-Delete, $DISPID-Object, $DISPID-Parent,
	     $DISPID-FONT-NAME, $DISPID-FONT-SIZE, $DISPID-FONT-BOLD,
	     $DISPID-FONT-ITALIC, $DISPID-FONT-UNDER, $DISPID-FONT-STRIKE,
	     $DISPID-FONT-WEIGHT, $DISPID-FONT-CHARSET,
	     $DISPID-PICT-HANDLE, $DISPID-PICT-HPAL, $DISPID-PICT-TYPE,
	     $DISPID-PICT-WIDTH, $DISPID-PICT-HEIGHT, $DISPID-PICT-RENDER
	     };

  use simple-format; // `format-to-string' used in `register-ole-control'
  use Win32-common;
  use Win32-user;
  use Win32-GDI;
  use Win32-registry;
  use Win32-kernel; // GlobalAlloc, GlobalFree, GlobalLock, GlobalUnlock
  use Win32-default-handler; // for *error-module-handle*
  use C-FFI;

  // for users to use:
  export <ole-control-framework>, \initialize-ole-control;
  export freeze-events?, freeze-UI?, OLE-util-locale;
  export OLE-util-key-change, OLE-util-on-focus;
  export OLE-util-translate-color;

  // these may not actually be used externally:
  export register-ole-control, register-control-coclass;
  export container-IDispatch;

  // extensible internals:
  export <CPersistStreamInit>,
    <CProvideClassInfo>, <CProvideClassInfo2>;

  // generic functions for which the user can define methods:
  export OLE-part-max-save-size, OLE-part-Load-From-Stream,
    OLE-part-Save-To-Stream;
  export OLE-part-mnemonics, OLE-part-on-mnemonic;
  export OLE-part-set-ambient-property, OLE-part-ambient-properties;

end module;
