Module:    OLE-Server
Synopsis:  Declare generic functions whose methods will be provided by
	   the application-specific code in the libraries using this one.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define open generic OLE-part-title (obj) => title ::  false-or(<string>);

define open generic OLE-part-open-out(obj) => window :: <HWND>;
define open generic OLE-part-hide (obj) => ();

define open generic OLE-part-in-place-activated ( obj ) => ();
define open generic OLE-part-UI-activated ( obj ) => ();
define open generic OLE-part-set-focus ( obj ) => ();
define open generic OLE-part-position-window ( obj, rect, repaint? ) => ();

define open generic OLE-part-insert-menus ( obj, hmenuShared, edit-position,
					   object-position, help-position)
 => ( nedit :: <integer>, nobject :: <integer>, nhelp :: <integer> );
define open generic OLE-part-release-menu (obj, hmenu) => ();


define open generic OLE-part-in-place-deactivated ( obj ) => ();
define open generic OLE-part-UI-deactivated ( obj ) => ();

define open generic OLE-part-doc-window ( obj ) => window :: <HWND>;
define open generic OLE-part-command-window ( obj ) => window :: <HWND>;
define open generic OLE-part-toolbar-window ( obj ) => window :: <HWND>;

define open generic OLE-part-requested-size (obj)
 => ( width :: <integer>, height :: <integer> );
define open generic OLE-part-change-size (obj, width, height)
 => (ok :: <boolean>);

define open generic OLE-part-get-data (obj, pformatetc, pmedium)
 => status :: <HRESULT>;
define open generic OLE-part-set-data (obj, pformatetc, pmedium)
 => status :: <HRESULT>;
define open generic OLE-part-draw-metafile(obj, hDC) => status :: <HRESULT>;

define open generic OLE-part-Create-Streams(obj, storage) => ();
define open generic OLE-part-Open-Streams(obj, storage) => ();
define open generic OLE-part-Release-Streams(obj) => ();
define open generic OLE-part-Save-To-Storage(obj, storage, SameAsLoad?) => ();
define open generic OLE-part-Load-From-Storage(obj, width, height) => ();
define open generic OLE-part-dirty? ( obj ) => dirty? :: <boolean>;
define open generic OLE-part-dirty?-setter (value, obj);

define open generic OLE-part-enable-dialog(obj, enable? :: <boolean>);
