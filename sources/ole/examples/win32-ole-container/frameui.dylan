Module:    sample-OLE-container
Synopsis:  object and methods for OLE container frame window
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// the "application object" class for this program

define COM-interface <sample-container-app> ( <container-app> ) 

  slot app-instance-handle :: <HINSTANCE> = $NULL-HINSTANCE;
  slot app-document :: false-or(<simple-doc>)= #f; // embedded document object
  slot app-activated? :: <boolean> = #f;      // true if app is active

  slot app-file-menu :: <HMENU> = $NULL-HMENU;
  slot app-edit-menu :: <HMENU> = $NULL-HMENU;
  slot app-help-menu :: <HMENU> = $NULL-HMENU;
end <sample-container-app>;

define method contained-object ( app :: <sample-container-app> )
 => ( contained :: false-or(<contained-object>) )
  let doc = app.app-document;
  doc & doc.contained-object
end;

//   ------------------------------------------------------------------
//
//   The following methods are added to open generic functions exported
//   from and called by the "ole-container" library.  These methods
//   implement the container application's response to an embedded
//   object's request for frame-level user interface support.
//
//   ------------------------------------------------------------------

define method container-host-names( app :: <sample-container-app> )
 => (application-name, document-name)
  values("win32-OLE-container app",
 	 "win32-OLE-container doc")
end;

define method container-insert-menus(app :: <sample-container-app>,
				     shared-menu :: <HMENU>)
  => ( file-count :: <integer>, edit-count :: <integer>,
       help-count :: <integer>)

  debug-message("container-insert-menus\n");

  AppendMenu(shared-menu, logior($MF-BYPOSITION, $MF-POPUP),
	     pointer-address(app.app-file-menu), TEXT("&File"));
  AppendMenu(shared-menu, logior($MF-BYPOSITION, $MF-POPUP),
	     pointer-address(app.app-help-menu), TEXT("&Other"));
  values(1, 0, 1)
end method;


// The object calls this method when it is actually going to start
// using the border space.
//
// Comments:
//
//      This routine could be a little smarter and check to see if
//      the object is requesting the entire client area of the
//      window.

define method container-set-border-space
    (app :: <sample-container-app>,
     left-space :: <integer>, top-space :: <integer>,
     right-space :: <integer>, bottom-space :: <integer>)
 => ( ok? :: <boolean> )

  with-stack-structure( rect :: <LPRECT> )
    GetClientRect(app.container-frame-window, rect);
    MoveWindow(app.app-document.doc-container-window,
	       rect.left-value + left-space,
	       rect.top-value + top-space,
	       rect.right-value - right-space - left-space,
	       rect.bottom-value - bottom-space - top-space,
	       #t)
  end with-stack-structure
end method;

