Module:    sample-OLE-container
Synopsis:  class and methods for handling the embedded document.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <simple-doc> ( <object> ) 
  slot doc-container-window :: <HWND> = $NULL-HWND;
  slot contained-object :: false-or(<contained-object>) = #f;
end <simple-doc>;

define method open-document(app :: <sample-container-app>,
			    rectangle :: <LPRECT>,
			    frame-window  :: <HWND>,
			    parent-window :: <HWND>)
	=> value :: <simple-doc>;

  let doc :: <simple-doc> = make(<simple-doc>);

  let main-menu = GetMenu(frame-window);
  app.app-file-menu := GetSubMenu(main-menu, 0);
  app.app-edit-menu := GetSubMenu(main-menu, 1);
  app.app-help-menu := GetSubMenu(main-menu, 2);

  // create the document Window
  let doc-window :: <HWND> =
    CreateWindow($doc-window-class,
		 $NULL-string,
		 %logior($WS-CHILD, $WS-CLIPCHILDREN),
		 rectangle.left-value,
		 rectangle.top-value,
		 rectangle.right-value,
		 rectangle.bottom-value,
		 parent-window,
		 null-pointer(<HMENU>),
		 app.app-instance-handle,
		 $NULL-VOID);
  doc.doc-container-window := doc-window;

  ShowWindow(doc-window, $SW-SHOWNORMAL);	// Show the window
  UpdateWindow(doc-window);			// Sends WM_PAINT message

  // Enable InsertObject menu choice
  EnableMenuItem(app.app-edit-menu, 0,
		 logior($MF-BYPOSITION, $MF-ENABLED));
  doc
end method open-document;


define method close-document(doc :: <simple-doc>, save? :: <boolean>) => ();

  debug-message("close-document\n");

  ShowWindow(doc.doc-container-window, $SW-HIDE);  // Hide the window
  if ( doc.contained-object )
    // if the edit menu was modified, remove the menu item and
    // destroy the cascading sub-menu if it exists
    container-remove-verbs(doc.contained-object);

    close-doc-object(doc.contained-object, save?: save?);
    doc.contained-object := #f;
  end if;

  DestroyWindow(doc.doc-container-window);
  values()
end method close-document;


// Inserts a new object to this document
//
// Comments:
//
//      This implementation only allows one object to be inserted
//      into a document.  Once the object has been inserted, then
//      the Insert Object menu choice is grayed out, to prevent
//      the user from inserting another.

define method insert-object(app :: <sample-container-app>,
			    doc :: <simple-doc>) => ();

  let contained = insert-object-from-dialog(app,
					    doc.doc-container-window);
  doc.contained-object := contained;
  if ( contained )
    // an object was successfully inserted 
    // disable "Insert Object" menu item
    EnableMenuItem(app.app-edit-menu, 0,
		   logior($MF-BYPOSITION, $MF-DISABLED, $MF-GRAYED));
  end if;
end;

