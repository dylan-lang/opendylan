Module:    DUIM-OLE-Container
Synopsis:  class and methods for an embedded OLE server object
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define inline-only constant <fixnum> = <integer>;

define open primary COM-interface <duim-contained-object> (<contained-object>)
  // The DUIM sheet that represents the OLE object:
  sealed slot object-sheet :: false-or(<ole-gadget>) = #f;
  // The following flag is used to avoid sending inappropriate commands
  // to an OLE object that is either not yet fully initialized or is in
  // process of termination.
  sealed slot alive? :: <boolean> = #f;
end <duim-contained-object>;

define method terminate ( obj :: <duim-contained-object> ) => ();
  obj.alive? := #f;
  let obj-sheet = obj.object-sheet;
  if ( obj-sheet )
    let parent-sheet = duim/sheet-parent(obj-sheet);
    if ( parent-sheet )
      duim/remove-child(parent-sheet, obj-sheet);
    end if;
  end if;
  next-method();
end method terminate;

define method container-document-class (app :: <duim-container-app>)
 => (class :: <class>)
  <duim-contained-object>
end method;

define method frame-container-app ( sheet :: duim/<sheet> )
 => ( app :: <duim-container-app> )
  let frame = duim/sheet-frame(sheet);
  frame.frame-container-app
end;

define method insert-by-class (parent-sheet :: duim/<sheet>,
			       ID :: <object>, #rest options)
 => ( child-sheet :: <ole-gadget> )
  if ( duim/sheet-mapped?(parent-sheet) )
    // initiate OLE server now.
    let app = parent-sheet.frame-container-app;
    let object = apply(insert-object-by-class, app,
		       duim/window-handle(parent-sheet), ID, options);
    // Now that the OLE server has been successfully initiated, create
    // its DUIM representation.
    make-sheet-for-object(object, parent-sheet)
  else // defer server activation until a window handle is available.
    make(<ole-gadget>, parent: parent-sheet,
	 class-id: ID, insert-options: options)
  end if
end method insert-by-class;

define method insert-from-file (parent-sheet :: duim/<sheet>,
				file-name :: <string>, #rest options)
 => ( child-sheet :: <ole-gadget> )
  if ( duim/sheet-mapped?(parent-sheet) )
    // initiate OLE server now.
    let app = parent-sheet.frame-container-app;
    let object = apply(insert-object-from-file, app,
		       duim/window-handle(parent-sheet), file-name, options);
    make-sheet-for-object(object, parent-sheet)
  else // defer server activation until a window handle is available.
    make(<ole-gadget>, parent: parent-sheet,
	 file-name: file-name, insert-options: options)
  end if
end method insert-from-file;

define method insert-from-dialog (parent-sheet :: duim/<sheet>, #rest options)
 => ( child-sheet :: false-or(<ole-gadget>) )
  let parent-handle = duim/window-handle(parent-sheet);
  if ( parent-handle == #f )
    // Don't defer activation of the server in this case because
    // this involves a user interaction which should not be deferred.
    error("Can't insert OLE object into unmirored sheet");
    #f
  else
    let app = parent-sheet.frame-container-app;
    // Present the user with a dialog box for choosing an OLE server:
    let object = apply(insert-object-from-dialog,
		       app, parent-handle, options);
    if ( object == #f ) // operation failed or cancelled
      #f
    else
      // Now that the OLE server has been successfully initiated, create
      // its DUIM representation.
      make-sheet-for-object(object, parent-sheet)
    end if
  end if
end method insert-from-dialog;

define function make-sheet-for-object ( obj :: <duim-contained-object>,
				        parent-sheet :: duim/<sheet> )
 => ( obj-sheet :: <ole-gadget> )
  let (left, top, right, bottom) = document-edges(obj);
  let obj-sheet :: <ole-gadget> =
    make(<ole-gadget>, contained-object: obj, parent: parent-sheet,
	 x: left, y: top, width: right - left, height: bottom - top);
  obj-sheet
end make-sheet-for-object;
