Module:    DUIM-OLE-server
Synopsis:  This file defines the DUIM-specific methods that are called
	   from the generic OLE server framework.  (Not including a few
	   more that are in file "storage.dylan".)
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method OLE-part-doc-window ( obj :: <basic-DUIM-OLE-server> )
 => doc-window :: <HWND>;
  obj.doc-window
end method;

// The window handle to which menu choices will be dispatched:
define method OLE-part-command-window ( obj :: <basic-DUIM-OLE-server> )
 => ( window :: <HWND> );
  app-window(obj.get-doc)
end method;

define method OLE-part-title (obj :: <basic-DUIM-OLE-server>)
				=> title :: false-or(<string>);
  let frame :: duim/<frame> = obj.app-frame;
  duim/frame-title(frame)
end method;

// Open as separate top-level window (instead of in-place).
define method OLE-part-open-out(obj :: <basic-DUIM-OLE-server>)
 => window :: <HWND>; 

  let doc :: <DUIM-server-doc> = obj.get-doc;

  // Show app window.
  let app = get-app(doc);
  show-app-window(app);
  app-window(doc)
end method;

define method OLE-part-show-window (obj :: <basic-DUIM-OLE-server>,
				    window :: <HWND>) => ();

  let mirror = duim/window-mirror(window);
  if ( mirror )
    let sheet = duim/mirror-sheet(mirror);
    // Show the window by mapping the DUIM sheet.
    // Note: the `do-repaint?' option really controls repainting of the parent;
    //	this sheet will still be painted anyway.
    duim/sheet-mapped?-setter(#t, sheet, do-repaint?: #f);
    values()
  else
    next-method()
  end if
end method;

define method OLE-part-in-place-activated ( obj :: <basic-DUIM-OLE-server> )
 => ();
  let frame = obj.app-frame;
  frame.frame-in-place-active? := #t;
  OLE-util-set-status-text(obj, #f); // clear the container's status bar
  frame.ole-frame-status-message := #f;
  note-in-place-activation(frame);
end method;

define method OLE-part-UI-deactivated ( obj :: <basic-DUIM-OLE-server> )
 => ();
  let frame = obj.app-frame;
  if ( frame.ole-frame-status-message )
    OLE-util-set-status-text(obj, #f); // clear the container's status bar
    frame.ole-frame-status-message := #f;
  end if;
  let tool-bar = frame.duim/frame-tool-bar;
  if ( tool-bar )
    duim/sheet-mapped?-setter(#f, tool-bar, do-repaint?: #f);
  end if;
  values()
end method;

define method OLE-part-in-place-deactivated ( obj :: <basic-DUIM-OLE-server> )
 => ();
  let frame = obj.app-frame;
  note-in-place-deactivation(frame);
  // Reset the flag afterwards to allow frame-status-message-setter to be used
  // from note-in-place-deactivation.
  frame.frame-in-place-active? := #f;
  duim/sheet-mapped?-setter(#f, obj.doc-sheet, do-repaint?: #f);
  values()
end method;

/* // on second thought, maybe it is better not to do this, since this 
   // position is relative to the container window, which DUIM doesn't
   // know anything about.	-- DNG 9/11/96
define method OLE-part-position-window ( obj :: <basic-DUIM-OLE-server>,
				      rect :: <LPRECT>,
				      repaint? :: <boolean> ) => ();
  let doc-sheet = obj.doc-sheet;
  duim/set-sheet-edges(doc-sheet,
		       rect.left-value, rect.top-value,
		       rect.right-value, rect.bottom-value);
  if ( repaint? )
    UpdateWindow(duim/%window-handle(doc-sheet));
  end if;
  values()
end method;
*/

define method OLE-part-change-size (obj :: <basic-DUIM-OLE-server>,
				    width, height) => (ok :: <boolean>);
  note-embedded-region-changed(obj.app-frame, width, height)
end method;


define method OLE-part-insert-menus ( obj :: <basic-DUIM-OLE-server>,
				     hmenuShared :: <HMENU>,
				     edit-position :: <integer>,
				     object-position :: <integer>,
				     help-position :: <integer> )
 => ( nedit :: <integer>, nobject :: <integer>, nhelp :: <integer> );

  let frame :: duim/<frame> = obj.app-frame;
  let ( edit-menu-bar, object-menu-bar, help-menu-bar ) =
    frame-container-menus(frame);
  let help-count :: <fixnum> =
    insert-menu-group (hmenuShared, help-menu-bar, help-position, frame);
  let object-count :: <fixnum> =
    insert-menu-group (hmenuShared, object-menu-bar, object-position, frame);
  let edit-count :: <fixnum> = 
    insert-menu-group (hmenuShared, edit-menu-bar, edit-position, frame);
  values( edit-count, object-count, help-count )
end method;

define method insert-menu-group ( hmenuShared :: <HMENU>,
				 menu-bar == #f,
				 starting-position :: <fixnum>,
				 frame )
 => count :: <fixnum>;
  0
end method;

define method insert-menu-group ( hmenuShared :: <HMENU>,
				 menu-bar :: duim/<sheet>,
				 starting-position :: <fixnum>,
				 frame )
 => count :: <fixnum>;

  insert-menu-group(hmenuShared, duim/sheet-children(menu-bar),
		    starting-position, frame)
end method;

define method insert-menu-group ( hmenuShared :: <HMENU>,
				 menus :: <sequence>,
				 starting-position :: <fixnum>,
				 frame )
 => count :: <fixnum>;
  let count :: <fixnum> = 0;
  for (sub-menu :: duim/<menu> in menus)
    count := count +
      insert-menu-group(hmenuShared, sub-menu,
			starting-position + count, frame)
  end;
  count
end method;

define method insert-menu-group ( hmenuShared :: <HMENU>,
				 sub-menu :: duim/<menu>,
				 starting-position :: <fixnum>,
				 frame )
 => count :: <fixnum>;

  let mirror = duim/sheet-direct-mirror(sub-menu);
  let sub-hmenu :: <HMENU> = 
    if ( mirror )
      duim/window-handle(mirror)
    else
      let handle = duim/make-win32-menu(sub-menu, frame: frame);
      debug-out("make-win32-menu(%=) => %=\n",
		sub-menu, pointer-address(handle));

      unless ( duim/port(sub-menu) )
	// Probably this ought to be done in `make-win32-menu', but it isn't.
	// (Normally this happens in `graft-sheet'.)
	let port = duim/port(frame);
	duim/%port(sub-menu) := port;
	for ( child :: duim/<sheet> in duim/sheet-children(sub-menu) )
	  // This is necessary in order to dispatch to the correct method for
	  // `distribute-event' when the button is pressed.
	  duim/%port(child) := port;
	end;
      end unless;
      handle
    end if;
  if ( InsertMenu(hmenuShared, starting-position,
		  logior($MF-BYPOSITION, $MF-POPUP),
		  pointer-address(sub-hmenu),
		  duim/gadget-label(sub-menu)) )
    1
  else
    report-win32-error("InsertMenu");
    0
  end if
end method;

define open generic frame-container-menus ( frame )
 => ( edit-menu-bar, object-menu-bar, help-menu-bar );

// Default method; may be overridden by application:
define method frame-container-menus ( frame :: duim/<frame> )
 => ( menu-bar :: false-or(duim/<sheet>), object-menu-bar, help-menu-bar );
  values( duim/frame-menu-bar(frame), #f, #f )
end method;

define method OLE-part-release-menu (obj :: <basic-DUIM-OLE-server>,
				     hmenu :: <HMENU>) => ();
  debug-out("OLE-part-release-menu %=\n", pointer-address(hmenu));
  let mirror = duim/window-mirror(hmenu);
  if ( mirror )
    let sheet = duim/mirror-sheet(mirror);
    duim/destroy-mirror(duim/port(sheet), sheet, mirror); // calls DestroyMenu
  else  // This should not happen; maybe should signal an error???
    debug-out(" No mirror for %=\n", hmenu);
  end if;
  values()
end method;



define method OLE-part-draw-metafile(obj :: <basic-DUIM-OLE-server>,
				     hDC :: <HDC>)
				=> (status :: <HRESULT>);
  Output-Debug-String("OLE-part-draw-metafile\r\n");
  let sheet = obj.doc-sheet;
  duim/repaint-in-dc-recursive(sheet, hDC, 1, 1, 0, 0);
  $S-OK
end method OLE-part-draw-metafile;


define method OLE-part-requested-size ( obj :: <basic-DUIM-OLE-server> ) 
 => (width :: <integer>, height :: <integer>);
  frame-embedded-size-requested(obj.app-frame)
end method;

define method OLE-part-toolbar-window ( obj :: <basic-DUIM-OLE-server> )
 => (window :: <HWND>);
  Output-Debug-String("OLE-part-toolbar-window\r\n");
  let tool-bar = obj.app-frame.duim/frame-tool-bar;
  if ( tool-bar )
    duim/window-handle(duim/sheet-mirror(tool-bar))
	// but what if the mirror hasn't been created yet???
  else
    $NULL-HWND
  end if
end;

define method OLE-part-accelerators (obj :: <basic-DUIM-OLE-server>)
 => (table :: false-or(<HACCEL>));
  // Output-Debug-String("OLE-part-accelerators\r\n");
  duim/accelerator-table(obj.doc-sheet)
end method;

define method OLE-part-enable-dialog (obj :: <basic-DUIM-OLE-server>,
				      enable? :: <boolean>) => ();
  let frame :: duim/<frame> = obj.app-frame;
  let top-sheet = duim/top-level-sheet(frame);
  if ( top-sheet )
    let top-level-mirror = duim/sheet-mirror(top-sheet);
    if ( top-level-mirror )
      let dialogs = duim/mirror-registered-dialogs(top-level-mirror);
      for ( dialog-mirror :: duim/<mirror> in dialogs )
	let dialog-sheet = duim/mirror-sheet(dialog-mirror);
	let dialog-frame = duim/sheet-frame(dialog-sheet);
	if ( instance?(dialog-frame, duim/<dialog-frame>) )
	  debug-out(" frame-enabled?(%=) := %=\n", dialog-frame, enable?);
	  duim/frame-enabled?(dialog-frame) := enable?;
	end if;
      end for;
    end if;
  end if;
  values()
end method;
