Module:    DUIM-OLE-Container
Synopsis:  object and methods for OLE container frame window
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// the "application object" class for DUIM containers

define open COM-interface <duim-container-app> ( <container-app> ) 

  sealed constant slot app-frame :: <container-frame-mixin>,
		required-init-keyword: frame:;
end <duim-container-app>;


define open abstract free class <container-frame-mixin>
    ( duim/<abstract-frame> )
  slot frame-container-app :: <duim-container-app>;
end class;

define method initialize ( frame :: <container-frame-mixin>,
			  #rest args, #key) => ();
  next-method();
  AddRef(frame.frame-container-app :=
	   make(<duim-container-app>,
		frame: frame,
		palette: null-pointer(w/<HPALETTE>)
		  ));
end method initialize;

define method duim/handle-event
    (frame :: <container-frame-mixin>,
     event :: duim/<frame-mapped-event>) => ()
  let top-sheet = duim/top-level-sheet(frame);
  let app = frame.frame-container-app;
  app.container-frame-window := duim/window-handle(top-sheet);
  let accel = duim/accelerator-table(top-sheet);
  if ( accel )
    app.container-accelerator-table := accel;
  end if;
end method duim/handle-event;
// how to handle accelerators in event loop ???

define method duim/note-win32-frame-destroyed
    (frame ::  <container-frame-mixin>)
 => ();
  Release(frame.frame-container-app);
  next-method();
end method duim/note-win32-frame-destroyed;


//   ------------------------------------------------------------------
//
//   The following methods are added to open generic functions exported
//   from and called by the "ole-container" library.  These methods
//   implement the container application's response to an embedded
//   object's request for frame-level user interface support.
//
//   ------------------------------------------------------------------

define method container-host-names( app :: <duim-container-app> )
 => (application-name, document-name)
  let frame = app.app-frame;
  let title = duim/frame-title(frame);
  values(title, title) // may want way to extend this ???
end;



// The object calls this method when it is actually going to start
// using the border space.
//
// Comments:
//
//      This routine could be a little smarter and check to see if
//      the object is requesting the entire client area of the
//      window.

define method container-set-border-space
    (app :: <duim-container-app>,
     left-space :: <integer>, top-space :: <integer>,
     right-space :: <integer>, bottom-space :: <integer>)
 => ( ok? :: <boolean> )
// ???
/*
  with-stack-structure( rect :: w/<LPRECT> )
    w/GetClientRect(app.container-frame-window, rect);
    w/MoveWindow(app.app-document.doc-container-window,
	       rect.w/left-value + left-space,
	       rect.w/top-value + top-space,
	       rect.w/right-value - right-space - left-space,
	       rect.w/bottom-value - bottom-space - top-space,
	       #t)
  end with-stack-structure
*/
end method;

define method container-set-status-text (app :: <duim-container-app>,
					 text :: <string>)
 => (status :: <HRESULT>)
  // Note: this is being done in a slightly convoluted way so that if the
  // application is itself an embedded server, the message will be 
  // forwarded to the next level container, even if this container doesn't
  // support a status bar itself.
  let frame = app.app-frame;
  let message :: <byte-string> = as(<byte-string>, text);
  duim/frame-status-message(frame) := message;
  let shown = duim/frame-status-message(frame);
  if ( shown == #f )
    if ( empty?(message) )
      $S-OK
    else
      $E-FAIL // no status bar support
    end if
/*
  elseif ( shown ~= message ) // don't think this can actually happen???
    $INPLACE-S-TRUNCATED
*/
  else
    $S-OK
  end if
end method container-set-status-text;

// Need way to call:  ???
//   container-UI-deactivate
//     container-add-verbs
//  container-remove-verbs
//   container-size-changed




define method container-insert-menus(app :: <duim-container-app>,
				     shared-menu :: w/<HMENU>)
  => ( file-count :: <integer>, edit-count :: <integer>,
       help-count :: <integer>)

  debug-message("container-insert-menus\n");
  let frame :: <container-frame-mixin> = app.app-frame;
  let ( file-menu-bar, edit-menu-bar, help-menu-bar ) =
    frame-active-container-menus(frame);
  let help-count :: <fixnum> =
    insert-menu-group(shared-menu, help-menu-bar, frame);
  let edit-count :: <fixnum> = 
    insert-menu-group(shared-menu, edit-menu-bar, frame);
  let file-count :: <fixnum> =
    insert-menu-group(shared-menu, file-menu-bar, frame);
  values( file-count, edit-count, help-count )
end method;

define method insert-menu-group ( hmenuShared :: w/<HMENU>,
				 menu-bar == #f,
				 frame )
 => count :: <fixnum>;
  0
end method;

define method insert-menu-group ( hmenuShared :: w/<HMENU>,
				 menu-bar :: duim/<sheet>,
				 frame )
 => count :: <fixnum>;

  insert-menu-group(hmenuShared, duim/sheet-children(menu-bar), frame)
end method;

define method insert-menu-group ( hmenuShared :: w/<HMENU>,
				 menus :: <sequence>, frame )
 => count :: <fixnum>;
  let count :: <fixnum> = 0;
  for (sub-menu :: duim/<menu> in menus)
    count := count +
      insert-menu-group(hmenuShared, sub-menu, frame)
  end;
  count
end method;

define method insert-menu-group ( hmenuShared :: w/<HMENU>,
				 sub-menu :: duim/<menu>, frame )
 => count :: <fixnum>;

  let mirror = duim/sheet-direct-mirror(sub-menu);
  let sub-hmenu :: w/<HMENU> = 
    if ( mirror )
      duim/window-handle(mirror)
    else
      let handle = duim/make-win32-menu(sub-menu, frame: frame);
      debug-message("make-win32-menu(%=) => %=\n",
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
  if ( w/InsertMenu(hmenuShared, 0,
		  logior(w/$MF-BYPOSITION, w/$MF-POPUP),
		  pointer-address(sub-hmenu),
		  duim/gadget-label(sub-menu)) )
    1
  else
    duim/report-error("InsertMenu");
    0
  end if
end method;

define open generic frame-active-container-menus ( frame )
 => ( file-menu-bar, edit-menu-bar, help-menu-bar );

// Default method; may be overridden by application:
define method frame-active-container-menus ( frame :: duim/<frame> )
 => ( file-menu, edit-menu, help-menu )
  let menu-bar = duim/frame-menu-bar(frame);
  values( if ( menu-bar )
	    first(duim/sheet-children(menu-bar))
	  else
	    #f
	  end if,
	 #f, #f )
end method;

/* do we need an equivalent of this ???
define method OLE-part-release-menu (obj :: <basic-DUIM-OLE-server>,
				     hmenu :: w/<HMENU>) => ();
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
*/


define method note-enable-modeless (app :: <duim-container-app>,
				    enable? :: <boolean>) => ()
  let frame :: duim/<frame> = app.app-frame;
  let top-sheet = duim/top-level-sheet(frame);
  if ( top-sheet )
    let top-level-mirror = duim/sheet-mirror(top-sheet);
    if ( top-level-mirror )
      let dialogs = duim/mirror-registered-dialogs(top-level-mirror);
      for ( dialog-mirror :: duim/<mirror> in dialogs )
	let dialog-sheet = duim/mirror-sheet(dialog-mirror);
	let dialog-frame = duim/sheet-frame(dialog-sheet);
	if ( instance?(dialog-frame, duim/<dialog-frame>) )
	  debug-message(" frame-enabled?(%=) := %=\n", dialog-frame, enable?);
	  duim/frame-enabled?(dialog-frame) := enable?;
	end if;
      end for;
    end if;
  end if;
  values()
end method;
