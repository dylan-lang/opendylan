Module:    DUIM-OLE-server
Synopsis:  methods on the "application" object
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method initialize ( App :: <DUIM-server-app>,
			  #rest args, #key interface, #all-keys) => ();
  next-method();

  let frame = App.app-frame;
  frame.frame-ole-object := interface;

  let Doc :: <DUIM-server-doc> =
    make(<DUIM-server-doc>, app: App, interface: interface);
  App.get-doc := Doc;
  frame.ole-frame-doc-obj := Doc;

  // This code copied from `duim/map-frame';
  // Create layout and mirrors, but don't yet map.
  duim/attach-frame(duim/frame-manager(frame), frame);
  let top-sheet = duim/top-level-sheet(frame);
  let geometry = duim/frame-geometry(frame);
  let x      = geometry[0];
  let y      = geometry[1];
  let width  = geometry[2];
  let height = geometry[3];
  duim/layout-frame(frame, width: width, height: height);
  if (x & y)
    duim/set-sheet-position(top-sheet, x, y)
  end if;

  App.app-window := duim/window-handle(top-sheet);

  let user-sheet = frame-embedded-sheet(frame);
  if ( user-sheet )
    Doc.doc-sheet := user-sheet;
    Doc.doc-window := $NULL-HWND; // disallow in-place activation
  else
    if ( Doc.doc-sheet == #f )
      Doc.doc-sheet := find-doc-sheet(top-sheet);
    end if;
    Doc.doc-window := duim/window-handle(Doc.doc-sheet);
  end if;

  App.started-by-ole? := OLE-util-started-by-OLE?();

  values()
end method initialize;

define method duim/frame-wrapper
    (framem :: duim/<win32-frame-manager>, frame :: <embeddable-frame>,
     layout-sheet :: false-or(duim/<sheet>))
 => (wrapper :: false-or(duim/<sheet>))
  if ( duim/frame-top-level-sheet-class(frame) ==
	duim/<embedded-top-level-sheet> ) // true for an OCX
    // Embedded-only frames don't have any decorations; just use the
    // <embedded-top-level-sheet> as the doc sheet.
    layout-sheet
  else
    // Might need decorations for out-of-place activation.
    let doc-sheet :: false-or(duim/<sheet>) = find-doc-sheet(layout-sheet);
    let wrapper = 
      if ( doc-sheet == #f )
	// Didn't find a single pane that can be embedded directly, so create
	// a new embeddable wrapper sheet.
	doc-sheet := make(<ole-main-sheet>);
	debug-out("sheet-parent(%=) := %=\n", layout-sheet, doc-sheet);
	duim/sheet-parent(layout-sheet) := doc-sheet;
	next-method(framem, frame, doc-sheet)
      else
	next-method()
      end if;
    let Doc = frame.ole-frame-doc-obj;
    if ( Doc )
      Doc.doc-sheet := doc-sheet;
    end if;
    wrapper
  end if
end method;

define method find-doc-sheet ( sheet :: duim/<sheet> )
 => sheet :: false-or(duim/<sheet>);
  // Using select by instance? instead of separate methods because the
  // class precedence order of the various mixins doesn't match our
  // priorities here.
  select (sheet by instance?)

    duim/<embedded-top-level-sheet> => sheet;

    duim/<top-level-sheet> =>
      begin
	// Return the user's original sheet, without any menu bar, tool bar
	// or status bar.
	let frame = sheet.duim/sheet-frame;
	let main-sheet = frame.duim/frame-layout;
	find-doc-sheet(main-sheet)
      end;

    duim/<action-gadget>, duim/<accelerator-mixin> =>
      // needs to have a mirrored parent to which event messages can be sent.
      #f;

    duim/<mirrored-sheet-mixin> => sheet;

    otherwise =>
      begin
	let children = duim/sheet-children(sheet);
	if ( size(children) = 1 )
	  find-doc-sheet(children[0])
	else
	  // Can't directly use the top-level sheet because we have to have
	  // a window of style WS_CHILD that can be re-parented to the 
	  // container.  Can't directly use a <layout> sheet because it
	  // has to have a mirror in order to receive Windows messages.  
	  // So create a new mirrored sheet.
	  #f
	end if
      end;
  end select
end;

// A layout sheet with a mirror that can be used as an embeddable parent for 
// multiple panes.
define class <ole-main-sheet> (duim/<mirrored-sheet-mixin>,
			       duim/<multiple-child-wrapping-pane>,
			       duim/<null-repainting-mixin>,
			       duim/<standard-input-mixin>)
end class;

// Temporary method to fill a gap in "win32-duim", (which, as of 11/13/97),
// defines a method on <drawing-pane> instead of <mirrored-sheet-mixin>):
define method duim/port-handles-repaint?
    (_port :: duim/<win32-port>, sheet :: <ole-main-sheet>)
 => (true? :: <boolean>)
  #t
end method duim/port-handles-repaint?;

// Needed to satisfy a (possibly erroneous) call in `repaint-mirror':
define method duim/sheet-medium (sheet :: <ole-main-sheet>) 
 => (medium :: false-or(duim/<medium>));
  #f
end method;

/*
// Clear the container's status bar text:
define method clear-status-text(App :: <DUIM-server-app>) => ();
  OLE-util-set-status-text(App.get-doc.get-object, #f);
  values()
end method clear-status-text;
*/


// Show the application as a top-level frame:
define method show-app-window(App :: <DUIM-server-app>) => ();
  let frame = App.app-frame;
  duim/frame-mapped?(frame) := #t;
  values()
end method show-app-window;

