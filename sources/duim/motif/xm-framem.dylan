Module:    motif-duim
Synopsis:  Motif frame manager implementation
Author:    Scott McKay, Stuart Croy
	   Based on work by John Aspinall and Richard Billington
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Motif frame management

define sealed class <motif-frame-manager> (<basic-frame-manager>)
end class <motif-frame-manager>;

define sealed domain make (singleton(<motif-frame-manager>));
define sealed domain initialize (<motif-frame-manager>);

define sealed method make-frame-manager
    (_port :: <motif-port>,
     #key palette, class = <motif-frame-manager>, #all-keys)
 => (framem :: <frame-manager>)
  make(class, port: _port, palette: palette)
end method make-frame-manager;


define method note-frame-title-changed
    (framem :: <motif-frame-manager>, frame :: <frame>) => ()
  // Update the title in the top-level window
  let sheet  = top-level-sheet(frame);
  let mirror = sheet & sheet-direct-mirror(top-sheet);
  when (mirror)
    let widget = top-level-shell-widget(mirror);
    let name   = frame-title(frame) | "";
    if (xt/WidgetClassName(xt/XtClass widget) = #"top-level-shell")
      xt/XtSetValues(widget, title: name, icon-name: name)
    else
      xt/XtSetValues(widget, title: name)
    end
  end
end method note-frame-title-changed;


define method note-frame-icon-changed
    (framem :: <motif-frame-manager>, frame :: <frame>) => ()
  // Update the icon in the top-level window
  let sheet  = top-level-sheet(frame);
  let mirror = sheet & sheet-direct-mirror(top-sheet);
  when (mirror)
    update-mirror-icon(mirror, frame-icon(frame))
  end
end method note-frame-icon-changed;

//---*** WHAT TO DO???
define method update-mirror-icon
    (mirror :: <top-level-mirror>, icon :: false-or(<image>)) => ()
  let hIcon = select (icon by instance?)
		<motif-icon>  => image-handle(icon);
		singleton(#f) => null-pointer(<HICON>);
		otherwise     => #f;
	      end;
  when (hIcon)
    let handle = window-handle(mirror);
    SendMessage(handle, $WM-SETICON, $ICON-SMALL, pointer-address(hIcon));
    SendMessage(handle, $WM-SETICON, $ICON-BIG,   pointer-address(hIcon))
  end
end method update-mirror-icon;


//---*** WHAT TO DO???
define method do-frame-occluded?
    (framem :: <motif-frame-manager>, frame :: <basic-frame>)
 => (occluded? :: <boolean>)
  let sheet  = top-level-sheet(frame);
  let handle = sheet & window-handle(sheet);
  // If it's not attached, just pretend it's occluded
  // If we're not the foreground window, assume we're occluded
  //--- It would be better to use GetNextWindow to loop over all of
  //--- the windows higher in the Z-order, and return #t only if
  //--- there is a window whose GetWindowRect overlaps our own...
  handle ~= GetForegroundWindow()
end method do-frame-occluded?;


//---*** WHAT TO DO???
define method note-frame-enabled
    (framem :: <motif-frame-manager>, frame :: <basic-frame>) => ()
  let sheet  = top-level-sheet(frame);
  let handle = sheet & window-handle(sheet);
  // Check IsWindowEnabled first to avoid circularity that arises
  // from handling the ensuing $WM-ENABLE message
  handle & ~IsWindowEnabled(handle) & EnableWindow(handle, #t)
end method note-frame-enabled;

//---*** WHAT TO DO???
define method note-frame-disabled
    (framem :: <motif-frame-manager>, frame :: <basic-frame>) => ()
  let sheet  = top-level-sheet(frame);
  let handle = sheet & window-handle(sheet);
  // Check IsWindowEnabled first to avoid circularity that arises
  // from handling the ensuing $WM-ENABLE message
  handle & IsWindowEnabled(handle) & EnableWindow(handle, #f)
end method note-frame-disabled;


define sealed method note-frame-iconified
    (framem :: <motif-frame-manager>, frame :: <simple-frame>) => ()
  next-method();				// update the frame's state
  let sheet  = top-level-sheet(frame);
  let mirror = sheet & sheet-direct-mirror(sheet);
  when (mirror)
    let widget = top-level-shell-widget(mirror);
    xt/XtSetValues(widget, iconic: #t)
  end
end method note-frame-iconified;

define sealed method note-frame-deiconified
    (framem :: <motif-frame-manager>, frame :: <simple-frame>) => ()
  next-method();				// update the frame's state
  let sheet  = top-level-sheet(frame);
  let mirror = sheet & sheet-direct-mirror(sheet);
  when (mirror)
    let widget = top-level-shell-widget(mirror);
    xt/XtSetValues(widget, iconic: #f)
  end
end method note-frame-deiconified;


/// Frame-level event handling

define method install-frame-event-handlers
    (mirror :: <top-level-mirror>) => ()
  let shell = mirror-shell-widget(mirror);
  xm/XmAddWmProtocolCallback(shell, xm/$WM-DELETE-WINDOW, delete-window-callback, mirror)
end method install-frame-event-handlers;

define xm/xm-callback-function delete-window-callback
    (widget, mirror, call-data)
  let sheet      = mirror-sheet(mirror);
  let frame      = sheet & sheet-frame(sheet);
  let controller = frame & frame-controlling-frame(frame);	//---*** use 'frame-owner'???
  xt/XtSetValues(widget, delete-response: if (controller) #"do-nothing" else #"destroy" end);
  when (controller)
    //---*** Is this really what we mean to do?
    //---*** CLIM dispatches a delete event to 'sheet'
    exit-frame(frame, destroy?: #t)
  end
end xm/xm-callback-function delete-window-callback;
