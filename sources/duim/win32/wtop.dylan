Module:    win32-duim
Synopsis:  Win32 top level window handling
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Some magic Win32 constants

define constant $first-gadget-id :: <integer> = 1000;

//---*** This should really be computed
define constant $top-level-y-spacing = 3;		// in pixels

//---*** These should be defined in Win32-User
define constant $ACCEL-FVIRTKEY     :: <integer> = #x01;
// define constant $ACCEL-FNOINVERT :: <integer> = #x02;
define constant $ACCEL-FSHIFT       :: <integer> = #x04;
define constant $ACCEL-FCONTROL     :: <integer> = #x08;
define constant $ACCEL-FALT         :: <integer> = #x10;


/// Frame protocols

define protocol <<win32-frame-protocol>> ()
  function note-win32-frame-destroyed (frame :: <abstract-frame>) => ();
end protocol <<win32-frame-protocol>>;


/// Top level mirrors

define sealed class <top-level-mirror> (<window-mirror>)
  // State for allocating resource ids and mapping ids<->gadgets
  sealed slot %next-resource-id = $first-gadget-id;
  sealed slot %resource-id-table :: <object-table> = make(<table>);
  // Accelerator table for the frame
  sealed slot %accelerator-table :: false-or(<HACCEL>) = #f;
  // A shared ToolTip for all of the sheets in the frame
  sealed slot %tool-tip = #f,
    init-keyword: tool-tip:;
  sealed slot %dialog-mirrors :: <stretchy-object-vector> = make(<stretchy-vector>);
end class <top-level-mirror>;

define sealed domain make (singleton(<top-level-mirror>));
define sealed domain initialize (<top-level-mirror>);

define sealed method top-level-mirror
    (sheet :: <sheet>, #key error? = #f)
 => (mirror :: false-or(<top-level-mirror>))
  let sheet  = top-level-sheet(sheet);
  let mirror = sheet & sheet-direct-mirror(sheet);
  mirror
    | (error? & error("Failed to find top-level mirror for %=", sheet))
end method top-level-mirror;

define sealed method top-level-mirror
    (frame :: <frame>, #key error? = #f)
 => (mirror :: false-or(<top-level-mirror>))
  let sheet  = top-level-sheet(frame);
  let mirror = sheet & sheet-direct-mirror(sheet);
  mirror
    | (error? & error("Failed to find top-level mirror for %=", sheet))
end method top-level-mirror;


/// Dialog handling

define method mirror-registered-dialogs
    (mirror :: <top-level-mirror>) => (dialogs :: <sequence>)
  mirror.%dialog-mirrors
end method mirror-registered-dialogs;

define method register-dialog-mirror
    (frame :: <simple-frame>, dialog-mirror :: <dialog-mirror>) => ()
  let top-sheet = top-level-sheet(frame);
  when (top-sheet)
    let top-mirror = sheet-direct-mirror(top-sheet);
    add!(top-mirror.%dialog-mirrors, dialog-mirror)
  end
end method register-dialog-mirror;

define method unregister-dialog-mirror
    (frame :: <simple-frame>, dialog-mirror :: <dialog-mirror>) => ()
  let top-sheet = top-level-sheet(frame);
  when (top-sheet)
    let top-mirror = sheet-direct-mirror(top-sheet);
    remove!(top-mirror.%dialog-mirrors, dialog-mirror)
  end
end method unregister-dialog-mirror;


/// Top level sheets

define abstract class <win32-top-level-sheet-mixin>
    (<standard-repainting-mixin>,
     <permanent-medium-mixin>,
     <win32-pane-mixin>)
  slot %needs-activation? :: <boolean> = #f;
end class <win32-top-level-sheet-mixin>;

define sealed class <win32-top-level-sheet>
    (<win32-top-level-sheet-mixin>,
     <top-level-sheet>)
end class <win32-top-level-sheet>;

define sealed domain make (singleton(<win32-top-level-sheet>));
define sealed domain initialize (<win32-top-level-sheet>);

define sealed method initialize
    (sheet :: <win32-top-level-sheet>, #key) => ()
  next-method();
  sheet-accepts-focus?(sheet) := #f
end method initialize;

define method class-for-make-pane
    (framem :: <win32-frame-manager>, class == <top-level-sheet>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-top-level-sheet>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-top-level-sheet>));
define sealed domain initialize (<win32-top-level-sheet>);


// Like a top-level sheet, but for embedded apps such as OLE parts
define sealed class <win32-embedded-top-level-sheet>
    (<win32-top-level-sheet-mixin>,
     <embedded-top-level-sheet>)
end class <win32-embedded-top-level-sheet>;

define method class-for-make-pane
    (framem :: <win32-frame-manager>, class == <embedded-top-level-sheet>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-embedded-top-level-sheet>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-embedded-top-level-sheet>));
define sealed domain initialize (<win32-embedded-top-level-sheet>);


define sealed method do-make-mirror
    (_port :: <win32-port>, sheet :: <win32-top-level-sheet-mixin>)
 => (mirror :: <win32-mirror>)
  ignore(_port);
  let frame = sheet-frame(sheet);
  let resource-id = frame-resource-id(frame);
  let (handle, resource, mirror-class, mirror-initargs)
    = make-top-level-window(frame, sheet, resource-id);
  let (left, top, right, bottom) = get-window-edges(handle);
  let mirror
    = apply(make,
	    mirror-class,
	    sheet:  sheet,
	    handle: handle,
	    resource: resource,
	    region: make-bounding-box(left, top, right, bottom),
	    mirror-initargs);
  let container = sheet-parent-window(sheet);
  if (container == #f)
    // Create a shared ToolTip so that the entire application can use it
    make-frame-tooltip(frame, mirror);
    // Update the icon for the frame
    update-mirror-icon(mirror, frame-icon(frame));
  end;
  when (frame-keyboard-interrupt?(frame))
    register-keyboard-interrupt-handler(_port, sheet)
  end;
  mirror
end method do-make-mirror;

// Returns the handle of a container, or #f
define sealed method sheet-parent-window
    (sheet :: <top-level-sheet>)
 => (window :: false-or(<HWND/HMENU>))
  let container = sheet-container(sheet);
  when (container)
    select (container by instance?)
      <frame> => 
	let top-sheet = top-level-sheet(container);
	top-sheet & window-handle(top-sheet);
      <sheet> => 
	window-handle(container);
      <mirror> =>
	window-handle(container);
      otherwise =>	// presumably an <HWND>...
	container;
    end
  end
end method sheet-parent-window;

// Returns the handle of the (nearest) parent window, or #f
define sealed method sheet-parent-window
    (sheet :: <sheet>)
 => (window :: false-or(<HWND/HMENU>))
  let mirror = sheet-mirror(sheet);
  mirror & window-handle(mirror)
end method sheet-parent-window;

define method frame-window-styles
    (frame :: <basic-frame>)
 => (style :: <unsigned-int>, extended-style :: <unsigned-int>)
  let style
    = %logior($WS-OVERLAPPEDWINDOW,
	      if (frame-iconified?(frame)) $WS-ICONIC else 0 end,
	      if (frame-maximized?(frame)) $WS-MAXIMIZE else 0 end);
  let extended-style
    = %logior($WS-EX-CONTROLPARENT,
	      if (frame-always-on-top?(frame)) $WS-EX-TOPMOST else 0 end);
  values(style, extended-style)
end method frame-window-styles;

define sealed method make-top-level-window
    (frame :: <basic-frame>, sheet :: <win32-top-level-sheet>,
     resource-id :: <resource-id>)
 => (handle :: <HWND>, resource :: <window-resource>,
     mirror-class :: <class>, mirror-initargs)
  let _port = port(sheet);
  let resource :: <top-window-resource>
    = lookup-resource($RT-DIALOG, resource-id);
  //---*** Need to do some error handling here
  let handle :: <HWND>
    = CreateDialog(application-instance-handle(),
		   encode-resource(resource-id),
		   $NULL-HWND,
		   Null-Proc);
  check-result("CreateDialog (top-level)", handle);
  // Set the default font
  let font-name  = as(<byte-string>, dialog-font-name(resource));
  let font-size  = dialog-font-size(resource);
  let text-style = make-text-style-from-name-and-size(_port, font-name, font-size);
  when (text-style)
    default-text-style(frame) := text-style
  end;
  // Set up the geometry
  let (x, y)          = window-position(resource);
  let (width, height) = window-size(resource);
  let (x, y)          = win32-dialog-units->pixels(_port, x, y);
  let (width, height) = win32-dialog-units->pixels(_port, width, height);
  duim-debug-message("Frame geometry %= from resource: %d x %d at %d, %d",
		     frame, width, height, x, y);
  set-frame-position(frame, x, y);
  set-frame-size(frame, width, height);
  initialize-sheet-geometry(sheet, x, y, width, height);
  values(handle, resource, <top-level-mirror>, #[])
end method make-top-level-window;

define sealed method make-top-level-window
    (frame :: <basic-frame>, sheet :: <win32-top-level-sheet>,
     resource-id == #f)
 => (handle :: <HWND>, resource :: singleton(#f),
     mirror-class :: <class>, mirror-initargs)
  let (left, top, right, bottom) = sheet-native-edges(sheet);
  let title = frame-title(frame) | "DUIM Window";
  let x = frame-geometry(frame)[0];
  let y = frame-geometry(frame)[1];
  let width = frame-geometry(frame)[2];
  let height = frame-geometry(frame)[3];
  let (style, extended-style) = frame-window-styles(frame);
  //--- Call compute-default-foreground/background/text-style to
  //--- figure out what characteristics the mirror should have
  let handle :: <HWND>
    = CreateWindowEx
        (extended-style,
	 $window-class-name,		// See RegisterClass call
	 title,				// Text for window title bar
	 style,				// Style for a normal top-level window
	 x | $CW-USEDEFAULT,		// x position
	 y | $CW-USEDEFAULT,		// y position
	 width | right - left,		// width
	 height | bottom - top,		// height
	 $NULL-HWND,			// No parent
	 $null-hMenu,			// Use the window class menu
	 application-instance-handle(),
	 $NULL-VOID);			// No data in our WM_CREATE
  check-result("CreateWindow (top-level)", handle);
  values(handle, #f, <top-level-mirror>, #[])
end method make-top-level-window;

define sealed method make-top-level-window
    (frame :: <basic-frame>,	//--- should be <basic-embedded-frame>,
     sheet :: <win32-embedded-top-level-sheet>,
     resource-id :: <resource-id>)
 => (handle :: <HWND>, resource :: singleton(#f),
     mirror-class :: <class>, mirror-initargs)
  ignore(resource-id);
  make-top-level-embedded-window(frame, sheet)
end method make-top-level-window;

define sealed method make-top-level-window
    (frame :: <basic-frame>,	//--- should be <basic-embedded-frame>,
     sheet :: <win32-embedded-top-level-sheet>,
     resource-id == #f)
 => (handle :: <HWND>, resource :: singleton(#f),
     mirror-class :: <class>, mirror-initargs)
  make-top-level-embedded-window(frame, sheet)
end method make-top-level-window;

define method make-top-level-embedded-window
    (frame :: <basic-frame>, // <basic-embedded-frame>,
     sheet :: <win32-embedded-top-level-sheet>)
  // Like 'make-top-level-window', except:
  // (1) Don't call 'frame-window-styles(frame)', just assume:
  //       style = $WS-CHILD + $WS-TABSTOP, no border or title bar
  //     & extended-style = $WS-EX-PARENTNOTIFY
  //     & hence just use $null-string rather than frame-title for title.
  // (2) Don't use 'frame-geometry(frame)[3,4]' for width/height, act as if
  //     both returned #f.
  // (3) Use 'sheet-parent-window' for parent if present, instead of $NULL-HWND.
  let (left, top, right, bottom) = sheet-native-edges(sheet);
  let x = frame-geometry(frame)[0];
  let y = frame-geometry(frame)[1];
  //--- Call compute-default-foreground/background/text-style to
  //--- figure out what characteristics the mirror should have
  let handle :: <HWND>
    = CreateWindowEx
        ($WS-EX-NOPARENTNOTIFY,
	 $window-class-name,		// See RegisterClass call
	 $null-string,			// no title
	 %logior($WS-CHILD, $WS-TABSTOP),// no border or title bar
	 x | $CW-USEDEFAULT,		// x position
	 y | $CW-USEDEFAULT,		// y position
	 right - left,			// width
	 bottom - top,			// height
	 sheet-parent-window(sheet) | $NULL-HWND, // parent, if known yet
	 $null-hMenu,			// no menu
	 application-instance-handle(),
	 $NULL-VOID);			// No data in our WM_CREATE
  check-result("CreateWindow (embedded top-level)", handle);
  values(handle, #f, <top-level-mirror>, #[])
end make-top-level-embedded-window;


define locked variable *first-show-window?* :: <boolean> = #t;

define sealed method map-mirror
    (_port :: <win32-port>,
     sheet :: <win32-top-level-sheet-mixin>, mirror :: <top-level-mirror>) => ()
  local method show-window (handle :: <HWND>) => (was-visible? :: <boolean>)
	  //--- This should maybe be done with 'conditional-update'
	  if (*first-show-window?*)
	    *first-show-window?* := #f;
	    ShowWindow(handle, application-show-window())
	  else
	    ShowWindow(handle, $SW-SHOW)
	  end
	end method;
  let frame = sheet-frame(sheet);
  let handle :: <HWND> = window-handle(mirror);
  show-window(handle);
  unless (frame-input-focus(frame))
    let layout = frame-layout(frame);
    let focus  = layout & find-child-for-focus(layout);
    when (focus)
      duim-debug-message("'map-mirror' setting frame focus to %=", focus);
      frame-input-focus(frame) := focus
    end
  end;
  UpdateWindow(handle)	// Sends WM_PAINT message and returns status
end method map-mirror;

define sealed method raise-mirror 
    (_port :: <win32-port>,
     sheet :: <win32-top-level-sheet-mixin>, mirror :: <top-level-mirror>,
     #key activate? = #t) => ()
  let handle :: <HWND> = window-handle(mirror);
  if (activate?)
    unless (handle = GetForegroundWindow())
      duim-debug-message("Raising top level window %=", sheet);
      check-result("SetForegroundWindow", SetForegroundWindow(handle))
    end
  else
    check-result("SetWindowPos ($HWND-TOP)",
		 SetWindowPos(handle, $HWND-TOP, 0, 0, 0, 0,
			      %logior($SWP-NOMOVE, $SWP-NOSIZE, $SWP-NOACTIVATE)))
  end
end method raise-mirror;

define sealed method set-mirror-edges
    (_port :: <win32-port>,
     sheet :: <win32-top-level-sheet-mixin>, mirror :: <top-level-mirror>,
     left  :: <integer>, top    :: <integer>,
     right :: <integer>, bottom :: <integer>) => ()
  let handle :: <HWND> = window-handle(mirror);
  let frame = sheet-frame(sheet);
  let (old-left, old-top, old-right, old-bottom) = box-edges(mirror.%region);
  let old-width  = old-right  - old-left;
  let old-height = old-bottom - old-top;
  let width  = right  - left;
  let height = bottom - top;
  mirror.%region := set-box-edges(mirror.%region, left, top, right, bottom);
  duim-debug-message("Setting top level geometry for %= to %d, %d, %d x %d",
		     sheet, left, top, width, height);
  duim-debug-message("  [old geometry was %d, %d, %d x %d]",
		     old-left, old-top, old-width, old-height);
  let same-position? = (left  = old-left)  & (right  = old-right);
  let same-size?     = (width = old-width) & (height = old-height);
  let flags = %logior($SWP-NOACTIVATE, $SWP-NOZORDER,
		      if (same-position?) $SWP-NOMOVE else 0 end,
		      if (same-size?)     $SWP-NOSIZE else 0 end);
  check-result("SetWindowPos",
	       SetWindowPos(handle, $NULL-HWND,
			    left, top, width, height, flags))
end method set-mirror-edges;

define sealed method destroy-mirror 
    (_port :: <win32-port>,
     sheet :: <win32-top-level-sheet-mixin>, mirror :: <top-level-mirror>) => ()
  destroy-accelerator-table(sheet);
  destroy-tooltip(mirror);
  next-method()
end method destroy-mirror;

define method note-win32-frame-destroyed
    (frame :: <simple-frame>) => ()
  //--- This test needs to be stronger!  We shouldn't post a quit message
  //--- for any frame that gets started from within a thread, if that thread
  //--- can ever be used to start another frame
  unless (frame-owner(frame))
    duim-debug-message("Quitting frame %=", frame);
    PostQuitMessage(0)
  end
end method note-win32-frame-destroyed;

define method note-win32-frame-destroyed
    (frame :: <container-frame>) => ()
  when (container-uses-own-thread?(frame))
    duim-debug-message("Quitting container frame %=", frame);
    PostQuitMessage(0)
  end
end method note-win32-frame-destroyed;


/// Top level layout

define class <top-level-layout> (<layout-pane>)
  sealed slot top-level-client-layout,
    init-keyword: client-layout:;
end class <top-level-layout>;

define sealed method do-compose-space
    (layout :: <top-level-layout>, #key width, height)
 => (space-req :: <space-requirement>)
  let frame = sheet-frame(layout);
  let menu-width = frame-menu-bar-size(frame);
  let client-layout = top-level-client-layout(layout);
  let (extra-width, extra-height) = window-frame-extra-size(frame);
  if (client-layout)
    let client-width  = width  & (width  - extra-width);
    let client-height = height & (height - extra-height);
    let child-space
      = compose-space(client-layout, 
		      width: client-width, height: client-height);
    let (w, w-, w+, h, h-, h+)
      = space-requirement-components(client-layout, child-space);
    let best-width  = max(w,  menu-width) + extra-width;
    let min-width   = max(w-, menu-width) + extra-width;
    let max-width   = max(w+, menu-width) + extra-width;
    let best-height = h  + extra-height;
    let min-height  = h- + extra-height;
    let max-height  = h+ + extra-height;
    make(<space-requirement>,
	 width:  best-width,  min-width:  min-width,  max-width:  max-width,
	 height: best-height, min-height: min-height, max-height: max-height)
  else
    let min-width   = extra-width;
    let min-height  = extra-height;
    let best-width  = max(width  | 0, min-width);
    let best-height = max(height | 0, min-height);
    make(<space-requirement>,
	 width:  best-width,  min-width: min-width, max-width: $fill,
	 height: best-height, min-height: min-height, max-height: $fill)
  end
end method do-compose-space;

define sealed method do-allocate-space
    (layout :: <top-level-layout>, width :: <integer>, height :: <integer>) => ()
  let frame = sheet-frame(layout);
  let (extra-width, extra-height) = window-frame-extra-size(frame);
  let client-layout = top-level-client-layout(layout);
  set-sheet-edges(client-layout,
		  0, 0, 
		  width - extra-width, height - extra-height)
end method do-allocate-space;

define sealed method frame-menu-bar-size
    (frame :: <basic-frame>)
 => (width  :: <integer>, height  :: <integer>)
  let menu-bar = frame-menu-bar(frame);
  // Menu bars aren't part of the DUIM sheet hierarchy in Windows,
  // so we have to call 'compose-space' ourselves to measure it
  if (menu-bar)
    let space-req = compose-space(menu-bar);
    let (w, w-, w+, h, h-, h+) 
      = space-requirement-components(menu-bar, space-req);
    ignore(w-, w+, h-, h+);
    values(w, h)
  else
    values(0, 0)
  end
end method frame-menu-bar-size;

define method frame-client-area-offset
    (frame :: <basic-frame>)
 => (x :: <integer>, y :: <integer>)
  let (x, y)          = values(100, 100);
  let (width, height) = frame-size(frame);
  let width  = width  | $default-sheet-size;
  let height = height | $default-sheet-size;
  frame-non-client-geometry(frame, x, y, x + width, y + height)
end method frame-client-area-offset;

define method window-frame-extra-size
    (frame :: <basic-frame>)
 => (width :: <integer>, height :: <integer>)
  let (x, y)          = values(100, 100);
  let (width, height) = frame-size(frame);
  let width  = width  | $default-sheet-size;
  let height = height | $default-sheet-size;
  let (left, top, right, bottom)
    = client->frame-edges(frame, x, y, x + width, y + height);
  values(right - left - width, bottom - top - height)
end method window-frame-extra-size;

define method frame-non-client-geometry
    (frame :: <basic-frame>, 
     left  :: <integer>, top    :: <integer>,
     right :: <integer>, bottom :: <integer>)
 => (x :: <integer>, y :: <integer>, width :: <integer>, height :: <integer>)
  let (width, height) = values(right - left, bottom - top);
  let (l, t, r, b) = client->frame-edges(frame, left, top, right, bottom);
  let (w, h) = values(r - l, b - t);
  let x-offset     = left   - l;
  let y-offset     = top    - t;
  let extra-width  = width  - w;
  let extra-height = height - h;
  values(x-offset, y-offset, extra-width, extra-height)
end method frame-non-client-geometry;

define method client->frame-edges
    (frame :: <basic-frame>, 
     left  :: <integer>, top    :: <integer>,
     right :: <integer>, bottom :: <integer>)
 => (l :: <integer>, t :: <integer>, r :: <integer>, b :: <integer>)
  with-stack-structure (rect :: <LPRECT>)
    rect.left-value   := left;
    rect.top-value    := top;
    rect.right-value  := right;
    rect.bottom-value := bottom;
    let menu-bar? = frame-menu-bar(frame) & #t;
    let (style, extended-style) = frame-window-styles(frame);
    check-result("AdjustWindowRectEx",
		 AdjustWindowRectEx(rect, style, menu-bar?, extended-style));
    let frame-left   = rect.left-value;
    let frame-top    = rect.top-value;
    let frame-right  = rect.right-value;
    let frame-bottom = rect.bottom-value;
    duim-debug-message
      ("Adjusted [%=,%=,%=,%=] => [%=,%=,%=,%=]: diff [%=,%=,%=,%=]",
       left, top, right, bottom,
       frame-left, frame-top, frame-right, frame-bottom,
       frame-left - left, frame-top - top,
       frame-right - right, frame-bottom - bottom);
    values(frame-left, frame-top, frame-right, frame-bottom)
  end
end method client->frame-edges;


/// Frame wrapper

define method frame-wrapper
    (framem :: <win32-frame-manager>, frame :: <simple-frame>,
     layout :: false-or(<sheet>))
 => (wrapper :: false-or(<sheet>))
  let menu-bar      = frame-menu-bar(frame);
  let client-layout = make-client-layout(framem, frame, layout);
  make(<top-level-layout>,
       client-layout: client-layout,
       children: make-children(menu-bar, client-layout))
end method frame-wrapper;

define sealed method update-frame-wrapper
    (framem :: <win32-frame-manager>, frame :: <simple-frame>) => ()
  let top-sheet = top-level-sheet(frame);
  when (top-sheet)
    let top-layout :: false-or(<top-level-layout>) = sheet-child(top-sheet);
    if (top-layout)
      let menu-bar = frame-menu-bar(frame);
      let children = sheet-children(top-layout);
      let client-layout = ~empty?(children) & children[size(children) - 1];
      let client-layout
	= if (client-layout)
	    update-client-layout(framem, frame, client-layout)
	  else
	    make-client-layout(framem, frame, frame-layout(frame))
	  end;
      let new-children = make-children(menu-bar, client-layout);
      unless (new-children = sheet-children(top-layout))
	sheet-children(top-layout) := new-children;
	if (sheet-mapped?(top-layout))
	  if (client-layout)
	    sheet-mapped?(client-layout) := #t;
	    relayout-parent(client-layout)
	  end
	end
      end
    else
      let wrapper = frame-wrapper(framem, frame, frame-layout(frame));
      sheet-child(top-sheet) := wrapper;
      relayout-parent(wrapper);
      if (sheet-mapped?(top-sheet))
	sheet-mapped?(wrapper, clear?: #t, repaint?: #t) := #t
      end
    end
  end
end method update-frame-wrapper;

define sealed method make-client-layout
    (framem :: <win32-frame-manager>, frame :: <simple-frame>,
     layout :: false-or(<sheet>))
 => (client-layout :: <sheet>)
  let tool-bar   = frame-tool-bar(frame);
  let status-bar = frame-status-bar(frame);
  with-frame-manager (framem)
    let indented-children
      = make-children(tool-bar & tool-bar-decoration(tool-bar), layout);
    let indented-children-layout
      = unless (empty?(indented-children))
	  with-spacing (spacing: win32-dialog-x-pixels(framem, 1))
	    make(<column-layout>,
		 children: indented-children,
		 y-spacing: $top-level-y-spacing)
          end
        end;
    make(<column-layout>,
	 children: make-children(indented-children-layout, status-bar),
	 y-spacing: $top-level-y-spacing)
  end
end method make-client-layout;

define sealed method update-client-layout
    (framem :: <win32-frame-manager>, frame :: <simple-frame>,
     client-layout :: <column-layout>) => ()
  let tool-bar   = frame-tool-bar(frame);
  let layout     = frame-layout(frame);
  let status-bar = frame-status-bar(frame);
  with-frame-manager (framem)
    let old-children = sheet-children(client-layout);
    let old-first-child = ~empty?(old-children) & old-children[0];
    let old-layout = instance?(old-first-child, <spacing>) & old-first-child;
    let indented-children
      = make-children(tool-bar & tool-bar-decoration(tool-bar), layout);
    let new-layout
      = if (~empty?(indented-children))
	  if (old-layout)
	    let column-layout = sheet-child(old-layout);
	    update-sheet-children(column-layout, indented-children)
	  else
	    with-spacing (spacing: win32-dialog-x-pixels(framem, 1))
	      make(<column-layout>,
		   children: indented-children,
		   y-spacing: $top-level-y-spacing)
	    end
	  end
	end;
    let new-children = make-children(new-layout, status-bar);
    update-sheet-children(client-layout, new-children)
  end
end method update-client-layout;

define function make-children
    (#rest maybe-children)
 => (children :: <sequence>)
  let children :: <stretchy-object-vector> = make(<stretchy-vector>);
  for (child in maybe-children)
    child & add!(children, child)
  end;
  children
end function make-children;

define function update-sheet-children
    (sheet :: <sheet>, children :: <sequence>) => ()
  unless (children = sheet-children(sheet))
    sheet-children(sheet) := children;
    if (sheet-mapped?(sheet))
      relayout-parent(sheet);
      for (child in sheet-children(sheet))
	sheet-mapped?(child) := #t
      end
    end
  end
end function update-sheet-children;

define method update-frame-layout
    (framem :: <win32-frame-manager>, frame :: <simple-frame>) => ()
  update-frame-wrapper(framem, frame)
end method update-frame-layout;


/// Focus handling

// We have yet to find any way in Windows to be informed as children take
// and lose the focus, so we just poll the focus on a timer.

define constant $use-focus-timer          :: <boolean> = #t;
define constant $focus-timer-id           :: <integer> = 100;
define constant $focus-updates-per-second :: <integer> = 20;

define method note-sheet-activated
    (sheet :: <win32-top-level-sheet-mixin>) => ()
  let frame = sheet-frame(sheet);
  let focus = frame & frame-input-focus(frame);
  let _port = frame & port(frame);
  when (_port)
    port-input-focus(_port) := focus;
    $use-focus-timer & install-focus-timer(sheet);
    distribute-event(_port, make(<frame-focus-in-event>, frame: frame))
  end
end method note-sheet-activated;

define method note-sheet-deactivated
    (sheet :: <win32-top-level-sheet-mixin>) => ()
  let frame = sheet-frame(sheet);
  let _port = frame & port(frame);
  $use-focus-timer & uninstall-focus-timer(sheet);
  when (_port)
    distribute-event(_port, make(<frame-focus-out-event>, frame: frame))
  end
end method note-sheet-deactivated;

define sealed method install-focus-timer
    (sheet :: <top-level-sheet>) => ()
  let handle  = window-handle(sheet);
  let timeout = floor/(1000, $focus-updates-per-second);
  if (zero?(SetTimer(handle, $focus-timer-id, timeout, Timer-Proc)))
    report-error("SetTimer")
  end
end method install-focus-timer;

define sealed method uninstall-focus-timer
    (sheet :: <top-level-sheet>) => ()
  let handle = window-handle(sheet);
  check-result("KillTimer", KillTimer(handle, $focus-timer-id))
end method uninstall-focus-timer;

define sealed method handle-timer
    (handle :: <HWND>, message, event, time) => ()
  #f
end method handle-timer;

define sealed method handle-timer
    (handle :: <HWND>, message, event == $focus-timer-id, time) => ()
  let top-sheet :: <top-level-sheet> = handle-sheet(handle);
  maybe-update-focus(port(top-sheet))
end method handle-timer;

define callback Timer-Proc :: <TIMERPROC> = handle-timer;


/// Geometry updating

define sealed method handle-move
    (sheet :: <top-level-sheet>, mirror :: <top-level-mirror>,
     x :: <integer>, y :: <integer>)
 => (handled? :: <boolean>)
  let (old-x, old-y) = sheet-position(sheet);
  unless (x = old-x & y = old-y)
    let frame = sheet-frame(sheet);
    let (x-offset, y-offset) = frame-client-area-offset(frame);
    let x :: <integer> = x - x-offset;
    let y :: <integer> = y - y-offset;
    duim-debug-message("Sheet %= moved to %=, %= (from %=, %=)",
		       sheet, x, y, old-x, old-y);
    set-sheet-position(sheet, x, y)
  end;
  #t
end method handle-move;

define sealed method handle-resize
    (sheet :: <top-level-sheet>, mirror :: <top-level-mirror>,
     client-width :: <integer>, client-height :: <integer>)
 => (handled? :: <boolean>)
  let frame = sheet-frame(sheet);
  let (left, top, right, bottom) 
    = client->frame-edges(frame, 0, 0, client-width, client-height);
  let (width, height) = values(right - left, bottom - top);
  let (left, top) = box-position(mirror.%region);
  let region = make-bounding-box(left, top, left + width, top + height);
  //--- This hack is to avoid doing anything on the first WM_SIZE
  //--- which comes in before the children are attached.  Can we
  //--- actually do something with the event?
  if (sheet-mapped?(sheet))
    let (old-width, old-height) = box-size(sheet-region(sheet));
    duim-debug-message("Resizing %= to %dx%d -- was %dx%d",
		       sheet, width, height, old-width, old-height);
    distribute-event(port(sheet),
		     make(<window-configuration-event>,
			  sheet: sheet,
			  region: region))
  else
    duim-debug-message("Ignoring WM_SIZE event for %= to size %dx%d",
		       sheet, width, height)
  end;
  #t
end method handle-resize;


/// Accelerator handling

define function make-keyboard-gesture
    (keysym :: <symbol>, #rest modifiers)
 => (gesture :: <keyboard-gesture>)
  make(<keyboard-gesture>, keysym: keysym, modifiers: modifiers)
end function make-keyboard-gesture;

define function gesture-modifiers
    (gesture :: <keyboard-gesture>)
 => (shift? :: <boolean>, control? :: <boolean>, alt? :: <boolean>)
  let modifier-state = gesture-modifier-state(gesture);
  values(~zero?(logand(modifier-state, $shift-key)),
	 ~zero?(logand(modifier-state, $control-key)),
	 ~zero?(logand(modifier-state, $alt-key)))
end function gesture-modifiers;

define table $accelerator-table :: <object-table>
  = { // This is the set defined by WIG, Appendix B, Table B.2, page 438
      #"Copy"        => make-keyboard-gesture(#"c", #"control"),
      #"Cut"         => make-keyboard-gesture(#"x", #"control"),
      #"Help"        => make-keyboard-gesture(#"f1"),
      #"Open"        => make-keyboard-gesture(#"o", #"control"),
      #"Open..."     => make-keyboard-gesture(#"o", #"control"),
      #"Paste"       => make-keyboard-gesture(#"v", #"control"),
      #"Print"       => make-keyboard-gesture(#"p", #"control"),
      #"Print..."    => make-keyboard-gesture(#"p", #"control"),
      #"Save"        => make-keyboard-gesture(#"s", #"control"),
      #"Undo"        => make-keyboard-gesture(#"z", #"control"),

      // The same set with the mnemonics already in (a bit of a hack!)
      #"&Copy"       => make-keyboard-gesture(#"c", #"control"),
      #"Cu&t"        => make-keyboard-gesture(#"x", #"control"),
      #"&Help"       => make-keyboard-gesture(#"f1"),
      #"&Open"       => make-keyboard-gesture(#"o", #"control"),
      #"&Open..."    => make-keyboard-gesture(#"o", #"control"),
      #"&Paste"      => make-keyboard-gesture(#"v", #"control"),
      #"&Print"      => make-keyboard-gesture(#"p", #"control"),
      #"&Print..."   => make-keyboard-gesture(#"p", #"control"),
      #"&Save"       => make-keyboard-gesture(#"s", #"control"),
      #"&Undo"       => make-keyboard-gesture(#"z", #"control"),

      // Some extras that seemed to be missing
      #"Delete"      => make-keyboard-gesture(#"delete"),
      #"Find"        => make-keyboard-gesture(#"f", #"control"),
      #"Find..."     => make-keyboard-gesture(#"f", #"control"),
      #"New"         => make-keyboard-gesture(#"n", #"control"),
      #"New..."      => make-keyboard-gesture(#"n", #"control"),
      #"Redo"        => make-keyboard-gesture(#"y", #"control"),
      #"Select All"  => make-keyboard-gesture(#"a", #"control"),

      // The same set with the mnemonics already in (a bit of a hack!)
      #"&Delete"     => make-keyboard-gesture(#"delete"),
      #"&Find"       => make-keyboard-gesture(#"f", #"control"),
      #"&Find..."    => make-keyboard-gesture(#"f", #"control"),
      #"&New"        => make-keyboard-gesture(#"n", #"control"),
      #"&New..."     => make-keyboard-gesture(#"n", #"control"),
      #"&Redo"       => make-keyboard-gesture(#"y", #"control"),
      #"&Select All" => make-keyboard-gesture(#"a", #"control")
      };

define sealed method defaulted-gadget-accelerator
    (framem :: <win32-frame-manager>, gadget :: <accelerator-mixin>)
 => (accelerator :: false-or(<accelerator>))
  let accelerator = gadget-accelerator(gadget);
  if (unsupplied?(accelerator))
    let label = gadget-label(gadget);
    let key   = instance?(label, <string>) & as(<symbol>, label);
    element($accelerator-table, key, default: #f)
  else
    accelerator
  end
end method defaulted-gadget-accelerator;


define sealed method gadget-label-postfix
    (gadget :: <win32-gadget-mixin>) => (label :: <string>)
  ""
end method gadget-label-postfix;

define sealed method gadget-label-postfix
    (gadget :: <accelerator-mixin>) => (label :: <string>)
  let framem  = frame-manager(gadget);
  let gesture = defaulted-gadget-accelerator(framem, gadget);
  if (gesture)
    let keysym = gesture-keysym(gesture);
    let (shift?, control?, alt?) = gesture-modifiers(gesture);
    concatenate-as(<string>, 
		   "\t",
		   if (control?) "Ctrl+"  else "" end,
		   if (alt?)     "Alt+"   else "" end,
		   if (shift?)   "Shift+" else "" end,
		   keysym->key-name(keysym))
  else
    ""
  end
end method gadget-label-postfix;

// Map keysyms to their labels on a typical keyboard
define table $keysym->key-name :: <object-table>
  = { #"return"     => "Enter",
      #"newline"    => "Shift+Enter",
      #"linefeed"   => "Line Feed",
      #"up"	    => "Up Arrow",
      #"down"	    => "Down Arrow",
      #"left"	    => "Left Arrow",
      #"right"	    => "Right Arrow",
      #"prior"	    => "Page Up",
      #"next"	    => "Page Down",
      #"lwin"	    => "Left Windows",
      #"rwin"	    => "Right Windows",
      #"numpad0"    => "Num 0",
      #"numpad1"    => "Num 1",
      #"numpad2"    => "Num 2",
      #"numpad3"    => "Num 3",
      #"numpad4"    => "Num 4",
      #"numpad5"    => "Num 5",
      #"numpad6"    => "Num 6",
      #"numpad7"    => "Num 7",
      #"numpad8"    => "Num 8",
      #"numpad9"    => "Num 9",
      #"num-lock"   => "Num Lock",
      #"caps-lock"  => "Caps Lock" };

define function keysym->key-name
    (keysym) => (name :: <string>)
  element($keysym->key-name, keysym, default: #f)
  | string-capitalize(as(<string>, keysym | ""))
end function keysym->key-name;


define sealed method accelerator-table
    (sheet :: <top-level-sheet>) => (accelerators :: false-or(<HACCEL>))
  let mirror = sheet-direct-mirror(sheet);
  // Ensure that we don't build the accelerator table too early (i.e.,
  // before all of the resource ids have been created).  This isn't as bad
  // as it seems, since users won't have been able to use an accelerator
  // before the top-level sheet is mapped anyway...
  when (sheet-mapped?(sheet))
    mirror.%accelerator-table
    | (mirror.%accelerator-table := make-accelerator-table(sheet))
  end
end method accelerator-table;

// Useful trampoline...
define sealed method accelerator-table
    (sheet :: <sheet>) => (accelerators :: false-or(<HACCEL>))
  let top-sheet = top-level-sheet(sheet);
  top-sheet & accelerator-table(top-sheet)
end method accelerator-table;

define method make-accelerator-table
    (sheet :: <top-level-sheet>) => (accelerators :: <HACCEL>)
  local method fill-accelerator-entry
	    (gadget :: <accelerator-mixin>, accelerator :: <accelerator>,
	     entry :: <LPACCEL>) => ()
	  let keysym    = gesture-keysym(accelerator);
	  let modifiers = gesture-modifier-state(accelerator);
	  let char      = gesture-character(accelerator);
	  let (vkey :: <integer>, fVirt :: <integer>)
	    = if (char
		  & zero?(logand(modifiers, logior($control-key, $meta-key)))
		  & character->virtual-key(char))
		values(character->virtual-key(char), 0)
	      else
		let vkey = keysym->virtual-key(keysym);
		if (vkey)
		  values(vkey,
			 logior($ACCEL-FVIRTKEY,
				if (zero?(logand(modifiers, $shift-key)))   0 else $ACCEL-FSHIFT end,
				if (zero?(logand(modifiers, $control-key))) 0 else $ACCEL-FCONTROL end,
				if (zero?(logand(modifiers, $alt-key)))     0 else $ACCEL-FALT end))
		else
		  error("Can't decode the gesture with keysym %=, modifiers #o%o",
			keysym, modifiers)
		end
	      end;
	  let cmd :: <integer>
	    = sheet-resource-id(gadget) | gadget->id(gadget);
	  entry.fVirt-value := fVirt;
	  entry.key-value   := vkey;
	  entry.cmd-value   := cmd;
	end method;
  let accelerators   = frame-accelerators(sheet-frame(sheet));
  let n :: <integer> = size(accelerators);
  if (n > 0)
    with-stack-structure (entries :: <LPACCEL>, element-count: n)
      for (i :: <integer> from 0 below n)
	let entry  = accelerators[i];
	let gadget = entry[0];
	let accel  = entry[1];
	let entry  = pointer-value-address(entries, index: i);
	fill-accelerator-entry(gadget, accel, entry)
      end;
      check-result("CreateAcceleratorTable", CreateAcceleratorTable(entries, n))
    end
  else
    $null-HACCEL
  end
end method make-accelerator-table;

define sealed method destroy-accelerator-table
    (sheet :: <top-level-sheet>) => ()
  let accelerator-table = accelerator-table(sheet);
  when (accelerator-table & ~null-handle?(accelerator-table))
    DestroyAcceleratorTable(accelerator-table)
  end;
  let mirror = sheet-direct-mirror(sheet);
  mirror.%accelerator-table := #f
end method destroy-accelerator-table;

define method note-accelerators-changed
    (framem :: <win32-frame-manager>, frame :: <basic-frame>) => ()
  // Force the accelerators to be recomputed
  let top-sheet = top-level-sheet(frame);
  when (top-sheet)
    destroy-accelerator-table(top-sheet)
  end
end method note-accelerators-changed;


/// Tooltip handling

define sealed method make-frame-tooltip
    (frame :: <simple-frame>, mirror :: <top-level-mirror>)
 => (tooltip :: <HWND>)
  let handle = window-handle(mirror);
  initialize-common-controls(port(frame));
  let tooltip
    = CreateWindowEx
        (0,
	 $TOOLTIPS-CLASS,
	 $NULL-string,
	 %logior($TTS-NOPREFIX, $TTS-ALWAYSTIP),
	 $CW-USEDEFAULT,		// x position
	 $CW-USEDEFAULT,		// y position
	 $CW-USEDEFAULT,		// width
	 $CW-USEDEFAULT,		// height
	 handle,			// top-level sheet is the parent
	 $null-hMenu,			// no menu
	 application-instance-handle(),
	 $NULL-VOID);			// No data in our WM_CREATE
  check-result("CreateWindow (TOOLTIP)", tooltip);
  mirror.%tool-tip := tooltip
end method make-frame-tooltip;

define sealed method destroy-tooltip
    (mirror :: <top-level-mirror>) => ()
  let tooltip = mirror.%tool-tip;
  when (tooltip)
    unless (null-handle?(tooltip))
      DestroyWindow(tooltip)	// error-checking probably won't buy anything...
    end
  end
end method destroy-tooltip;

//--- We could support tooltips for unmirrored sheets by using $LPSTR-TEXTCALLBACK
//--- and handling the $TTN-NEEDTEXT notification messages
define sealed method register-tooltip-for-sheet
    (sheet :: <mirrored-sheet-mixin>, documentation :: <string>) => ()
  let top-mirror = top-level-mirror(sheet);
  let tooltip    = top-mirror & top-mirror.%tool-tip;
  let handle     = window-handle(sheet);
  when (tooltip)
    // Register a tool tip for this gadget
    with-c-string (c-string = documentation)
      with-stack-structure (ti :: <LPTOOLINFOA>)
	ti.cbSize-value   := safe-size-of(<TOOLINFOA>);
	ti.uFlags-value   := %logior($TTF-IDISHWND, $TTF-SUBCLASS);
	ti.hWnd-value     := handle;
	ti.uId-value      := pointer-address(handle);
	ti.lpszText-value := c-string;
        SendMessage(tooltip, $TTM-ADDTOOL, 0, pointer-address(ti))
      end
    end
  end
end method register-tooltip-for-sheet;

define sealed method unregister-tooltip-for-sheet
    (sheet :: <mirrored-sheet-mixin>) => ()
  let top-mirror = top-level-mirror(sheet);
  let tooltip    = top-mirror & top-mirror.%tool-tip;
  let handle     = window-handle(sheet);
  when (tooltip)
    with-stack-structure (ti :: <LPTOOLINFOA>)
      ti.cbSize-value   := safe-size-of(<TOOLINFOA>);
      ti.uFlags-value   := %logior($TTF-IDISHWND, $TTF-SUBCLASS);
      ti.hWnd-value     := handle;
      ti.uId-value      := pointer-address(handle);
      SendMessage(tooltip, $TTM-DELTOOL, 0, pointer-address(ti))
    end
  end
end method unregister-tooltip-for-sheet;


/// Keyboard interrupt (i.e., hot key) handling

define sealed method register-keyboard-interrupt-handler
    (_port :: <win32-port>, sheet :: <win32-top-level-sheet-mixin>) => ()
  let id = gethash(_port.%hot-keys, sheet);
  unless (id)
    with-port-locked (_port)
      let handle = window-handle(sheet);
      when (handle)
	// Compute a hot key id that isn't currently in use
	let id = block (return)
		   for (id :: <integer> from 0)
		     unless (member?(id, _port.%hot-keys))
		       return(id)
		     end
		   end
		 end;
	gethash(_port.%hot-keys, sheet) := id;
	// By fiat, the "break" key on Windows is Crtl+Cancel (== Ctrl+Pause)
	//---*** Too bad this registers a system-wide key!
	// RegisterHotKey(handle, id, $MOD-CONTROL, $VK-CANCEL)
      end
    end
  end
end method register-keyboard-interrupt-handler;

define sealed method unregister-keyboard-interrupt-handler
    (_port :: <win32-port>, sheet :: <win32-top-level-sheet-mixin>) => ()
  let id = gethash(_port.%hot-keys, sheet);
  when (id)
    with-port-locked (_port)
      let handle = window-handle(sheet);
      when (handle)
	//---*** Fix this when we actually call 'RegisterHotKey'
	// UnregisterHotKey(handle, id)
      end
    end
  end
end method unregister-keyboard-interrupt-handler;
