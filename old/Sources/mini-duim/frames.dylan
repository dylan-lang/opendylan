Module:    mini-duim
Synopsis:  Mini-DUIM frames
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Frames

define open abstract primary class <frame> (<object>)
  slot top-level-sheet :: false-or(<sheet>) = #f;
  slot %title :: false-or(<string>) = #f,
    init-keyword: title:;
  slot %mapped? :: <boolean> = #f;
  slot frame-geometry = vector(#f, #f, #f, #f),	// x, y, width, height
    init-keyword: geometry:;
  slot %layout :: false-or(<sheet>) = #f,
    init-keyword: layout:;
  slot %menu-bar :: false-or(<sheet>) = #f,
    init-keyword: menu-bar:;
  slot %tool-bar :: false-or(<sheet>) = #f,
    init-keyword: tool-bar:;
  slot %status-bar :: false-or(<sheet>) = #f,
    init-keyword: status-bar:;
  slot frame-manager :: false-or(<frame-manager>) = #f,
    init-keyword: frame-manager:;
  slot %container = #f,
    init-keyword: container:;
  slot %container-region = #f,
    init-keyword: container-region:;
  slot frame-state = #"detached";
  slot frame-owner :: false-or(<frame>) = #f,
    init-keyword: frame-owner:;
end class <frame>;

define open class <simple-frame> (<frame>)
end class <simple-frame>;

define method initialize
    (frame :: <frame>, #key x, y, width, height)
  next-method();
  // Save the requested geometry
  let geometry = frame-geometry(frame);
  geometry[0] := x;
  geometry[1] := y;
  geometry[2] := width;
  geometry[3] := height;
end method initialize;


define open generic frame-layout
    (frame :: <frame>) => (sheet :: false-or(<sheet>));
define open generic frame-menu-bar
    (frame :: <frame>) => (sheet :: false-or(<sheet>));
define open generic frame-tool-bar
    (frame :: <frame>) => (sheet :: false-or(<sheet>));
define open generic frame-status-bar
    (frame :: <frame>) => (sheet :: false-or(<sheet>));

define method frame-layout 
    (frame :: <frame>) => (layout :: false-or(<sheet>))
  frame.%layout
end method frame-layout;

define method frame-layout-setter 
    (layout :: false-or(<sheet>), frame :: <frame>)
 => (layout :: false-or(<sheet>))
  let old-layout = frame-layout(frame);
  unless (old-layout == layout)
    frame.%layout := layout;
    let framem = frame-manager(frame);
    framem & update-frame-layout(framem, frame)
  end;
  layout
end method frame-layout-setter;

define method frame-menu-bar 
    (frame :: <frame>) => (menu-bar :: false-or(<sheet>))
  frame.%menu-bar
end method frame-menu-bar;

define method frame-tool-bar 
    (frame :: <frame>) => (tool-bar :: false-or(<sheet>))
  frame.%tool-bar
end method frame-tool-bar;

define method frame-status-bar 
    (frame :: <frame>) => (status-bar :: false-or(<sheet>))
  frame.%status-bar
end method frame-status-bar;

define method frame-title
    (frame :: <frame>) => (title :: false-or(<string>))
  frame.%title
end method frame-title;

define method frame-title-setter
    (title :: false-or(<string>), frame :: <frame>) => (title :: false-or(<string>))
  frame.%title := title;
  let framem = frame-manager(frame);
  if (framem)
    note-title-changed(framem, frame)
  end;
  title
end method frame-title-setter;

define method port (frame :: <frame>) => (port :: <port>)
  find-port()
end method port;


/// Defining frames

define macro frame-definer
  { define ?modifiers:* frame ?:name (?superclasses:*) ?slots:* end }
    => { define /* ?modifiers */ frame-class ?name (?superclasses) ?slots end;
         define frame-panes ?name (?superclasses) ?slots end;
         define frame-gadget-bars ?name (?superclasses) ?slots end; }
end macro frame-definer;

define macro frame-class-definer
  { define ?modifiers:* frame-class ?:name (?superclasses:*) ?slots:* end }
    => { define /* ?modifiers */ class ?name (?superclasses) ?slots end }
slots:
  { } => { }
  { ?slot:*; ... } => { ?slot; ... }
 slot:
  { slot ?:variable = ?init:expression, #rest ?options:expression }
    => { slot ?variable = ?init, ?options }
  { slot ?:variable, #rest ?options:expression }
    => { slot ?variable, ?options }
  { pane ?:name (?frame:variable) ?:body }
    => { slot ?name ## "-pane" :: false-or(<sheet>) = #f }
  // The next six feel like the 'exception' clause in 'block', in that
  // they take an argument (the frame) and a body, but no 'end'
  { layout (?frame:variable) ?:body } => { }		// uses %layout slot
  { menu-bar (?frame:variable) ?:body } => { }		// uses %menu-bar slot
  { tool-bar (?frame:variable) ?:body }	=> { }		// uses %tool-bar slot
  { status-bar (?frame:variable) ?:body } => { }	// uses %status-bar slot
end macro frame-class-definer;

define macro frame-panes-definer
  { define frame-panes ?class:name (?superclasses:*) end }
    => { }
  { define frame-panes ?class:name (?superclasses:*) 
      pane ?:name (?frame:variable) ?:body; ?more-slots:*
    end }
    => { define method ?name (?frame :: ?class)
	   ?frame.?name ## "-pane"
	   | (?frame.?name ## "-pane" := begin ?body end)
         end; 
         define frame-panes ?class (?superclasses) ?more-slots end; }
  { define frame-panes ?class:name (?superclasses:*) 
      ?non-pane-slot; ?more-slots:*
    end }
    => { define frame-panes ?class (?superclasses) ?more-slots end; }
end macro frame-panes-definer;

define macro frame-gadget-bars-definer
  { define frame-gadget-bars ?class:name (?superclasses:*) end }
    => { }
  { define frame-gadget-bars ?class:name (?superclasses:*) 
      layout (?frame:variable) ?:body; ?more-slots:*
    end }
    => { define method frame-layout (?frame :: ?class) => (sheet :: false-or(<sheet>))
	   ?frame.%layout
	   | (?frame.%layout := begin ?body end)
         end; 
         define frame-gadget-bars ?class (?superclasses) ?more-slots end; }
  { define frame-gadget-bars ?class:name (?superclasses:*) 
      menu-bar (?frame:variable) ?:body; ?more-slots:*
    end }
    => { define method frame-menu-bar (?frame :: ?class) => (sheet :: false-or(<sheet>))
	   ?frame.%menu-bar
	   | (?frame.%menu-bar := begin ?body end)
         end; 
         define frame-gadget-bars ?class (?superclasses) ?more-slots end; }
  { define frame-gadget-bars ?class:name (?superclasses:*) 
      tool-bar (?frame:variable) ?:body; ?more-slots:*
    end }
    => { define method frame-tool-bar (?frame :: ?class) => (sheet :: false-or(<sheet>))
	   ?frame.%tool-bar
	   | (?frame.%tool-bar := begin ?body end)
         end; 
         define frame-gadget-bars ?class (?superclasses) ?more-slots end; }
  { define frame-gadget-bars ?class:name (?superclasses:*) 
      status-bar (?frame:variable) ?:body; ?more-slots:*
    end }
    => { define method frame-status-bar (?frame :: ?class) => (sheet :: false-or(<sheet>))
	   ?frame.%status-bar
	   | (?frame.%status-bar := begin ?body end)
         end; 
         define frame-gadget-bars ?class (?superclasses) ?more-slots end; }
  { define frame-gadget-bars ?class:name (?superclasses:*) 
      ?non-bar-slot; ?more-slots:*
    end }
    => { define frame-gadget-bars ?class (?superclasses) ?more-slots end; }
end macro frame-gadget-bars-definer;


define open generic generate-panes
    (frame :: <frame>) => (panes :: false-or(<sheet>));

// Note that this assumes that 'current-frame()' will return the right thing...
define method generate-panes
    (frame :: <frame>) => (panes :: false-or(<sheet>))
  frame-layout(frame)
end method generate-panes;


/// Starting frames

define method start-frame
    (frame :: <frame>, #key activate? = #t)
 => (status-code)
  // Always call 'find-port' to force port initialization
  let _port = find-port();
  let framem = frame-manager(frame) | find-frame-manager();
  attach-frame(framem, frame);
  frame-mapped?(frame) := #t;
  if (activate?)
    simple-frame-top-level(frame)
  end
end method start-frame;

define method simple-frame-top-level
    (frame :: <simple-frame>, #rest args, #key, #all-keys)
 => (status-code)
  let _port = port(frame);
  block (return)
    // The "read-eval-print" loop for event-driven applications...
    while (#t)
      let (exit?, status-code) = process-next-event(_port);
      case
        exit? =>
	  distribute-event(_port,
			   make(<application-exited-event>,
				frame: frame,
				status-code: status-code));
	  return(status-code);
        frame-state(frame) = #"destroyed" =>
          return(0);
      end
    end
  end
end method simple-frame-top-level;

define method frame-exit (frame :: <frame>, #key destroy? = #t) => ()
  case
    destroy? =>
      destroy-frame(frame);
    otherwise =>
      frame-mapped?(frame) := #f;
  end
end method frame-exit;

define method frame-position
    (frame :: <frame>) => (left, top)
  let container-region = frame.%container-region;
  if (container-region)
    values(box-left(container-region), box-top(container-region))
  else
    values(frame-geometry(frame)[0], frame-geometry(frame)[1])
  end
end method frame-position;

// Hacks to make the frame big enough for the menu and border
// In DUIM, this gets handled by doing proper layout
define constant $window-frame-extra-width  = 8;
define constant $window-frame-extra-height = 46;

define method frame-size
    (frame :: <frame>) => (width, height)
  let container-region = frame.%container-region;
  if (container-region)
    box-size(container-region)
  else
    let layout = frame-layout(frame);
    let (layout-width, layout-height) 
      = if (layout) box-size(layout) else values(100, 100) end;
    let width  = (frame-geometry(frame)[2] | layout-width)  + $window-frame-extra-width;
    let height = (frame-geometry(frame)[3] | layout-height) + $window-frame-extra-height;
    values(width, height)
  end
end method frame-size;

define method attach-frame
    (framem :: <frame-manager>, frame :: <frame>) => ()
  let panes = generate-panes(frame);
  let top-level-layout = frame-wrapper(framem, frame, panes);
  let (x, y) = frame-position(frame);
  let (width, height) = frame-size(frame);
  let top-sheet
    = make-pane(<top-level-sheet>,
		frame:  frame,
		x:      x,
		y:      y,
		width:  width,
		height: height,
		container: frame.%container-region,
		container-region: frame.%container-region);
  top-level-sheet(frame) := top-sheet;
  graft-sheet(find-port(), top-sheet);
  if (top-level-layout)
    add-child(top-sheet, top-level-layout)
  end;
  frame-manager(frame) := framem;
  add!(framem.%frames, frame)
end method attach-frame;

// Kludge for 'frame-wrapper' below...
define sealed class <wrapper-sheet> (<basic-sheet>)
end class <wrapper-sheet>;

define method frame-wrapper
    (framem :: <frame-manager>, frame :: <frame>, panes)
 => (layout :: <sheet>)
  ignore(framem);
  let menu-bar = frame-menu-bar(frame);
  if (menu-bar & panes)
    // The geometry is computed correctly by only using the 'frame-layout'
    // to deduce the size of the client area.  DUIM gets this right by
    // doing proper layout
    make-pane(<wrapper-sheet>,
	      children: vector(menu-bar, panes))
  else
    menu-bar | panes
  end
end method frame-wrapper;

define method frame-mapped? (frame :: <frame>) => (mapped? :: <boolean>)
  frame.%mapped?
end method frame-mapped?;

define method frame-mapped?-setter (mapped?, frame :: <frame>)
  if (mapped?)
    map-frame(frame)
  else
    unmap-frame(frame)
  end;
  frame.%mapped? := mapped?
end method frame-mapped?-setter;

define method map-frame (frame :: <frame>) => ()
  unless (frame.%mapped?)
    let top-sheet = top-level-sheet(frame);
    if (top-sheet)
      sheet-mapped?(top-sheet) := #t
    end
  end
end method map-frame;

define method unmap-frame (frame :: <frame>) => ()
  if (frame.%mapped?)
    let top-sheet = top-level-sheet(frame);
    if (top-sheet)
      sheet-mapped?(top-sheet) := #f
    end
  end
end method unmap-frame;


define method destroy-frame (frame :: <frame>) => ()
  let top-sheet = top-level-sheet(frame);
  let framem = frame-manager(frame);
  if (framem)
    remove!(framem.%frames, frame)
  end;
  if (top-sheet)
    destroy-sheet(top-sheet)
  end
end method destroy-frame;

define method handle-event
    (frame :: <frame>, event :: <frame-destroyed-event>) => ()
  frame-state(frame) := #"destroyed"
end method handle-event;

// Find the most recently created frame of class 'class'
define method find-frame
    (class :: <class>, #key frame-manager: framem)
 => (frame :: false-or(<frame>))
  block (return)
    let framem = framem | find-frame-manager();
    for (frame in reverse(framem.%frames))
      if (object-class(frame) == class)
	return(frame)
      end
    end;
    #f
  end
end method find-frame;


/// 'contain'

define method contain
    (pane, #rest keys, #key activate? = #f)
 => (pane :: <sheet>, frame :: <frame>)
  let container = apply(make-container, pane, keys);
  start-frame(container, activate?: activate?);
  values(pane, container)
end method contain;

define method contain
    (class :: <class>, #rest initargs, #key)
 => (pane :: <sheet>, frame :: <frame>)
  apply(contain, make(class), initargs)
end method contain;

define method make-container
    (frame :: <frame>, #rest initargs, #key) => (frame :: <frame>)
  ignore(initargs);
  frame
end method make-container;

define method make-container 
    (sheet :: <sheet>, #rest initargs, #key) => (frame :: <frame>)
  apply(make, <frame>, layout: sheet, title: "Container", initargs)
end method make-container;

define method make-container
    (menu-bar :: <menu-bar>, #rest initargs, #key) => (frame :: <frame>)
  apply(make, <frame>, menu-bar: menu-bar, title: "Container", initargs)
end method make-container;

define method make-container
    (menu :: <menu>, #rest initargs, #key) => (frame :: <frame>)
  apply(make-container,
	make(<menu-bar>, children: vector(menu)),
	initargs)
end method make-container;

define method make-container
    (box :: <menu-box>, #rest initargs, #key) => (frame :: <frame>)
  apply(make-container,
	make(<menu>, label: "Menu", children: vector(box)),
	initargs)
end method make-container;

define method make-container
    (button :: <menu-button>, #rest initargs, #key) => (frame :: <frame>)
  apply(make-container,
	make(<menu>, label: "Menu", children: vector(button)),
	initargs)
end method make-container;


/// Container frames

define open class <container-frame> (<frame>)
  slot container-sub-frame = #f,
    init-keyword: frame:;
  slot container-sub-frame-region = make-bounding-box(100, 100, 200, 200),
    init-keyword: frame:;
end class <container-frame>;
