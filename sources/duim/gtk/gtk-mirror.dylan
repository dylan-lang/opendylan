Module:       gtk-duim
Synopsis:     GTK mirror implementation
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// GTK panes

define open abstract class <gtk-pane-mixin>
    (<standard-input-mixin>,
     <mirrored-sheet-mixin>)
end class <gtk-pane-mixin>;

// Returns #t, meaning that the port will take care of repainting
define method port-handles-repaint?
    (_port :: <gtk-port>, sheet :: <mirrored-sheet-mixin>)
 => (true? :: <boolean>)
  #t
end method port-handles-repaint?;

define method port-handles-repaint?
    (_port :: <gtk-port>, sheet :: <sheet-with-repainting-mixin>)
 => (true? :: <boolean>)
  #f
end method port-handles-repaint?;

/// GTK mirrors

define open abstract class <gtk-mirror> (<mirror>)
  sealed slot mirror-sheet :: <sheet>,
    required-init-keyword: sheet:;
  sealed slot signal-handler-ids :: <table> = make(<table>);
end class <gtk-mirror>;

define method initialize
    (mirror :: <gtk-mirror>, #key) => ()
  next-method();
  sheet-direct-mirror(mirror-sheet(mirror)) := mirror;
end method initialize;

define protocol <<gtk-mirror-protocol>> ()
  function make-gtk-mirror
    (sheet :: <abstract-sheet>) => (mirror :: <gtk-mirror>);
  function install-event-handlers
    (sheet :: <abstract-sheet>, mirror :: <gtk-mirror>) => ();
  function update-mirror-attributes
    (sheet :: <abstract-sheet>, mirror :: <gtk-mirror>) => ();
  function set-mirror-parent
    (mirror :: <gtk-mirror>, parent :: <gtk-mirror>) => ();
  function move-mirror
    (parent :: <gtk-mirror>, mirror :: <gtk-mirror>, 
     x :: <integer>, y :: <integer>)
 => ();
  function size-mirror
    (parent :: <gtk-mirror>, mirror :: <gtk-mirror>, 
     width :: <integer>, height :: <integer>)
 => ();
end protocol <<gtk-mirror-protocol>>;

define constant $mirror-widget-table :: <object-table> = make(<table>);

define sealed method do-make-mirror
    (_port :: <gtk-port>, sheet :: <sheet>)
 => (mirror :: <gtk-mirror>)
  let parent = sheet-device-parent(sheet);
  let mirror = make-gtk-mirror(sheet);
  unless (instance?(mirror, <top-level-mirror>))
    with-gdk-lock gtk-widget-show(mirror.mirror-widget) end;
  end;
  install-event-handlers(sheet, mirror);
  update-mirror-attributes(sheet, mirror);
  set-mirror-parent(mirror, sheet-direct-mirror(parent));
  mirror
end method do-make-mirror;

define sealed method widget-mirror
    (widget :: <C-pointer>) => (mirror :: false-or(<gtk-mirror>))
  element($mirror-widget-table, pointer-address(widget), default: #f)
end method widget-mirror;

define sealed method widget-mirror-setter
    (mirror :: <gtk-mirror>, widget :: <C-pointer>)
 => (mirror :: <gtk-mirror>)
  element($mirror-widget-table, pointer-address(widget)) := mirror
end method widget-mirror-setter;

define sealed method widget-mirror-setter
    (mirror :: singleton(#f), widget :: <C-pointer>)
 => (mirror :: singleton(#f))
  remove-key!($mirror-widget-table, pointer-address(widget));
  #f
end method widget-mirror-setter;


/// Empty methods on non-window mirrors

define sealed method mirror-edges
    (_port :: <gtk-port>, sheet :: <sheet>, mirror :: <gtk-mirror>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  values(0, 0, 100, 100)	//--- kludge city
end method mirror-edges;

// The real methods are on more specific classes, such as <widget-mirror>
define sealed method set-mirror-edges
    (_port :: <gtk-port>, sheet :: <sheet>, mirror :: <gtk-mirror>,
     left  :: <integer>, top    :: <integer>,
     right :: <integer>, bottom :: <integer>) => ()
  #f
end method set-mirror-edges;

// Ditto...
define sealed method map-mirror
    (_port :: <gtk-port>, sheet :: <sheet>, mirror :: <gtk-mirror>) => ()
  #f
end method map-mirror;

// Ditto...
define sealed method unmap-mirror
    (_port :: <gtk-port>, sheet :: <sheet>, mirror :: <gtk-mirror>) => ()
  #f
end method unmap-mirror;

// Ditto...
define sealed method destroy-mirror 
    (_port :: <gtk-port>, sheet :: <sheet>, mirror :: <gtk-mirror>) => ()
  sheet-direct-mirror(sheet) := #f
end method destroy-mirror;

// Ditto...
define method install-event-handlers
    (sheet :: <sheet>, mirror :: <gtk-mirror>) => ()
  #f
end method install-event-handlers;

// Ditto...
define method update-mirror-attributes
    (sheet :: <sheet>, mirror :: <gtk-mirror>) => ()
  #f
end method update-mirror-attributes;


/// Mirror creation and destruction

define abstract class <widget-mirror> (<gtk-mirror>)
  sealed slot mirror-widget = #f,
    init-keyword: widget:;
  sealed slot %region :: <bounding-box>,
    required-init-keyword: region:;
end class <widget-mirror>;

define sealed domain make (singleton(<widget-mirror>));
define sealed domain initialize (<widget-mirror>);

define sealed inline method make
    (mirror :: subclass(<widget-mirror>), #rest args, #key sheet)
 => (mirror :: <widget-mirror>)
  let (left, top, right, bottom) = sheet-native-edges(sheet);
  apply(next-method, mirror,
	region: make-bounding-box(left, top, right, bottom),
	args)
end method make;

define method initialize
    (mirror :: <widget-mirror>, #key) => ()
  next-method();
  let widget = mirror-widget(mirror);
  when (widget)
    widget-mirror(widget) := mirror
  end
end method initialize;

define sealed method destroy-mirror 
    (_port :: <gtk-port>, sheet :: <sheet>, mirror :: <widget-mirror>) => ()
  let widget = mirror-widget(mirror);
  mirror-widget(mirror) := #f;
  ignoring("destroy-mirror")
end method destroy-mirror;

//---*** WHAT ABOUT THIS?  WHO IS SUPPOSED TO CALL IT?
// Called by main WM_DESTROY handler
define sealed method note-mirror-destroyed
    (sheet :: <sheet>, mirror :: <widget-mirror>) => ()
  ignoring("note-mirror-destroyed")
  // let handle :: <HWND> = window-handle(mirror);
  // window-mirror(handle) := #f;
  // window-handle(mirror) := $NULL-HWND
end method note-mirror-destroyed;


/// Mirror manipulation

// For non-top-level sheets, we just show the window
define sealed method map-mirror
    (_port :: <gtk-port>, sheet :: <sheet>, mirror :: <widget-mirror>) => ()
  let widget = mirror-widget(mirror);
  duim-debug-message("Showing %=", sheet);
  with-gdk-lock
    gtk-widget-show(widget)
  end
end method map-mirror;

define sealed method unmap-mirror
    (_port :: <gtk-port>, sheet :: <sheet>, mirror :: <widget-mirror>) => ()
  let widget = mirror-widget(mirror);
  with-gdk-lock
    gtk-widget-hide(widget)
  end
end method unmap-mirror;

define sealed method raise-mirror 
    (_port :: <gtk-port>, sheet :: <sheet>, mirror :: <widget-mirror>,
     #key activate? = #t)
 => ()
  if (activate?)
    ignoring("activate? keyword to raise-mirror")
  end;
  let widget = mirror-widget(mirror);
  with-gdk-lock
    gdk-window-raise(gtk-widget-get-window(widget))
  end
end method raise-mirror;

define sealed method lower-mirror
    (_port :: <gtk-port>, sheet :: <sheet>, mirror :: <widget-mirror>) => ()
  let widget = mirror-widget(mirror);
  with-gdk-lock
    gdk-window-lower(widget.gtk-widget-get-window)
  end
end method lower-mirror;

define sealed method mirror-visible? 
    (_port :: <gtk-port>, sheet :: <sheet>, mirror :: <widget-mirror>)
 => (visible? :: <boolean>)
  let widget = mirror-widget(mirror);
  with-gdk-lock
    gdk-window-is-visible(widget.gtk-widget-get-window) == $false
  end
end method mirror-visible?;


/// Window mirrors

define sealed method mirror-edges
    (_port :: <gtk-port>, sheet :: <sheet>, mirror :: <widget-mirror>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  box-edges(mirror.%region)
end method mirror-edges;

define sealed method set-mirror-edges
    (_port :: <gtk-port>, sheet :: <sheet>, mirror :: <widget-mirror>,
     left  :: <integer>, top    :: <integer>,
     right :: <integer>, bottom :: <integer>)
 => ()
  let parent = sheet-device-parent(sheet);
  let parent-mirror = sheet-direct-mirror(parent);
  let width  = right - left;
  let height = bottom - top;
  let old-region = mirror.%region;
  let (old-left, old-top)     = box-position(old-region);
  let (old-width, old-height) = box-size(old-region);
  mirror.%region := set-box-edges(mirror.%region, left, top, right, bottom);
  if (left ~== old-left | top ~== old-top)
    move-mirror(parent-mirror, mirror, left, top)
  end;
  if (width ~== old-width | height ~== old-height)
    size-mirror(parent-mirror, mirror, width, height)
  end
end method set-mirror-edges;


// Returns the position of the sheet in "absolute" (screen) coordinates
define sealed method sheet-screen-position
    (_port :: <gtk-port>, sheet :: <sheet>)
 => (x :: <integer>, y :: <integer>)
  let ancestor  = sheet-device-parent(sheet);
  let transform = sheet-delta-transform(sheet, ancestor);
  // Get the position of the sheet in its mirrored parent's coordinates
  let (x, y) = transform-position(transform, 0, 0);
  let mirror = sheet-direct-mirror(ancestor);
  client-to-screen-position(mirror, x, y)
end method sheet-screen-position;

// Given a position (x, y) within a mirror, convert it to a position on the screen
define sealed method client-to-screen-position
    (mirror :: <widget-mirror>, x :: <integer>, y :: <integer>)
 => (screen-x :: <integer>, screen-y :: <integer>)
  ignoring("client-to-screen-position");
  values(x, y)
end method client-to-screen-position;


/// Fixed container mirrors
///
/// The class of mirror that can contain other mirrors

define class <fixed-container-mirror> (<widget-mirror>)
end class <fixed-container-mirror>;

define class <drawing-area-mirror> (<widget-mirror>)
end class <drawing-area-mirror>;

define method make-gtk-mirror 
    (sheet :: <mirrored-sheet-mixin>)
=> (mirror :: <widget-mirror>)
  do-make-gtk-mirror(sheet)
end method make-gtk-mirror;

define method do-make-gtk-mirror 
    (sheet :: <mirrored-sheet-mixin>)
=> (mirror :: <widget-mirror>)
  let widget = with-gdk-lock gtk-fixed-new() end;
  //---*** We really want to switch this off entirely...
  //gtk-container-set-resize-mode(widget, $GTK-RESIZE-QUEUE);
  make(<fixed-container-mirror>,
       widget: widget,
       sheet:  sheet)
end method do-make-gtk-mirror;

define method do-make-gtk-mirror
    (sheet :: <standard-repainting-mixin>)
  => (mirror :: <widget-mirror>)
  with-gdk-lock
    let widget = gtk-drawing-area-new();
    //  gtk-drawing-area-size(widget, 200, 200);
    gtk-widget-set-size-request(widget, 200, 200);
    widget.@can-focus := $true;
    make(<drawing-area-mirror>,
         widget: widget,
         sheet:  sheet);
  end;
end method do-make-gtk-mirror;

define macro duim-g-signal-connect
 { duim-g-signal-connect (?sheet:name, ?signal-name:expression) (?args:*) ?body:* end }
  => { begin
         let mirror = ?sheet.sheet-direct-mirror;
         let widget = mirror-widget(mirror);
         with-gdk-lock
           let handler-id
             = g-signal-connect(widget, as(<string>, ?signal-name),
                                method(?args) ?body end);
           mirror.signal-handler-ids[?signal-name] := handler-id;
         end
       end; }
end;
                                             
define macro with-disabled-event-handler
  { with-disabled-event-handler (?mirror:expression, ?signal-name:expression)
      ?body:*
    end }
 => { let signal-handler-id
        = element(?mirror.signal-handler-ids, ?signal-name, default: #f);
      let widget = mirror-widget(?mirror);
      if (signal-handler-id)
        block()
          g-signal-handler-block(widget, signal-handler-id);
          ?body
        cleanup
          g-signal-handler-unblock(widget, signal-handler-id);
        end;
      else
        duim-debug-message("key %= not found in table %=", ?signal-name, ?mirror.signal-handler-ids.key-sequence);
      end; }
end;

define method install-event-handlers
    (sheet :: <mirrored-sheet-mixin>, mirror :: <fixed-container-mirror>) => ()
  next-method();
  let widget = mirror-widget(mirror);
  duim-g-signal-connect(sheet, #"expose-event") (widget, event) handle-gtk-expose-event(sheet, event) end;
  with-gdk-lock
    gtk-widget-add-events(widget, $GDK-EXPOSURE-MASK);
  end
end method install-event-handlers;

define method install-event-handlers
    (sheet :: <mirrored-sheet-mixin>, mirror :: <drawing-area-mirror>) => ()
  next-method();
  let widget = mirror-widget(mirror);
  duim-g-signal-connect(sheet, #"expose-event") (widget, event) handle-gtk-expose-event(sheet, event) end;
  duim-g-signal-connect(sheet, #"button-press-event") (widget, event) 
     gtk-widget-grab-focus(widget);
     handle-gtk-button-event(sheet, event)
  end;
  duim-g-signal-connect(sheet, #"button-release-event") (widget, event) handle-gtk-button-event(sheet, event) end;
  duim-g-signal-connect(sheet, #"motion-notify-event") (widget, event) handle-gtk-motion-event(sheet, event) end;
  duim-g-signal-connect(sheet, #"key-press-event") (widget, event) handle-gtk-key-event(sheet, event) end;
  duim-g-signal-connect(sheet, #"key-release-event") (widget, event) handle-gtk-key-event(sheet, event) end;
  with-gdk-lock
    gtk-widget-add-events(widget, $GDK-BUTTON-RELEASE-MASK);
    gtk-widget-add-events(widget, $GDK-EXPOSURE-MASK);
    gtk-widget-add-events(widget, $GDK-BUTTON-PRESS-MASK);
    gtk-widget-add-events(widget, logior($GDK-POINTER-MOTION-MASK, $GDK-POINTER-MOTION-HINT-MASK));
    gtk-widget-add-events(widget, logior($GDK-KEY-PRESS-MASK, $GDK-KEY-RELEASE-MASK));
  end
end method install-event-handlers;

define sealed method handle-gtk-expose-event
    (sheet :: <mirrored-sheet-mixin>, event :: <GdkEventExpose>)
 => (handled? :: <boolean>)
  let area   = event.GdkEventExpose-area;
  let x      = area.GdkRectangle-x;
  let y      = area.GdkRectangle-y;
  let width  = area.GdkRectangle-width;
  let height = area.GdkRectangle-height;
  let region = make-bounding-box(x, y, x + width, y + height);
  duim-debug-message("Repainting %=: %d, %d %d x %d",
		     sheet, x, y, width, height);
  // We call 'handle-event' instead of 'distribute-event' because we
  // want the repainting to happen between BeginPaint and EndPaint
  handle-event(sheet,
	       make(<window-repaint-event>,
		    sheet: sheet,
		    region: region))
end method handle-gtk-expose-event;

define method set-mirror-parent
    (child :: <widget-mirror>, parent :: <fixed-container-mirror>)
 => ()
  let (x, y) = sheet-native-edges(mirror-sheet(child));
  with-gdk-lock
    gtk-fixed-put(mirror-widget(parent),
                  mirror-widget(child),
                  x, y)
  end
end method set-mirror-parent;

define method set-mirror-parent
    (child :: <widget-mirror>, parent :: <drawing-area-mirror>)
 => ()
  let (x, y) = sheet-native-edges(mirror-sheet(child));
  with-gdk-lock
    gtk-fixed-put(mirror-widget(parent),
                  mirror-widget(child),
                  x, y)
  end
end method set-mirror-parent;

define method set-mirror-parent
    (child :: <widget-mirror>, parent :: <widget-mirror>)
 => ()
  with-gdk-lock
    gtk-container-add(mirror-widget(parent),
                      mirror-widget(child))
  end
end;

/*
define method set-mirror-parent
    (child :: <popup-menu-mirror>, parent :: <display-mirror>)
 => ()
   gtk-container-add(GTK-CONTAINER(mirror-widget(parent)),
		    mirror-widget(child))
end;
*/

define method move-mirror
    (parent :: <fixed-container-mirror>, child :: <widget-mirror>,
     x :: <integer>, y :: <integer>)
 => ()
  with-gdk-lock
    gtk-fixed-move(mirror-widget(parent),
                   mirror-widget(child),
                   x, y)
  end
end method move-mirror;

define method move-mirror
    (parent :: <fixed-container-mirror>, child :: <scrolled-mirror>,
     x :: <integer>, y :: <integer>)
 => ()
  with-gdk-lock
    gtk-fixed-move(mirror-widget(parent),
                   scrolled-window(child),
                   x, y)
  end
end method move-mirror;

define method size-mirror
    (parent :: <fixed-container-mirror>, child :: <widget-mirror>,
     width :: <integer>, height :: <integer>)
 => ()
  ignore(parent);
  set-mirror-size(child, width, height)
end method size-mirror;

define method size-mirror
    (parent :: <drawing-area-mirror>, child :: <widget-mirror>,
     width :: <integer>, height :: <integer>)
 => ()
  ignore(parent);
  set-mirror-size(child, width, height)
end method size-mirror;

define method set-mirror-size
    (mirror :: <widget-mirror>, width :: <integer>, height :: <integer>)
 => ()
  set-widget-size(mirror, mirror.mirror-widget, width, height);
end;

define method set-widget-size (mirror :: <widget-mirror>, widget :: <GtkWidget>, width :: <integer>, height :: <integer>)
  gtk-debug("set-mirror-size for %= to %=x%=", widget, width, height);
  with-gdk-lock
    gtk-widget-set-size-request(widget, width, height);
  end;
/*let (left, top) = box-position(mirror.%region);
  with-stack-structure (allocation :: <GtkAllocation>)
    allocation.GdkRectangle-x      := left;
    allocation.GdkRectangle-y      := top;
    allocation.GdkRectangle-width  := width;
    allocation.GdkRectangle-height := height;
    with-gdk-lock
      gtk-widget-size-allocate(widget, allocation)
    end
  end;*/
end method;

define method set-mirror-size
    (mirror :: <drawing-area-mirror>, width :: <integer>, height :: <integer>)
 => ()
//  gtk-drawing-area-size(mirror-widget(mirror), width, height);
  gtk-debug("set-mirror-size for %= to %=x%= (ignored)", mirror-widget(mirror), width, height);
  with-gdk-lock
    gtk-widget-set-size-request(mirror-widget(mirror), width, height);
  end
end method set-mirror-size;

define method set-mirror-size
    (mirror :: <top-level-mirror>, width :: <integer>, height :: <integer>)
 => ()
  gtk-debug("set-mirror-size for %= to %=x%= (top-level)", mirror-widget(mirror), width, height);
  with-gdk-lock
    gtk-window-set-default-size(mirror-widget(mirror), width, height);
  end
end method set-mirror-size;
