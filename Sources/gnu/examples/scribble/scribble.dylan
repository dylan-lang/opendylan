Module:       gtk-scribble
Author:       Scott McKay
Synopsis:     Simple scribble application
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $TRUE  :: <integer> = 1;
define constant $FALSE :: <integer> = 0;

define constant $null-gpointer = null-pointer(<gpointer>);

define variable *backing-pixmap* :: false-or(<GdkPixmap*>) = #f;

define configure-callback gtk-configure-callback = handle-configure-event;

// Create a new backing pixmap of the appropriate size
define function handle-configure-event
    (widget :: <GtkWidget*>, event :: <GdkEventConfigure*>)
 => (code :: <integer>)
  when (*backing-pixmap*)
    gdk-pixmap-unref(*backing-pixmap*);
    *backing-pixmap* := #f
  end;
  let allocation = widget.allocation-value;
  let pixmap = gdk-pixmap-new(widget.window-value,
                              allocation.width-value,
                              allocation.height-value,
                              -1);
  gdk-draw-rectangle(pixmap,
                     widget.style-value.white-gc-value,
                     $TRUE,
                     0, 0,
		     widget.allocation-value.width-value,
		     widget.allocation-value.height-value);
  *backing-pixmap* := pixmap;
  $TRUE
end function handle-configure-event;

define expose-callback gtk-expose-callback = handle-expose-event;

// Redraw the screen from the backing pixmap
define function handle-expose-event
    (widget :: <GtkWidget*>, event :: <GdkEventExpose*>)
 => (code :: <integer>)
  gdk-draw-pixmap(widget.window-value,
		  //---*** Uh-oh, this ain't right
                  widget.style-value.fg-gc-value[GTK-WIDGET(widget).state-value],
                  *backing-pixmap*,
                  event.area-value.x-value, event.area-value.y-value,
                  event.area-value.x-value, event.area-value.y-value,
                  event.area-value.width-value, event.area-value.height-value);
  $FALSE;
end function handle-expose-event;

// Draw a rectangle on the screen
define function draw-brush
   (widget :: <GtkWidget*>, x :: <real>, y :: <real>) => ()
  with-stack-structure (update-rect :: <GdkRectangle*>)
    update-rect.x-value := x - 5;
    update-rect.y-value := y - 5;
    update-rect.width-value  := 10;
    update-rect.height-value := 10;
    gdk-draw-rectangle(*backing-pixmap*,
                      widget.style-value.black-gc-value,
                      $TRUE,
                      update-rect.x-value, update-rect.y-value,
                      update-rect.width-value, update-rect.height-value);
    gtk-widget-draw(widget, update-rect)
  end
end function draw-brush;

define button-press-callback gtk-button-press-callback = handle-button-press;

define function handle-button-press
    (widget :: <GtkWidget*>, event :: <GdkEventButton*>)
 => (code :: <integer>)
  when (event.button-value = 1 & *backing-pixmap*)
    draw-brush(widget, event.x-value, event.y-value)
  end;
  $TRUE
end function handle-button-press;

define motion-callback gtk-motion-calback = handle-motion-event;

define function handle-motion-event
    (widget :: <GtkWidget*>, event :: <GdkEventMotion*>)
 => (code :: <integer>)
  let (x, y, state)
    = if (event.is-hint-value ~= 0)
	gdk-window-get-pointer(event.window-value)
      else
	values(event.x-value, event.y-value, event.state-value)
      end;
  when (logand(state, $GDK-BUTTON1-MASK) ~= 0 & *backing-pixmap*)
    draw-brush(widget, x, y)
  end;
  $TRUE
end function handle-motion-event;

define widget-callback gtk-quit = quit;

define function quit
    (widget :: <GtkWidget*>, data :: <object>)
  gtk-exit(0)
end function quit;

define function main ()
  initialize-gtk();

  // Create the top level window
  let window = gtk-window-new($GTK-WINDOW-TOPLEVEL);
  with-c-string (title = "Test Input")
    gtk-window-set-title(window, title)
  end;
  gtk-signal-connect*(GTK-OBJECT(window), "destroy", gtk-quit, $null-gpointer);

  // Create a vbox container
  let vbox = gtk-vbox-new($FALSE, 0);
  gtk-container-add(GTK-CONTAINER(window), vbox);
  gtk-widget-show(vbox);

  // Create the drawing area
  let drawing-area = gtk-drawing-area-new();
  gtk-drawing-area-size(drawing-area, 200, 200);
  gtk-box-pack-start(GTK-BOX(vbox), drawing-area, $TRUE, $TRUE, 0);
  gtk-widget-show(drawing-area);

  // Signals used to handle backing pixmap
  gtk-signal-connect*(GTK-OBJECT(drawing-area), "expose-event", gtk-expose-callback, $null-gpointer);
  gtk-signal-connect*(GTK-OBJECT(drawing-area), "configure-event", gtk-configure-callback, $null-gpointer);

  // Event signals
  gtk-signal-connect*(GTK-OBJECT(drawing-area), "motion-notify-event", gtk-motion-calback, $null-gpointer);
  gtk-signal-connect*(GTK-OBJECT(drawing-area), "button-press-event", gtk-button-press-callback, $null-gpointer);
  gtk-widget-set-events(drawing-area, 
			logior($GDK-EXPOSURE-MASK,
			       $GDK-LEAVE-NOTIFY-MASK,
			       $GDK-BUTTON-PRESS-MASK,
			       $GDK-POINTER-MOTION-MASK,
			       $GDK-POINTER-MOTION-HINT-MASK));

  // ...And a quit button
  let button 
    = with-c-string (label = "Quit")
	gtk-button-new-with-label(label)
      end;
  gtk-box-pack-start(GTK-BOX(vbox), button, $FALSE, $FALSE, 0);
  // gtk-signal-connect-object*(GTK-OBJECT(button), "clicked", gtk-widget-destroy, window);
  gtk-widget-show(button);

  // Finally, display the main window
  gtk-widget-show(window);

  // Wait for events...
  gtk-main();
end function main;

main();
