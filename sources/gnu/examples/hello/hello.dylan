Module:       gtk-hello-world
Author:       Scott McKay
Synopsis:     First "Hello World" example from GTK Tutorial
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $TRUE  :: <integer> = 1;
// define constant $FALSE :: <integer> = 0;

define constant $null-gpointer = null-pointer(<gpointer>);

define function main () => ()
  // Initialize GTK
  initialize-gtk();

  // Create a new window
  let window = gtk-window-new($GTK-WINDOW-TOPLEVEL);

  // Connect callbacks for delete and destroy events
  debug-message("GTK-OBJECT(window)=%=", GTK-OBJECT(window));
  debug-message("Register delete event callback");
  gtk-signal-connect*(GTK-OBJECT(window), "delete-event", gtk-delete-event-callback, $null-gpointer);
  debug-message("Register destroy callback");
  gtk-signal-connect*(GTK-OBJECT(window), "destroy", gtk-destroy-callback, $null-gpointer);

  // Set the border width of the top level window
  debug-message("GTK-CONTAINER(window)=%=", GTK-CONTAINER(window));
  debug-message("Set border width");
  gtk-container-set-border-width(GTK-CONTAINER(window), 10);

  // Create a new button
  debug-message("Create button");
  let button
    = with-c-string (string = "Hello World")
	gtk-button-new-with-label(string)
      end;

  // Connect the button-clicked callback for the button
  gtk-signal-connect*(GTK-OBJECT(button), "clicked", gtk-button-callback, $null-gpointer);

  // Pack the button into the window
  debug-message("Add the button to the window");
  gtk-container-add(GTK-CONTAINER(window), button);
    
  // Finally, display the window(s)
  debug-message("Show the button");
  gtk-widget-show(button);
  debug-message("Show the main window");
  gtk-widget-show(window);
    
  // Wait for events...
  debug-message("Start the event loop");
  gtk-main();
end function main;

define delete-event-callback gtk-delete-event-callback = handle-delete-event;

// Returns FALSE to indicate that the widget should be destroyed,
// or TRUE to indicated that the window should not be destroyed
define function handle-delete-event
    (widget :: <GtkWidget*>, event :: <GdkEvent*>, data :: <object>)
 => (code :: <integer>)
  debug-message("Delete event occurred");
  // Change TRUE to FALSE and the main window will be destroyed with a "delete-event"
  $TRUE
end function handle-delete-event;

define widget-callback gtk-destroy-callback = handle-destroy-event;

define function handle-destroy-event
    (widget :: <GtkWidget*>, data :: <object>)
 => (code :: <integer>)
  debug-message("Destroy event occurred");
  gtk-main-quit();
  0
end function handle-destroy-event;

define widget-callback gtk-button-callback = handle-button-click;

define function handle-button-click
    (widget :: <GtkWidget*>, data :: <object>)
 => (code :: <integer>)
  debug-message("Hello World!");
  0
end function handle-button-click;

main();
