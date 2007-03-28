Module:       gtk-duim
Synopsis:     GTK event processing implementation
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// GTK signals

define constant $signal-handlers = make(<object-table>);

define class <signal-handler> (<sealed-constructor-mixin>)
  constant sealed slot handler-name :: <C-string>,
    required-init-keyword: name:;
  constant sealed slot handler-function :: <GtkSignalFunc>,
    required-init-keyword: function:;
end class <signal-handler>;

define function register-signal-handler
    (name :: <string>, function :: <GtkSignalFunc>,
     #key key = as(<symbol>, name)) => ()
  $signal-handlers[key]
    := make(<signal-handler>,
      name:     as(<C-string>, name),
      function: function)
end function register-signal-handler;

define function fetch-signal-handler
    (name :: <symbol>) => (_handler :: <signal-handler>)
  element($signal-handlers, name, default: #f)
    | error("No GTK handler registered for '%s'", name)
end function fetch-signal-handler;

define function do-with-disabled-event-handler
    (function :: <function>, widget :: <GtkWidget*>, name :: <symbol>)
 => (#rest values)
  let _handler = handler-function(fetch-signal-handler(name));
  let object = GTK-OBJECT(widget);
  block ()
    gtk-signal-handler-block-by-func(object, _handler, $null-gpointer);
    function()
  cleanup
    gtk-signal-handler-unblock-by-func(object, _handler, $null-gpointer);
  end
end function do-with-disabled-event-handler;

define macro with-disabled-event-handler
  { with-disabled-event-handler (?widget:expression, ?name:expression)
      ?body:body
    end }
 => { do-with-disabled-event-handler(method () ?body end, GTK-WIDGET(?widget), ?name) }
end macro with-disabled-event-handler;

define macro event-handler-definer
  { define event-handler (?name:name, ?event-type:name)
      ?eq:token ?handler:name }
 => { define gtk-callback ("_gtk-" ## ?name ## "-callback", ?event-type)
        ?eq "handle_" ## ?name;
      register-signal-handler(as-lowercase(?"name"), "_gtk-" ## ?name ## "-callback");
      define open generic ?handler
    (sheet :: <abstract-sheet>, widget :: <GtkWidget*>, 
     event :: ?event-type)
       => (handled? :: <boolean>);
      define function "handle_" ## ?name
    (widget :: <GtkWidget*>, event :: ?event-type)
       => (code :: <integer>)
  do-handle-gtk-signal
    (?handler, widget, ?"name", widget, event)
      end }
end macro event-handler-definer;

define event-handler (destroy,              <GdkEventAny*>)       = handle-gtk-destroy-event;
define event-handler (delete_event,         <GdkEventAny*>)       = handle-gtk-delete-event;
define event-handler (motion_notify_event,  <GdkEventMotion*>)    = handle-gtk-motion-event;
define event-handler (button_press_event,   <GdkEventButton*>)    = handle-gtk-button-press-event;
define event-handler (button_release_event, <GdkEventButton*>)    = handle-gtk-button-release-event;
define event-handler (key_press_event,      <GdkEventKey*>)       = handle-gtk-key-press-event;
define event-handler (key_release_event,    <GdkEventKey*>)       = handle-gtk-key-release-event;
define event-handler (configure_event,      <GdkEventConfigure*>) = handle-gtk-configure-event;
define event-handler (expose_event,         <GdkEventExpose*>)    = handle-gtk-expose-event;
define event-handler (enter_event,          <GdkEventCrossing*>)  = handle-gtk-enter-event;
define event-handler (leave_event,          <GdkEventCrossing*>)  = handle-gtk-leave-event;

define event-handler (clicked,              <GdkEventAny*>)       = handle-gtk-clicked-event;
define event-handler (select_row,           <GdkEventAny*>)       = handle-gtk-select-row-event;
define event-handler (click_column,         <GdkEventAny*>)       = handle-gtk-click-column-event;
define event-handler (resize_column,        <GdkEventAny*>)       = handle-gtk-resize-column-event;

  
define inline function do-handle-gtk-signal
    (handler_ :: <function>, widget :: <GtkWidget*>, name :: <string>,
     #rest args)
 => (code :: <integer>)
  let mirror = widget-mirror(widget);
  debug-assert(mirror, "Unknown widget");
  let gadget = mirror-sheet(mirror);
  duim-debug-message("Handling %s for %=", name, gadget);
  let value = apply(handler_, gadget, args);
  duim-debug-message("  handled?: %=", value);
  if (instance?(value, <integer>))
    value
  elseif (value)
    $true
  else
    $false
  end
end function do-handle-gtk-signal;

/// Non-event signals

define macro signal-handler-definer
  { define signal-handler ?:name (?args:*) ?eq:token ?handler:name }
    => { signal-handler-aux ?"name",
         ?handler (?args),
  "%gtk-" ## ?name ## "-signal-handler" (?args),
  "_gtk-" ## ?name ## "-signal-handler" (?args)
       end }
end macro;
define macro signal-handler-aux
  { signal-handler-aux ?signal:expression,
      ?handler:name (?args),
      ?%handler:name (?params:*),
      ?_handler:name (?c-params)
    end }
    => { define function ?%handler (widget :: <GtkWidget*>, ?params)
    do-handle-gtk-signal(?handler, widget, ?signal, ?args)
   end;
         define C-callable-wrapper ?_handler of ?%handler
            parameter widget :: <GtkWidget*>;
           ?c-params
         end;
         register-signal-handler(?signal, ?_handler)
       }
c-params:
    { } => { }
    { ?:variable, ... } => { parameter ?variable; ... }
args:
    { } => { }
    { ?arg:name :: ?:expression, ... } => { ?arg, ... }
end macro;

define signal-handler changed (user-data :: <gpointer>)
  = gtk-changed-signal-handler;
define signal-handler activate (user-data :: <gpointer>)
  = gtk-activate-signal-handler;


/// Adjustments
define function %gtk-adjustment-value-changed-signal-handler
    (adj :: <GtkAdjustment*>, widget :: <GtkWidget*>)
  do-handle-gtk-signal(gtk-adjustment-value-changed-signal-handler,
           widget, "adjustment/value_changed", adj)
end;
define C-callable-wrapper _gtk-adjustment-value-changed-signal-handler
  of %gtk-adjustment-value-changed-signal-handler
  parameter adj :: <GtkAdjustment*>;
  parameter user-data :: <GtkWidget*>;
end;
register-signal-handler("value_changed",
      _gtk-adjustment-value-changed-signal-handler,
      key: #"adjustment/value_changed");

define function install-named-handlers
    (mirror :: <gtk-mirror>, handlers :: <sequence>, #key adjustment) => ()
  let widget = mirror-widget(mirror);
  let object = GTK-OBJECT(widget);
  duim-debug-message("Installing handlers for %=: %=",
      mirror-sheet(mirror), handlers);
  for (key :: <symbol> in handlers)
    let handler_ = fetch-signal-handler(key);
    if (handler_)
      let name     = as(<byte-string>, handler_.handler-name);
      let function = handler_.handler-function;
      let value = if (adjustment)
        g-signal-connect(adjustment, name, function, widget);
      else
        //--- Should we pass an object to help map back to a mirror?
        g-signal-connect(object, name, function, $null-gpointer);
      end;
      if (zero?(value))
  duim-debug-message("Unable to connect signal '%s'", name)
      end
    end
  end;
  gtk-widget-add-events(widget,
      logior($GDK-EXPOSURE-MASK, $GDK-LEAVE-NOTIFY-MASK,
             if (member?(#"motion_notify_event", handlers))
              logior($GDK-POINTER-MOTION-MASK, $GDK-POINTER-MOTION-HINT-MASK)
             else
              0
             end,
             if (member?(#"button_press_event", handlers))
              logior($GDK-BUTTON-PRESS-MASK, $GDK-BUTTON-RELEASE-MASK)
             else
              0
            end));
end function install-named-handlers;


/// Install event handlers

define sealed method generate-trigger-event
    (port :: <gtk-port>, sheet :: <sheet>) => ()
  let mirror = sheet-mirror(sheet);
  when (mirror)
    let widget = mirror-widget(mirror);
    ignoring("generate-trigger-event")
  end
end method generate-trigger-event;

define sealed method process-next-event
    (_port :: <gtk-port>, #key timeout)
 => (timed-out? :: <boolean>)
  //--- We should do something with the timeout
  ignore(timeout);
  gtk-main-iteration();
  #f;
end method process-next-event;


/// Event handlers

define method handle-gtk-destroy-event
    (sheet :: <sheet>, widget :: <GtkWidget*>, event :: <GdkEventAny*>)
 => (handled? :: <boolean>)
  ignoring("handle-gtk-destroy-event");
  // frame-can-quit?...
  #t
end method handle-gtk-destroy-event;

define method handle-gtk-motion-event
    (sheet :: <sheet>, widget :: <GtkWidget*>, event :: <GdkEventMotion*>)
 => (handled? :: <boolean>)
  let mirror = widget-mirror(widget);
  let sheet = mirror-sheet(mirror);
  let _port = port(sheet);
  when (_port)
    ignoring("motion modifiers");
    let (unused-widget, native-x, native-y, native-state)
    = if (event.GdkEventMotion-is-hint ~= 0)
         gdk-window-get-pointer(event.GdkEventMotion-window)
      else
        values(event.GdkEventMotion-window, event.GdkEventMotion-x, event.GdkEventMotion-y, event.GdkEventMotion-state)
      end;
    let modifiers = 0;
    let state = key-flags->button-state(native-state); 
    let (x, y)
      = untransform-position(sheet-native-transform(sheet), native-x, native-y);
    if (logand(state, logior($left-button,$middle-button,$right-button))  ~= 0)
      distribute-event(_port,
        make(<pointer-drag-event>,
          sheet: sheet,
          pointer: port-pointer(_port),
          modifier-state: modifiers,
          button: state,
          x: round(x), y: round(y)));
    else
      distribute-event(_port,
        make(<pointer-motion-event>,
          sheet: sheet,
          pointer: port-pointer(_port),
          modifier-state: modifiers,
          x: round(x), y: round(y)));
    end;
  end;
  #t
end method handle-gtk-motion-event;

define method handle-gtk-enter-event
    (sheet :: <sheet>, widget :: <GtkWidget*>, event :: <GdkEventCrossing*>)
 => (handled? :: <boolean>)
  ignore(widget);
  handle-gtk-crossing-event(sheet, event, <pointer-enter-event>)
end method handle-gtk-enter-event;

define method handle-gtk-leave-event
    (sheet :: <sheet>, widget :: <GtkWidget*>, event :: <GdkEventCrossing*>)
 => (handled? :: <boolean>)
  ignore(widget);
  handle-gtk-crossing-event(sheet, event, <pointer-exit-event>)
end method handle-gtk-leave-event;

// Watch out, because leave events show up after window have been killed!
define sealed method handle-gtk-crossing-event
    (sheet :: <sheet>, event :: <GdkEventCrossing*>,
     event-class :: subclass(<pointer-motion-event>))
 => (handled? :: <boolean>)
  let _port = port(sheet);
  when (_port)
    let native-x  = event.GdkEventCrossing-x;
    let native-y  = event.GdkEventCrossing-y;
    let state     = event.GdkEventCrossing-state;
    let modifiers = 0;  //--- Do this properly!
    let detail    = event.GdkEventCrossing-detail;
    let (x, y)
      = untransform-position(sheet-native-transform(sheet), native-x, native-y);
    distribute-event(_port,
         make(event-class,
        sheet: sheet,
        pointer: port-pointer(_port),
        kind: gtk-detail->duim-crossing-kind(detail),
        modifier-state: modifiers,
        x: x, y: y));
    #t
  end
end method handle-gtk-crossing-event;

define function gtk-detail->duim-crossing-kind
    (detail :: <integer>) => (kind :: <symbol>)
  select (detail)
    $GDK-NOTIFY-ANCESTOR          => #"ancestor";
    $GDK-NOTIFY-VIRTUAL           => #"virtual";
    $GDK-NOTIFY-INFERIOR          => #"inferior";
    $GDK-NOTIFY-NONLINEAR         => #"nonlinear";
    $GDK-NOTIFY-NONLINEAR-VIRTUAL => #"nonlinear-virtual";
  end
end function gtk-detail->duim-crossing-kind;

define method handle-gtk-button-press-event
    (sheet :: <sheet>, widget :: <GtkWidget*>, event :: <GdkEventButton*>)
 => (handled? :: <boolean>)
  handle-gtk-button-event(sheet, widget, event)
end method handle-gtk-button-press-event;

define method handle-gtk-button-release-event
    (sheet :: <sheet>, widget :: <GtkWidget*>, event :: <GdkEventButton*>)
 => (handled? :: <boolean>)
  handle-gtk-button-press-event(sheet, widget, event)
end method handle-gtk-button-release-event;

define method handle-gtk-button-event
    (sheet :: <sheet>, widget :: <GtkWidget*>, event :: <GdkEventButton*>)
 => (handled? :: <boolean>)
  let _port = port(sheet);
  when (_port)
    let native-x  = event.GdkEventButton-x;
    let native-y  = event.GdkEventButton-y;
    let button    = gtk-button->duim-button(event.GdkEventButton-button);
    let state     = event.GdkEventButton-state;
    let type      = event.GdkEventButton-type;
    let modifiers = 0;  //--- Do this!
    let event-class
      = select (type)
    $GDK-2BUTTON-PRESS  => <double-click-event>;
    $GDK-BUTTON-PRESS   => <button-press-event>;
    $GDK-BUTTON-RELEASE => <button-release-event>;
    otherwise           => #f;
  end;
    if (event-class)
      let (x, y)
  = untransform-position(sheet-native-transform(sheet), native-x, native-y);
      port-modifier-state(_port)    := modifiers;
      let pointer = port-pointer(_port);
      pointer-button-state(pointer) := button;
      distribute-event(_port,
           make(event-class,
          sheet: sheet,
          pointer: pointer,
          button: button,
          modifier-state: modifiers,
          x: round(x), y: round(y)));
      #t
    end
  end
end method handle-gtk-button-event;

define function gtk-button->duim-button
    (the-gtk-button :: <integer>) => (duim-button :: <integer>)
  select (the-gtk-button)
    1 => $left-button;
    2 => $middle-button;
    3 => $right-button;
  end
end function gtk-button->duim-button;

define method handle-gtk-expose-event
    (sheet :: <sheet>, widget :: <GtkWidget*>, event :: <GdkEventExpose*>)
 => (handled? :: <boolean>)
  let _port = port(sheet);
  when (_port)
    let area = event.GdkEventExpose-area;
    let native-x = area.GdkRectangle-x;
    let native-y = area.GdkRectangle-y;
    let native-width = area.GdkRectangle-width;
    let native-height = area.GdkRectangle-height;
    let native-transform = sheet-native-transform(sheet);
    let (x, y)
      = untransform-position(native-transform, native-x, native-y);
    let (width, height)
      = untransform-distance(native-transform, native-width, native-height);
    let region = make-bounding-box(x, y, x + width, y + height);
    distribute-event(_port,
         make(<window-repaint-event>,
        sheet:  sheet,
        region: region))
  end;
  #t
end method handle-gtk-expose-event;

/*---*** Not handling state changes yet
define xt/xt-event-handler state-change-callback
    (widget, mirror, event)
  ignore(widget);
  handle-gtk-state-change-event(mirror, event);
  #t
end xt/xt-event-handler state-change-callback;

define sealed method handle-gtk-state-change-event
    (mirror :: <gtk-mirror>, event :: x/<XEvent>) => ()
  let sheet = mirror-sheet(mirror);
  let _port = port(sheet);
  when (_port)
    let type = event.x/type-value;
    select (type)
      #"configure-notify" =>
  handle-gtk-configuration-change-event(_port, sheet, event);
      #"map-notify"       =>
  note-mirror-enabled/disabled(_port, sheet, #t);
      #"unmap-notify"     =>
  note-mirror-enabled/disabled(_port, sheet, #f);
      #"circulate-notify" => #f;
      #"destroy-notify"   => #f;
      #"gravity-notify"   => #f;
      #"reparent-notify"  => #f;
    end
  end
end method handle-gtk-state-change-event;


define xt/xt-event-handler state-change-config-callback
    (widget, mirror, event)
  ignore(widget);
  handle-gtk-state-change-config-event(mirror, event);
  #t
end xt/xt-event-handler state-change-config-callback;

define sealed method handle-gtk-state-change-config-event
    (mirror :: <gtk-mirror>, event :: x/<XEvent>) => ()
  let sheet = mirror-sheet(mirror);
  let _port = port(sheet);
  when (_port)
    let type = event.x/type-value;
    select (type)
      #"configure-notify" =>
  handle-gtk-configuration-change-event(_port, sheet, event);
      #"map-notify"       => #f;
      #"unmap-notify"     => #f;
      #"circulate-notify" => #f;
      #"destroy-notify"   => #f;
      #"gravity-notify"   => #f;
      #"reparent-notify"  => #f;
    end
  end
end method handle-gtk-state-change-config-event;


define xt/xt-event-handler state-change-no-config-callback
    (widget, mirror, event)
  ignore(widget);
  handle-gtk-state-change-no-config-event(mirror, event);
  #t
end xt/xt-event-handler state-change-no-config-callback;

define sealed method handle-gtk-state-change-no-config-event
    (mirror :: <gtk-mirror>, event :: x/<XEvent>) => ()
  let sheet = mirror-sheet(mirror);
  let _port = port(sheet);
  when (_port)
    let type = event.x/type-value;
    select (type)
      #"map-notify"       =>
  note-mirror-enabled/disabled(_port, sheet, #t);
      #"unmap-notify"     =>
  note-mirror-enabled/disabled(_port, sheet, #f);
      #"configure-notify" => #f;
      #"circulate-notify" => #f;
      #"destroy-notify"   => #f;
      #"gravity-notify"   => #f;
      #"reparent-notify"  => #f;
    end
  end
end method handle-gtk-state-change-no-config-event;
*/

define method handle-gtk-configure-event
    (sheet :: <sheet>, widget :: <GtkWidget*>, event :: <GdkEventConfigure*>)
 => (handled? :: <boolean>)
  let allocation = widget.GtkWidget-allocation;
  let native-x  = event.GdkEventConfigure-x;
  let native-y  = event.GdkEventConfigure-y;
  let native-width  = allocation.GdkRectangle-width;
  let native-height = allocation.GdkRectangle-height;
  let native-transform = sheet-native-transform(sheet);
  let (x, y)
    = untransform-position(native-transform, native-x, native-y);
  let (width, height)
    = untransform-distance(native-transform, native-width, native-height);
  let region = make-bounding-box(x, y, x + width, y + height);
  distribute-event(port(sheet),
       make(<window-configuration-event>,
      sheet:  sheet,
      region: region));
  #t
end method handle-gtk-configure-event;

define sealed method note-mirror-enabled/disabled
    (_port :: <gtk-port>, sheet :: <sheet>, enabled? :: <boolean>) => ()
  ignoring("note-mirror-enabled/disabled")
end method note-mirror-enabled/disabled;

define sealed method note-mirror-enabled/disabled
    (_port :: <gtk-port>, sheet :: <top-level-sheet>, enabled? :: <boolean>) => ()
  ignoring("note-mirror-enabled/disabled")
end method note-mirror-enabled/disabled;

define function key-flags->button-state
    (flags :: <integer>) => (button-state :: <integer>)
  let button-state :: <integer> = 0;
  when (~zero?(logand(flags, $GDK-BUTTON1-MASK)))
    button-state := logior(button-state, $left-button)
  end;
  when (~zero?(logand(flags, $GDK-BUTTON2-MASK)))
    button-state := logior(button-state, $middle-button)
  end;
  when (~zero?(logand(flags, $GDK-BUTTON3-MASK)))
    button-state := logior(button-state, $right-button)
  end;
  button-state
end function key-flags->button-state;

