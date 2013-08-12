Module:       gtk-duim
Synopsis:     GTK event processing implementation
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

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
  with-gdk-lock
    gtk-main-iteration();
  end;
  #f;
end method process-next-event;


/// Event handlers

define method handle-gtk-destroy-event
    (sheet :: <sheet>, widget :: <GtkWidget>, event :: <GdkEventAny>)
 => (handled? :: <boolean>)
  ignoring("handle-gtk-destroy-event");
  // frame-can-quit?...
  #t
end method handle-gtk-destroy-event;

define method handle-gtk-motion-event
    (sheet :: <sheet>, event :: <GdkEventMotion>)
 => (handled? :: <boolean>)
  let _port = port(sheet);
  when (_port)
    ignoring("motion modifiers");
    let (unused-widget, native-x, native-y, native-state)
    = if (event.gdk-event-motion-is-hint ~= 0)
         gdk-window-get-pointer(event.gdk-event-motion-window)
      else
        values(event.gdk-event-motion-window, event.gdk-event-motion-x, event.gdk-event-motion-y, event.gdk-event-motion-state)
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
    (sheet :: <sheet>, widget :: <GtkWidget>, event :: <GdkEventCrossing>)
 => (handled? :: <boolean>)
  ignore(widget);
  handle-gtk-crossing-event(sheet, event, <pointer-enter-event>)
end method handle-gtk-enter-event;

define method handle-gtk-leave-event
    (sheet :: <sheet>, widget :: <GtkWidget>, event :: <GdkEventCrossing>)
 => (handled? :: <boolean>)
  ignore(widget);
  handle-gtk-crossing-event(sheet, event, <pointer-exit-event>)
end method handle-gtk-leave-event;

// Watch out, because leave events show up after window have been killed!
define sealed method handle-gtk-crossing-event
    (sheet :: <sheet>, event :: <GdkEventCrossing>,
     event-class :: subclass(<pointer-motion-event>))
 => (handled? :: <boolean>)
  let _port = port(sheet);
  when (_port)
    let native-x  = event.gdk-event-crossing-x;
    let native-y  = event.gdk-event-crossing-y;
    let state     = event.gdk-event-crossing-state;
    let modifiers = 0;  //--- Do this properly!
    let detail    = event.gdk-event-crossing-detail;
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

define method handle-gtk-button-event
    (sheet :: <sheet>, event :: <GdkEventButton>)
 => (handled? :: <boolean>)
  let _port = port(sheet);
  when (_port)
    let native-x  = event.gdk-event-button-x;
    let native-y  = event.gdk-event-button-y;
    let button    = gtk-button->duim-button(event.gdk-event-button-button);
    let state     = event.gdk-event-button-state;
    let type      = event.gdk-event-button-type;
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
        = untransform-position(sheet-native-transform(sheet),
                               native-x, native-y);
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
    (sheet :: <sheet>, event :: <GdkEventExpose>)
 => (handled? :: <boolean>)

  let _port = port(sheet);
  when (_port)
    let area = event.gdk-event-expose-area;
    let native-x = area.cairo-rectangle-int-x;
    let native-y = area.cairo-rectangle-int-y;
    let native-width = area.cairo-rectangle-int-width;
    let native-height = area.cairo-rectangle-int-height;
    let native-transform = sheet-native-transform(sheet);
    let (x, y)
      = untransform-position(native-transform, native-x, native-y);
    let (width, height)
      = untransform-distance(native-transform, native-width, native-height);
    let region = make-bounding-box(x, y, x + width, y + height);
    distribute-event(_port,
                     make(<window-repaint-event>,
                          sheet:  sheet,
                          region: region));
    #f
  end;
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
    (sheet :: <sheet>, widget :: <GtkWidget>, event :: <GdkEventConfigure>)
 => (handled? :: <boolean>)
  with-stack-structure (allocation :: <cairoRectangleInt>)
    gtk-widget-get-allocation(widget, allocation);
    let native-x  = event.gdk-event-configure-x;
    let native-y  = event.gdk-event-configure-y;
    let native-width  = allocation.cairo-rectangle-int-width;
    let native-height = allocation.cairo-rectangle-int-height;
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
  end with-stack-structure;
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

