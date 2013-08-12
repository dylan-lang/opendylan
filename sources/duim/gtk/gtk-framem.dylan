Module:       gtk-duim
Synopsis:     GTK frame manager implementation
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// GTK frame management

define sealed class <gtk-frame-manager> (<basic-frame-manager>)
end class <gtk-frame-manager>;

define sealed domain make (singleton(<gtk-frame-manager>));
define sealed domain initialize (<gtk-frame-manager>);

define sealed method make-frame-manager
    (_port :: <gtk-port>,
     #key palette, class = <gtk-frame-manager>, #all-keys)
 => (framem :: <frame-manager>)
  make(class, port: _port, palette: palette)
end method make-frame-manager;


define method note-frame-title-changed
    (framem :: <gtk-frame-manager>, frame :: <frame>) => ()
  // Update the title in the top-level window
  let sheet  = top-level-sheet(frame);
  let mirror = sheet & sheet-direct-mirror(sheet);
  when (mirror)
    let widget = mirror-widget(mirror);
    let title   = frame-title(frame) | "";
    with-c-string (c-string = title)
      with-gdk-lock
        gtk-window-set-title(widget, c-string)
      end
    end
  end
end method note-frame-title-changed;


define method note-frame-icon-changed
    (framem :: <gtk-frame-manager>, frame :: <frame>) => ()
  // Update the icon in the top-level window
  let sheet  = top-level-sheet(frame);
  let mirror = sheet & sheet-direct-mirror(sheet);
  when (mirror)
    update-mirror-icon(mirror, frame-icon(frame))
  end
end method note-frame-icon-changed;

define method update-mirror-icon
    (mirror :: <top-level-mirror>, icon :: false-or(<image>)) => ()
  ignoring("update-mirror-icon")
end method update-mirror-icon;


define method do-frame-occluded?
    (framem :: <gtk-frame-manager>, frame :: <basic-frame>)
 => (occluded? :: <boolean>)
  ignoring("do-frame-occluded?");
  #f
end method do-frame-occluded?;


define method note-frame-enabled
    (framem :: <gtk-frame-manager>, frame :: <basic-frame>) => ()
  ignoring("note-frame-enabled")
end method note-frame-enabled;

define method note-frame-disabled
    (framem :: <gtk-frame-manager>, frame :: <basic-frame>) => ()
  ignoring("note-frame-disabled")
end method note-frame-disabled;


define sealed method note-frame-iconified
    (framem :: <gtk-frame-manager>, frame :: <simple-frame>) => ()
  next-method();                                // update the frame's state
  let sheet  = top-level-sheet(frame);
  let mirror = sheet & sheet-direct-mirror(sheet);
  when (mirror)
    ignoring("note-frame-iconified")
  end
end method note-frame-iconified;

define sealed method note-frame-deiconified
    (framem :: <gtk-frame-manager>, frame :: <simple-frame>) => ()
  next-method();                                // update the frame's state
  let sheet  = top-level-sheet(frame);
  let mirror = sheet & sheet-direct-mirror(sheet);
  when (mirror)
    ignoring("note-frame-deiconified")
  end
end method note-frame-deiconified;
