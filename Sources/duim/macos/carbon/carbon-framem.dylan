Module:       carbon-duim
Synopsis:     Macintosh frame manager implementation
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// MAC frame management

define sealed class <carbon-frame-manager> (<basic-frame-manager>)
end class <carbon-frame-manager>;

define sealed domain make (singleton(<carbon-frame-manager>));
define sealed domain initialize (<carbon-frame-manager>);

define sealed method make-frame-manager
    (_port :: <carbon-port>,
     #key palette, class = <carbon-frame-manager>, #all-keys)
 => (framem :: <frame-manager>)
  make(class, port: _port, palette: palette)
end method make-frame-manager;


define method note-frame-title-changed
    (framem :: <carbon-frame-manager>, frame :: <frame>) => ()
  // Update the title in the top-level window
  let sheet  = top-level-sheet(frame);
  let mirror = sheet & sheet-direct-mirror(sheet);
  when (mirror)
    ignoring("note-frame-title-changed")
  end
end method note-frame-title-changed;


define method note-frame-icon-changed
    (framem :: <carbon-frame-manager>, frame :: <frame>) => ()
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
    (framem :: <carbon-frame-manager>, frame :: <basic-frame>)
 => (occluded? :: <boolean>)
  ignoring("do-frame-occluded?");
  #f
end method do-frame-occluded?;


define method note-frame-enabled
    (framem :: <carbon-frame-manager>, frame :: <basic-frame>) => ()
  ignoring("note-frame-enabled")
end method note-frame-enabled;

define method note-frame-disabled
    (framem :: <carbon-frame-manager>, frame :: <basic-frame>) => ()
  ignoring("note-frame-disabled")
end method note-frame-disabled;


define sealed method note-frame-iconified
    (framem :: <carbon-frame-manager>, frame :: <simple-frame>) => ()
  next-method();				// update the frame's state
  let sheet  = top-level-sheet(frame);
  let mirror = sheet & sheet-direct-mirror(sheet);
  when (mirror)
    ignoring("note-frame-iconified")
  end
end method note-frame-iconified;

define sealed method note-frame-deiconified
    (framem :: <carbon-frame-manager>, frame :: <simple-frame>) => ()
  next-method();				// update the frame's state
  let sheet  = top-level-sheet(frame);
  let mirror = sheet & sheet-direct-mirror(sheet);
  when (mirror)
    ignoring("note-frame-deiconified")
  end
end method note-frame-deiconified;
