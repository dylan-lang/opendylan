Module:    environment-framework
Synopsis:  Environment Framework
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Frame Linking

define open abstract class <frame-linking-mixin> (<frame>)
  constant slot frame-linked-frames = make(<stretchy-vector>);
  slot frame-master-frame = #f;
end class <frame-linking-mixin>;

define method link-frames
    (master :: <frame-linking-mixin>, slave :: <frame-linking-mixin>) => ()
  let old-master = frame-master-frame(slave);
  if (old-master)
    unlink-frames(old-master, slave)
  end;
  add!(frame-linked-frames(master), slave);
  frame-master-frame(slave) := master;
  if (frame-selected-objects(master))
    note-frame-selection-updated(master)
  end
end method link-frames;

define method unlink-frames
    (master :: <frame-linking-mixin>, slave :: <frame-linking-mixin>) => ()
  remove!(frame-linked-frames(master), slave);
  frame-master-frame(slave) := #f;
end method unlink-frames;

define method note-frame-selection-updated
    (frame :: <frame-linking-mixin>) => ()
  next-method();
  let linked-frames = frame-linked-frames(frame);
  unless (empty?(linked-frames))
    let objects = frame-selected-objects(frame);
    unless (empty?(objects))
      //---*** It would be nice to be able to handle multiple objects here...
      let object = objects[0];
      do(method (frame)
           frame-primary-object(frame) := object
         end,
         linked-frames);
    end
  end
end method note-frame-selection-updated;
