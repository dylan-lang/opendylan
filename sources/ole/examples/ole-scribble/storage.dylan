Module:    scribble
Author:    Scott McKay and David Gray
Synopsis:  Simple OLE scribble application
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Persistent storage for the Scribble app as an OLE server

define method save-frame-to-storage
    (frame :: <scribble-frame>, stream :: <storage-istream>) => ()
  let pane :: <drawing-pane> = frame.surface;
  let store = make(<dood>, stream: stream);
  store.dood-root := pane.scribble-segments;
  dood-commit(store);
  dood-close(store);
end method save-frame-to-storage;

define method load-frame-from-storage
    (frame :: <scribble-frame>, stream :: <storage-istream>) => ()
  let pane :: <scribble-pane> = frame.surface;
  pane.scribble-segments.size := 0;
  let store = make(<dood>, stream: stream);
  for (segment in store.dood-root)
    add!(pane.scribble-segments, segment);
  end;
  pane.scribble-segment  := #f;
end method load-frame-from-storage;
