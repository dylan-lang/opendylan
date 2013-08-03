Module:       standalone-deuce-internals
Synopsis:     Standalone wrapper for DUIM-Deuce
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Initialization for Standalone Deuce

define variable *editor* :: false-or(<editor>) = #f;

define constant $initially-disabled-commands
    = vector(save-file, find-next-string,
             deuce/undo-command, deuce/redo-command,
             compile-file, load-file,
             evaluate-region, evaluate-buffer, macroexpand-region,
             describe-object, browse-object, browse-class,
             show-arglist, show-documentation);

define function start-deuce (#key reset?)
  when (~*editor* | reset?)
    *editor* := make(<deuce-editor>);
  end;
  let frame = make(<deuce-frame>,
                   width: 625, height: 650,
                   title: "Deuce",
                   editor: *editor*,
                   disabled-commands: $initially-disabled-commands);
  frame-window(frame) := frame.%window;
  //---*** Globally set these because CAPI back-end never calls 'frame-top-level'
  *editor-frame* := frame;
  let buffer = make-initial-buffer();
  *buffer* := buffer;
  dynamic-bind (*editor-frame* = frame,
                *buffer* = buffer)
    select-buffer(frame-window(frame), buffer);
    start-frame(frame)
  end
end function start-deuce;

define function ensure-deuce-started () => ()
  when (~*editor* | empty?(editor-frames(*editor*)))
    start-deuce()
  end
end function ensure-deuce-started;

ensure-deuce-started();
