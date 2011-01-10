Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Generally useful macros

define macro inc!
  { inc! (?place:expression, ?dx:expression) }
    => { ?place := ?place + ?dx; }
  { inc! (?place:expression) }
    => { ?place := ?place + 1; }
end macro inc!;

define macro dec!
  { dec! (?place:expression, ?dx:expression) }
    => { ?place := ?place - ?dx; }
  { dec! (?place:expression) }
    => { ?place := ?place - 1; }
end macro dec!;


define macro max!
  { max! (?place:expression, ?others:*) }
    => { ?place := max(?place, ?others) }
end macro max!;

define macro min!
  { min! (?place:expression, ?others:*) }
    => { ?place := min(?place, ?others) }
end macro min!;


define macro swap!
  { swap! (?place1:expression, ?place2:expression) }
    => { begin
	   let _value = ?place1;
	   ?place1 := ?place2;
	   ?place2 := _value 
	 end }
end macro swap!;


// 'push!' and 'pop!' are intended to be called only on lists
define macro push!
  { push! (?list:expression, ?item:expression) }
    => { ?list := add!(?list, ?item) }
end macro push!;

// NB: this returns #() -- not #f -- when the list is empty!
define macro pop!
  { pop! (?list:expression) }
    => { begin
           let _result = head(?list);
           ?list := tail(?list);
           _result
	 end }
end macro pop!;


/// Protocols

define macro protocol-definer
  //--- We don't use the name or supers yet...
  { define protocol ?:name (?supers:*) ?slots-and-generics:* end }
    => { ?slots-and-generics }
 slots-and-generics:
  { } => { }
  { ?slot-or-generic:*; ... }
    => { ?slot-or-generic; ... }
 slot-or-generic:
  { getter ?getter-name:name ?getter-arglist:* => ?values:* }
    => { define open generic ?getter-name ?getter-arglist => ?values }
  { getter ?getter-name:name ?getter-arglist:* }
    => { define open generic ?getter-name ?getter-arglist }
  { setter ?setter-name:name ?setter-arglist:* => ?values:* }
    => { define open generic ?setter-name ?setter-arglist => ?values }
  { setter ?setter-name:name ?setter-arglist:* }
    => { define open generic ?setter-name ?setter-arglist }
  { function ?function-name:name ?function-arglist:* => ?values:* }
    => { define open generic ?function-name ?function-arglist => ?values }
  { function ?function-name:name ?function-arglist:* }
    => { define open generic ?function-name ?function-arglist }
end macro protocol-definer;


/// Node locking

define macro with-node-locked
  { with-node-locked (?node:expression) ?:body end }
    => { with-thing-locked (?node, node-lock) ?body end }
end macro with-node-locked;

define macro with-buffer-locked
  { with-buffer-locked (?buffer:expression) ?:body end }
    => { with-thing-locked (?buffer, buffer-lock) ?body end }
end macro with-buffer-locked;

define macro with-editor-locked
  { with-editor-locked (?editor:expression) ?:body end }
    => { with-thing-locked (?editor, editor-lock) ?body end }
end macro with-editor-locked;

define macro with-window-locked
  { with-window-locked (?window:expression) ?:body end }
    => { with-thing-locked (?window, window-lock) ?body end }
end macro with-window-locked;

define macro with-thing-locked
  { with-thing-locked (?thing:expression, ?accessor:name) ?:body end }
    => { begin
	   let _thing = ?thing;
	   let _lock  = _thing & ?accessor(_thing);
	   block ()
	     when (_lock) wait-for(_lock) end;
	     ?body
	   cleanup
	     when (_lock) release(_lock) end;
	   end
	 end }
end macro with-thing-locked;


/// Editor state binding

define macro with-editor-state-bound
  { with-editor-state-bound (?buffer:variable = ?window:expression) ?:body end }
    => { begin
	   let _frame  = window-frame(?window);
	   let ?buffer = frame-buffer(_frame);
	   dynamic-bind (*editor-frame* = _frame,
			 *buffer*       = ?buffer)
	     ?body
	   end
	 end }
  { with-editor-state-bound (?window:expression) ?:body end }
    => { begin
	   let _frame  = window-frame(?window);
	   let _buffer = frame-buffer(_frame);
	   dynamic-bind (*editor-frame* = _frame,
			 *buffer*       = _buffer)
	     ?body
	   end
	 end }
end macro with-editor-state-bound;


// Iterate over the windows associated with the given frame's editor.
// If the frame is #f, this does nothing.
define macro do-associated-windows
  { do-associated-windows (?window:variable = ?frame:expression) ?:body end }
    => { begin
	   let _frame = ?frame;
	   when (_frame)
	     for (?window in editor-windows(frame-editor(_frame)))
	       ?body
	     end
	   end
	 end }
end macro do-associated-windows;

// Iterate over the buffers associated with the given frame's editor.
// If the frame is #f, this does nothing.
define macro do-associated-buffers
  { do-associated-buffers (?buffer:variable = ?frame:expression) ?:body end }
    => { begin
	   let _frame = ?frame;
	   when (_frame)
	     for (?buffer in editor-buffers(frame-editor(_frame)))
	       ?body
	     end
	   end
	 end }
end macro do-associated-buffers;
