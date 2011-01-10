Module:       DUIM-Recording-Internals
Synopsis:     DUIM output recording
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// 'tracking-pointer'

//---*** Perhaps this belongs in the sheets layer?
define macro tracking-pointer
  { tracking-pointer (?event:name, ?sheet:expression, #rest ?options:expression) ?clauses:* end }
    => { begin
	   local method tracking-pointer-clauses (?event)
	     select (?event by instance?)
	       ?clauses;
               otherwise => #f;
	     end
	   end method;
	   do-tracking-pointer(?sheet, tracking-pointer-clauses, ?options)
	 end }
  { tracking-pointer (?sheet:expression, #rest ?options:expression) ?clauses:* end }
    => { begin
	   local method tracking-pointer-clauses (_event)
	     select (_event by instance?)
	       ?clauses;
               otherwise => #f;
	     end
	   end method;
	   do-tracking-pointer(?sheet, tracking-pointer-clauses, ?options)
	 end }
 clauses:
  { } => { }
  { ?clause:*; ... }
    => { ?clause; ... }
 clause:
  { ?event-class:name => ?body:* }
    => { ?event-class => ?body }
end macro tracking-pointer;


/// The pointer-tracking event handler

define sealed class <tracking-event-handler> (<event-handler>)
  sealed constant slot %function :: <function>,
    required-init-keyword: function:;
end class <tracking-event-handler>;

// Pass unhandled events to the original event handler
define method handle-event
    (handler :: <tracking-event-handler>, event :: <event>) => ()
  handle-event(event-client(event), event)
end method handle-event;

define method handle-event
    (handler :: <tracking-event-handler>, event :: <pointer-motion-event>) => ()
  handler.%function(event)
end method handle-event;

define method handle-event
    (handler :: <tracking-event-handler>, event :: <pointer-button-event>) => ()
  handler.%function(event)
end method handle-event;

define method handle-event
    (handler :: <tracking-event-handler>, event :: <keyboard-event>) => ()
  handler.%function(event)
end method handle-event;


/// The guts of 'tracking-pointer'

define open generic do-tracking-pointer
    (sheet :: <abstract-sheet>, clauses :: <function>,
     #rest options,
     #key multiple-windows?, #all-keys)
 => (#rest values);

//---*** Implement 'multiple-windows?'
//---*** Support presentations, highlighting...
define sealed method do-tracking-pointer
    (sheet :: <basic-sheet>, clauses :: <function>,
     #key multiple-windows? = #t)
 => (#rest values)
  let old-handler = event-handler(sheet);
  let new-handler = make(<tracking-event-handler>, function: clauses);
  block ()
    //---*** This should probably do 'with-pointed-grabbed', right?
    event-handler(sheet) := new-handler;
    let top-sheet = top-level-sheet(sheet-frame(sheet));
    while (#t)
      let event = read-event(top-sheet);
      handle-event(new-handler, event);
    end;
  cleanup
    event-handler(sheet) := old-handler;
  end
end method do-tracking-pointer;


/// 'dragging-output'

//---*** Do this
