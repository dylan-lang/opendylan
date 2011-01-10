Module:       duim-utilities
Synopsis:     DUIM utilities
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// First some generally useful macros...

define macro swap!
  { swap! (?place1:expression, ?place2:expression) }
    => { begin
	   let _value = ?place1;
	   ?place1 := ?place2;
	   ?place2 := _value 
	 end }
end macro swap!;


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


define macro min!
  { min! (?place:expression, ?others:*) }
    => { ?place := min(?place, ?others); }
end macro min!;

define macro max!
  { max! (?place:expression, ?others:*) }
    => { ?place := max(?place, ?others); }
end macro max!;


define macro destructuring-let
  { destructuring-let ((?pattern:*) = ?value:expression) ?:body end }
    => { begin
	   let _destructuring-let-body = method (?pattern) ?body end;
	   apply(_destructuring-let-body, ?value)
	 end }
end macro destructuring-let;


// 'push!' and 'pop!' are intended to be called only on lists
define macro push!
  { push! (?list:expression, ?item:expression) }
    => { ?list := add!(?list, ?item) }
end macro push!;

define macro pop!
  { pop! (?list:expression) }
    => { begin
           let _result = head(?list);
           ?list := tail(?list);
           _result
	 end }
end macro pop!;


/// Conditions and restarts

define macro with-restart
  { with-restart (?condition:expression, #rest ?initargs:*)
      ?:body
    end }
    => { block ()
	   ?body
	 exception (?condition, init-arguments: vector(?initargs))
	   values(#f, #t)
	 end }
end macro with-restart;

define macro with-simple-restart
  { with-simple-restart (?format-string:expression, ?format-args:*)
      ?:body
    end }
    => { with-restart (<simple-restart>,
		       format-string: ?format-string,
		       format-arguments: vector(?format-args))
	   ?body
         end }
end macro with-simple-restart;

define macro simple-restart-loop
  { simple-restart-loop (?format-string:expression, ?format-args:*)
      ?:body
    end }
    => { with-simple-restart (?format-string, ?format-args)
	   while (#t)
	     ?body
	   end
         end }
end macro simple-restart-loop;

define macro with-abort-restart
  { with-abort-restart ()
      ?:body
    end }
  =>
  { block ()
      ?body
    exception (<abort>)
      values(#f, #t)
    end }
end macro with-abort-restart;

define macro with-abort-restart-loop
  { with-abort-restart-loop (?format-string:expression, ?format-args:*)
      ?:body
    end }
  =>
  { with-simple-restart (?format-string, ?format-args)
      while (#t)
        with-abort-restart ()
          ?body
        end
      end
    end }
end macro with-abort-restart-loop;


///--- Declarations :-(

define inline function dynamic-extent (#rest args) => ()
  args;
  #f
end function dynamic-extent;


///--- Stack allocation

define macro with-stack-list
  { with-stack-list (?:name = ?elements:*) ?:body end }
    => { begin
           let ?name :: <list> = list(?elements);
           ?body
         end }
 elements:
  { } => { }
  { ?:expression, ... } => { ?expression, ... }
end macro with-stack-list;

define inline function evacuate-list (_list) => (_list)
  _list
end function evacuate-list;


define macro with-stack-vector
  { with-stack-vector (?:name = ?elements:*) ?:body end }
    => { begin
           let ?name :: <simple-object-vector> = vector(?elements);
           ?body
         end }
 elements:
  { } => { }
  { ?:expression, ... } => { ?expression, ... }
end macro with-stack-vector;

define inline function evacuate-vector (_vector) => (_vector)
  _vector
end function evacuate-vector;


define macro with-stack-object
  { with-stack-object (?:name :: ?type:expression, #rest ?initargs:*) ?:body end }
    => { begin
           let ?name :: ?type = make(?type, ?initargs);
           ?body
         end }
end macro with-stack-object;

define inline function evacuate-object (_object) => (_object)
  _object
end function evacuate-object;


/// Additional thread support

// Per-thread (i.e., dynamically bindable) slots
//--- It would be nice if Threads did this natively...
//---*** WEBSTER 9000 BUG: Defining the method as inline kills the compiler!
define macro thread-slot-definer
  { define thread-slot ?:name :: ?type:expression of ?class:expression}
    => { define /* inline */ method ?name ## "-dynamic-binder"
	     (new :: ?type, continuation :: <function>, object :: ?class) => (#rest values)
	   let old = object.?name;
	   block ()
	     object.?name := new;
	     continuation()
	   cleanup
	     object.?name := old;
	   end
	 end method }
  { define thread-slot ?:name :: ?type:expression}
    => { define thread-slot ?name :: ?type of <object> }
  { define thread-slot ?:name of ?class:expression}
    => { define thread-slot ?name :: <object> of ?class }
  { define thread-slot ?:name }
    => { define thread-slot ?name :: <object> of <object> }
end macro thread-slot-definer;

//--- Why Tony won't provide this is beyond me...
define method destroy-thread (thread)
  #f
end method destroy-thread;
