Module:    functional-dylan-internals
Author:    Paul Haahr
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// repeatable
///
///   repeatable name (variable = init, ...)
///     body
///   end repeatable
///
/// Evaluates body, with the variables bound to the inits.  Any call
/// to name inside the body terminates the current execution of the
/// body and repeats from the start, with the variables bound to the
/// arguments of the call.  (Similar to iterate, but the call to
/// restart the loop doesn't have to be in tail position.)

define macro repeatable
  { repeatable ?restart:name (?args:*) ?:body end }
  => { repeatable-aux ?restart (?args) (?args) ?body end }
end macro repeatable;

define macro repeatable-aux
  { repeatable-aux ?restart:name (?args) (?inits)
      ?:body
    end }
  => { block (exit)
         local method loop (?args)
		 let (#rest restart-args)
		   = block (?restart)
		       let (#rest body-results) = ?body;
		       apply(exit, body-results)
		     end block;
		 apply(loop, restart-args)
	       end loop;
         loop(?inits)
       end block }
 args:
  { }                                => { }
  { ?:variable = ?:expression, ... } => { ?variable, ... }
 inits:
  { }                                => { }
  { ?:variable = ?:expression, ... } => { ?expression, ... }
end macro repeatable-aux;


/// Simple timing macro

define macro timing
  { timing ()
      ?body:body
    end }
    => { primitive-start-timer();
	 ?body;
	 let elapsed-time = primitive-stop-timer();
	 values(elapsed-time[0], elapsed-time[1]) }
end macro timing;
