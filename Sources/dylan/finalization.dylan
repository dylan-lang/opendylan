Module:    finalization-internal
Synopsis:  The finalization interface
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//
// FINALIZE WHEN UNREACHABLE
//
define function finalize-when-unreachable (object :: <object>)
 => (object :: <object>)
  mm-finalize(object);
  object;
end function;


//
// FINALIZE
//
define open generic finalize (object :: <object>) => ();

// Default method for finalize which makes it safe to always call
// next-method() in user finalize methods. This also makes it safe to
// register any object for finalization even though there may be no user
// finalization method applicable for that object's class.
//
define method finalize (object :: <object>) => ()
end method;


//
// DRAIN FINALIZATION QUEUE
//
define function drain-finalization-queue () => ()
  for (object = mm-finalization-queue-first()
         then mm-finalization-queue-first(),
       while: object)
    finalize(object);
  end for;
end function;


//
// AUTOMATIC FINALIZATION
//

define variable *automatic-finalization-lock* :: <simple-lock> = make(<lock>);

define variable *automatic-finalization?* :: <boolean> = #f;
define variable *draining-thread-active?* :: <boolean> = #f;



define function automatic-finalization-enabled? () => (enabled? :: <boolean>)
  with-lock(*automatic-finalization-lock*)
    *automatic-finalization?*;
  end with-lock;
end function;


// sets the auto-finalization state to newval. The initial state is #f.
// If the state changes from #f to #t, then a new thread is created which
// regularly calls drain-finalization-queue. If the state changes from
// #t to #f, then the finalization thread will exit.
//
define function automatic-finalization-enabled?-setter (newval :: <boolean>)
 => ()

  with-lock(*automatic-finalization-lock*)
    if (*automatic-finalization?*)
      *automatic-finalization?* := newval;
    else
      if (newval)
        unless(*draining-thread-active?*)
          make(<thread>, name: "Automatic Finalization Thread",
                         function: automatic-finalization-function);
          *draining-thread-active?* := #t;
	end unless;
        *automatic-finalization?* := #t;
      end if;
    end if;
  end with-lock;
end function;


define function automatic-finalization-function () => ()
  local method continue? () => (well? :: <boolean>)
    with-lock(*automatic-finalization-lock*)
      unless (*automatic-finalization?*)
        *draining-thread-active?* := #f;
      end unless;
      *automatic-finalization?*;
    end with-lock;
  end method;

  while (continue?())
    for (object = mm-finalization-queue-first-block()
           then mm-finalization-queue-first(),
         while: object)
      finalize(object);
    end for;
  end while;
end function;


//
// WRAPPER INTERFACE FOR FINALIZATION PRIMITIVES
//
define inline-only function mm-finalize (object :: <object>) => ()
  primitive-mps-finalize(object);
end function;

define inline-only function mm-finalization-queue-first ()
 => (object :: <object>)
  let obj = primitive-mps-finalization-queue-first();
  if (primitive-machine-word-equal?
	(primitive-cast-pointer-as-raw(obj),
	 integer-as-raw(0)))
    #f;
  else
    obj;
  end if;
end function;

define inline-only function mm-finalization-queue-first-block ()
 => (object :: <object>)
  sleep(1);
  mm-finalization-queue-first();
end function;
