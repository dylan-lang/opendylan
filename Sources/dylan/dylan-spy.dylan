module:        internal
synopsis:      Functions called from the debugger.
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// ********************** Remote Object Registration *********************

// The idea behind this is as follows:
// Any dylan values (ie pointers) that become known to the debugger while
// the target is stopped can only be considered valid while the target
// _remains_ stopped. In the presence of a relocating garbage collector,
// there is every chance that a pointer will fall out of date while the
// application runs. 

// The debugger may occasionally require a persistent handle on an object,
// via which the object's current value can always be obtained. In
// debugger terminology, this is called "remotely registering" (or
// sometimes "tracking") the object.

// Remote object registration can be "strong" or "weak". An object that
// is strongly registered continues to be tracked even when the target
// itself ceases to reference it. A weakly registered object can be
// reclaimed by the garbage collector if it not otherwise referenced
// by the target. To implement this, the SPY contains two dylan tables,
// one strong and one weak.

// The debugger registers objects by remote calling functions in the SPY,
// which install the objects into the relevant table, and return integer
// keys. The keys, being integers, remain valid through any amount of
// garbage collection work. And, since the objects are stored in dylan
// tables, the memory manager will keep their references up to date.


///// *SPY-STRONGLY-REGISTERED-OBJECTS*
//    The table of objects that are being tracked by the debugger.

define constant *spy-strongly-registered-objects* :: <table> = make(<table>);


///// *SPY-WEAKLY-REGISTERED-OBJECTS*
//    The table of objects that are being weakly tracked by the debugger.

define constant *spy-weakly-registered-objects* :: <table> = make(<table>);


///// *SPY-GLOBAL-REGISTRATION-COOKIE*
//    Is returned as the handle on a registered object. Incremented each
//    time an object is registered, either strongly or weakly. Since this
//    just ticks up and up and up, there is an implementational limit
//    on the number of objects that can be tracked. However, this limit
//    should be far and away above the number of registrations that will
//    ever be performed in practice. I think we are justified in completely
//    ignoring this limit.

define variable *spy-global-registration-cookie* :: <integer> = 0;


///// SPY-REGISTER-REMOTE-OBJECT
//    Adds an object into the table of strongly tracked objects. Returns
//    the integer key.

define method spy-register-remote-object (x :: <object>) => (i :: <integer>)
  let cookie :: <integer> = *spy-global-registration-cookie*;
  *spy-global-registration-cookie* :=
     *spy-global-registration-cookie* + 1;
  // Inline this to avoid dispatch (why doesn't the compiler inline it?)
  puthash(x, *spy-strongly-registered-objects*, cookie);
  cookie;
end method;


///// SPY-REGISTER-WEAK-REMOTE-OBJECT
//    Adds an object into the table of weakly tracked objects. Returns
//    the integer key.

define method spy-register-weak-remote-object (x :: <object>) 
        => (i :: <integer>)
  let cookie :: <integer> = *spy-global-registration-cookie*;
  *spy-global-registration-cookie* :=
     *spy-global-registration-cookie* + 1;
  // Inline this to avoid dispatch (why doesn't the compiler inline it?)
  puthash(x, *spy-weakly-registered-objects*, cookie);
  cookie;
end method;


///// SPY-REMOTE-OBJECT-VALUE
//    Returns a strongly-tracked object's value via its integer key.

define method spy-remote-object-value (i :: <integer>) => (x :: <object>)
  // Inline this to avoid dispatch (why doesn't the compiler inline it?)
  gethash(*spy-strongly-registered-objects*, i, #f, #t)
end method;


///// SPY-WEAK-REMOTE-OBJECT-VALUE
//    Returns a weakly-tracked object's value via its integer key.

define method spy-weak-remote-object-value (i :: <integer>) => (x :: <object>)
  // Inline this to avoid dispatch (why doesn't the compiler inline it?)
  gethash(*spy-weakly-registered-objects*, i, #f, #t)
end method;


///// SPY-FREE-REMOTE-OBJECT
//    Explicitly releases an object that is no longer required to be
//    tracked.

define method spy-free-remote-object (i :: <integer>) => ()
  remove-key!(*spy-strongly-registered-objects*, i);
end method;


///// SPY-WEAK-FREE-REMOTE-OBJECT
//    Explicitly releases an object that is no longer required to be
//    weakly tracked.

define method spy-weak-free-remote-object (i :: <integer>) => ()
  remove-key!(*spy-weakly-registered-objects*, i);
end method;



// ***************************** Restarts *******************************


define method spy-restart-decline-continuation (#rest arguments) => ()
  // What else??
  error("The debugger did not expect a restart to decline");
end method;


///// SPY-INVOKE-NUMBERED-RESTART
//    Invokes a restart that matches the given numeric index. In the
//    list of active handlers, restarts are numbered from 1.

define method spy-invoke-numbered-restart (index :: <integer>) => ()
  let this-index = 1;
  do-handlers
    (method (type, test, handling-function, init-arguments)
       if (subtype?(type, <restart>))
         if (this-index == index)
           let restart-instance = 
             if (subtype?(type, <abort>))
               make(type)
             else
               apply(make, type, init-arguments)
             end if;
           restart-query(restart-instance);
           handling-function
             (restart-instance, spy-restart-decline-continuation);
         end if;
         this-index := this-index + 1;
       end if;
     end method);

  // If we get here, the debugger has asked the spy to call a restart that
  // does not exist according to the numering convention. This can only
  // mean that the debugger's internal implementation is screwed, and
  // we are well justified signalling an error!

  error("Internal SPY error.");
end method;


///// *CURRENT-INTERACTOR-LEVEL*
//    A dynamically bindable variable which contains the interactor
//    level, as used to number restarts.

define thread variable *current-interactor-level* :: <integer> = 0;


///// SPY-INVOKE-DYLAN-UNDER-CODED-RESTART
//    Invokes an arbitrary dylan function with arbitrary arguments and
//    returns its values. However, it installs a restart handler around
//    the function call. The debugger will be able to invoke this
//    restart if the function execution errors.
//    The restart's description will be of the form "Return to level X",
//    where X is an integer. This integer might be supplied from the 
//    debugger by passing a non-negative value for INTERACTOR-LEVEL. 
//    If a negative value is passed, the current level is simply
//    incremented.

define method spy-invoke-dylan-under-coded-restart
   (interactor-level :: <integer>, func :: <function>, #rest arguments)
       => (#rest r)
  let new-level 
    = if (interactor-level < 0)
	*current-interactor-level* + 1;
      else interactor-level;
      end if;
  dynamic-bind (*current-interactor-level* = new-level)
    block (exit)
      // NB:
      // Although the <abort> class is not permitted to take init-arguments,
      // we still ensure their presence in the _handler_ so that the
      // debugger can describe the abort in a meaningful way. When
      // invoking an <abort>, we ensure that no init-arguments are passed.
      let handler (<abort>,
                   init-arguments: 
		   vector(format-string: "Abort interactive level %d",
			  format-arguments: vector(new-level)))
         = method (condition, nxt)
             exit()
           end method;
  
      apply(func, arguments);
    end block;
  end dynamic-bind;
end method;


///// SPY-RESOLVE-KEYWORD
//    Turns a string into a keyword...

define method spy-resolve-keyword
   (keyword-string :: <byte-string>) => (sym :: <symbol>)
  as(<symbol>, keyword-string);
end method;


///// SPY-FORMAT-STRING-KEYWORD
//    Specialized argument-free spy function to return the #"format-string"
//    keyword.

define method spy-format-string-keyword () => (sym :: <symbol>)
  #"format-string"
end method;


///// SPY-FORMAT-ARGUMENTS-KEYWORD
//    Specialized argument-free spy function to return the #"format-arguments"
//    keyword.

define method spy-format-arguments-keyword () => (sym :: <symbol>)
  #"format-arguments"
end method;


///// SPY-CREATE-APPLICATION-THREAD
//    This is used to generate a new dylan thread in the application.
//    The thread will start up in the normal way, and then arrange to
//    stop at a breakpoint. 
//    Once the breakpoint is reached, the thread is at an interactive
//    location, and any amount of interactive dylan code can be run on it.
//    When the user chooses the appropriate restart, it will terminate.
//    The name of the thread must also be specified by the debugger.
//    The thread object itself will be returned by this function.

define method spy-create-application-thread
    (name :: <byte-string>) 
  => (new-thread :: <thread>)
  make(<synchronous-thread>,
       name: name,
       function: method ()
		   block ()
		     let message :: <byte-string> = "Beginning %=";
		     while (#t)
		       %break(message, name);
		       message := 
			 "Unexpected continuation on special thread %=";
		     end;
		   exception (<simple-restart>,
			      init-arguments:
				vector(format-string: "Terminate %=", 
				       format-arguments: list(name)))
		   end block;
                 end method)
end method;


