Module:    internal
Author:    Nosa Omo, Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Memory Manager Integration


define thread variable *handling-low-memory?* :: <boolean> = #f;

define class <out-of-memory-condition> (<serious-condition>)
  // Condition is signalled when an allocation fails due to low memory.
  // Handlers for <out-of-memory-condition> may return, in which case
  // the failed allocation will be retried.
  // Users may define their own handlers.
  // There is a default handler with implementation-defined behaviour.
  constant slot failing-allocation-class, required-init-keyword: class:;
  constant slot failing-allocation-size, required-init-keyword: size:;
end class;


define method default-handler (cond :: <out-of-memory-condition>) => (val)
  // Leave it to dynamic client top-of-stack condition handler (if there is one)
  cerror("Retry allocation of object",
         "Out of Memory: failed allocation of object; class: %=  size: %=",
         cond.failing-allocation-class,
         cond.failing-allocation-size);
  #f;
end method;



// Signal-low-memory
// Called by the runtime when an allocation fails.
// Returns a boolean indicating permission to retry allocation via reservoir.
// If this is a first notification of low memory, then signal & retry.
// If this is a recursive notification, then permit use of reservoir.

define function signal-low-memory
    (class :: <class>, obj-size :: <integer>)
 => (permit-reservoir? :: <boolean>)
  if (*dylan-library-initialized?*)
    if (*handling-low-memory?*)
      #t
    else
      dynamic-bind (*handling-low-memory?* = #t)
        // Must ensure no allocation happens before this point
        signal(make(<out-of-memory-condition>, class: class, size: obj-size))
      end dynamic-bind;
      #f
    end if;
  else
    #t
  end if;
end function;


///
///  Keyboard Break Handling for Console Applications
///

// This condition will be signaled for Control-C events

define class <keyboard-interrupt> (<serious-condition>)
end class;


// The default handler emulates the Windows default handler
// -- exits the running process

define method default-handler (cond :: <keyboard-interrupt>) => (val)
  primitive-exit-application(integer-as-raw(0));
  // break("Keyboard Interrupt(Control-c) caught");
  #f;
end method;

// The Signaller

define function keyboard-break-handler() => ()
  if (*dylan-library-initialized?*)
    signal(make(<keyboard-interrupt>));
  end if;
end function;

// Determines if a keyboard-interrupt has occurred

define function keyboard-interrupt?() => (interrupt? :: <boolean>)
  primitive-raw-as-boolean
    (primitive-keyboard-interrupt-signaled());
end function;

// Use this to manually clear a keyboard-interrupt before
// handling it

define function keyboard-interrupt?-setter
    (interrupt? :: <boolean>) => (interrupt? :: <boolean>)
  primitive-keyboard-interrupt-signaled() :=
    primitive-boolean-as-raw(interrupt?);
  interrupt?
end function;

// Determines if keyboard-interrupts are being automatically
// polled on any active threads (default is auto-polling on all
// threads)

define function keyboard-interrupt-polling?()
 => (interrupt-polling? :: <boolean>)
  primitive-raw-as-boolean
    (primitive-keyboard-interrupt-polling());
end function;

// Use this to switch to manual polling mode,
// for example on a dedicated thread

define function keyboard-interrupt-polling?-setter
    (interrupt-polling? :: <boolean>) => (interrupt-polling? :: <boolean>)
  primitive-keyboard-interrupt-polling() :=
    primitive-boolean-as-raw(interrupt-polling?);
  interrupt-polling?
end function;

// Determines if the current-thread is auto-polling for keyboard-interrupts

define function keyboard-interrupt-polling-thread?()
 => (interrupt-polling? :: <boolean>)
  primitive-raw-as-boolean
    (primitive-keyboard-interrupt-polling-thread(handle1(current-thread())));
end function;

// Use this to switch on/off auto-polling mode for current-thread;
// useful in multi-threaded context, where one thread auto-polls while
// the other manually polls at different points

define function keyboard-interrupt-polling-thread?-setter
    (interrupt-polling? :: <boolean>) => (interrupt-polling? :: <boolean>)
  primitive-keyboard-interrupt-polling-thread(handle1(current-thread())) :=
    primitive-boolean-as-raw(interrupt-polling?);
  interrupt-polling?
end function;

