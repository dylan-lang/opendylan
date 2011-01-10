module:        dm-internals
synopsis:      Debug-point specialization for function tracing
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <TRACEPOINT>
//    The superclass of all debug-points to do with function tracing.

define abstract class <tracepoint> (<debug-point>)
end class;


///// <ENTRY-TRACEPOINT>
//    The superclass of all debug-points used to trace entry into
//    functions.

define open abstract class <entry-tracepoint> (<tracepoint>)
  constant slot return-callback :: <function>,
    required-init-keyword: return-callback:;
end class;


///// <SIMPLE-ENTRY-TRACEPOINT>
//    Ensures that <entry-tracepoint> is a bit instantiable.

define class <simple-entry-tracepoint>
                  (<entry-tracepoint>)
end class;

define method make 
  (class == <entry-tracepoint>, #rest keys, #key, #all-keys)
    => (entry-tracepoint)
  apply (make, <simple-entry-tracepoint>, keys)
end method;


///// <RETURN-TRACEPOINT>
//    The superclass of all debug-points used to trace return from
//    functions.

define open abstract class <return-tracepoint> (<tracepoint>)

  constant slot relevant-frame :: <remote-value>,
    required-init-keyword: frame:;

  constant slot relevant-thread :: <remote-thread>,
    required-init-keyword: thread:;

  constant slot corresponding-entry-tracepoint :: <entry-tracepoint>,
    required-init-keyword: entry:;

end class;


///// <SIMPLE-RETURN-TRACEPOINT>
//    Ensures that <return-tracepoint> is a bit instantiable.

define class <simple-return-tracepoint>
                      (<return-tracepoint>)
end class;

define method make 
    (class == <return-tracepoint>, #rest keys, #key, #all-keys)
      => (return-tracepoint)
  apply (make, <simple-return-tracepoint>, keys);
end method;


///// MAKE-RETURN-TRACEPOINT (and default method).
//    This is called to create a general instance of <return-tracepoint>
//    in order to register it when an <entry-tracepoint> is encountered.

define open generic make-return-tracepoint
   (app :: <debug-target>, bp :: <entry-tracepoint>,
    thr :: <remote-thread>, #rest keys, #key, #all-keys)
      => (return-point :: <return-tracepoint>);

define method make-return-tracepoint
   (app :: <debug-target>, bp :: <entry-tracepoint>,
    thr :: <remote-thread>, #rest keys, #key, #all-keys)
      => (return-point :: <return-tracepoint>)
  apply (make, <simple-return-tracepoint>, keys);
end method;


///// INITIALIZE-RETURN-TRACEPOINT (and default method).
//    This is called to initialize a <return-tracepoint> once it has
//    been created by make-return-tracepoint

define open generic initialize-return-tracepoint
    (app :: <debug-target>, bp :: <return-tracepoint>,
     thr :: <remote-thread>, #key, #all-keys)
       => ();

define method initialize-return-tracepoint
    (app :: <debug-target>, bp :: <return-tracepoint>,
     thr :: <remote-thread>, #key, #all-keys)
       => ();
  register-debug-point (app, bp);
end method;


///// HANDLE-DEBUG-POINT-EVENT (<ENTRY-TRACEPOINT>)
//    When an <entry-tracepoint> is encountered, the appropriate 
//    <return-tracepoint> needs to be created and registered. The
//    callback for the <entry-tracepoint> also has to be invoked,
//    hence this method uses next-method().

define method handle-debug-point-event 
    (app :: <debug-target>, bp :: <entry-tracepoint>,
     thr :: <remote-thread>) 
       => (interested? :: <boolean>)

  // We have just entered the trace callee.

  let trace-callee-frame 
    = initialize-stack-trace (app.debug-target-access-path, thr);
  let trace-caller-frame
    = previous-frame (app.debug-target-access-path, trace-callee-frame);
  let ptr 
    = frame-pointer (app.debug-target-access-path, trace-caller-frame);
  let ret-addr 
    = frame-return-address (app.debug-target-access-path, trace-callee-frame);
  let return-point = 
    make-return-tracepoint (app, bp, thr,
                            address: ret-addr,
                            callback: bp.return-callback,
                            thread: thr,
                            frame: ptr,
                            entry: bp);
  initialize-return-tracepoint (app, return-point, thr);
  next-method ();
end method;


///// HANDLE-DEBUG-POINT-EVENT (<RETURN-TRACEPOINT>)
//    Checks that the current thread and stack contexts match those
//    of the <entry-tracepoint> that set this breakpoint. If so,
//    then invoke the registered callback, and also self-deregister.
//    Otherwise, this is not an interesting event, and the application
//    should be allowed to continue (as far as this debug-point is
//    concerned, at least).

define method handle-debug-point-event 
    (app :: <debug-target>, bp :: <return-tracepoint>,
     thr :: <remote-thread>) 
       => (interested? :: <boolean>)

  let top-frame = initialize-stack-trace
                          (app.debug-target-access-path, thr);
  let ptr = frame-pointer (app.debug-target-access-path,
                           top-frame);

  if ((thr == bp.relevant-thread) &
      (ptr = bp.relevant-frame))
    deregister-debug-point (app, bp);
    next-method();
  else
    #f
  end if;
end method;


///// DYLAN-CALLING-CONVENTION-INFORMATION (Internal)
//    For now, this is just a bit of a hack.

define method dylan-calling-convention-information
    (application :: <debug-target>)
 => (argument-register-codes :: <sequence>, return-register :: <integer>,
     function-register :: <integer>)
  values(#[17], 17, 20)
end method;


///// DYLAN-TRACE-ENTRY-ARGUMENTS
//    This can only be called when the application is known to be stopped
//    exactly at the (method/internal) entry point of a dylan function,
//    and when that function's signature is known.
//    Returns <remote-value>s for the function arguments.

define method dylan-trace-entry-arguments
    (application :: <debug-target>, thread :: <remote-thread>,
     function-signature :: <remote-value>)
 => (required-arguments :: <sequence>,
     rest-vector :: false-or(<remote-value>),
     keyword-arguments :: false-or(<sequence>))

  // Get information about which registers are used to house arguments.
  let (argument-register-codes, return-register, function-register)
    = dylan-calling-convention-information(application);

  let values-inspected = 0;
  let args-in-registers = argument-register-codes.size;
  let path = application.debug-target-access-path;

  local method inspect-next-value () => (val :: <remote-value>)
          if (values-inspected < args-in-registers)
            let register = 
              enumeration-code-to-register
                (path, argument-register-codes[values-inspected]);
            let thread-register = active-register(path, thread, register);
            values-inspected := values-inspected + 1;
            read-value(path, thread-register);
	  else
            let position = 
              calculate-stack-address
                (path, thread, 1 + values-inspected - args-in-registers);
            values-inspected := values-inspected + 1;
            read-value(path, position);
	  end if;
	end method;

  // Inspect the signature properties
  let (required-count, 
       value-count, 
       takes-rest?, 
       takes-keys?, 
       takes-all-keys?,
       default-rest-values?,
       default-values?)
    = dylan-signature-properties(application, function-signature);

  let (req-types, val-types, rest-type, keys, key-types)
    = remote-signature-inspect(application, function-signature);

  // Initialize the return values from this information.
  let required-arguments = make(<vector>, size: required-count);
  let rest-vector = #f;
  let keyword-arguments = 
    if (keys) make(<vector>, size: keys.size) else #f end if;

  for (req-i from 0 below required-count)
    required-arguments[req-i] := inspect-next-value();
  end for;

  if (takes-rest?)
    rest-vector := inspect-next-value();
  end if;

  if (keys)
    inspect-next-value(); // Dummy call to skip the argument count.
    for (key-i from 0 below keys.size)
      keyword-arguments[key-i] := pair(keys[key-i], inspect-next-value());
    end for
  end if;

  values(required-arguments, rest-vector, keyword-arguments);

end method;


///// DYLAN-TRACE-RETURN-VALUES
//    This can only be called when the application is known to be stopped
//    exactly after the return of a dylan function. The signature does
//    not have to be known.
//    Returns a sequence of the function's return values.

define method dylan-trace-return-values
    (application :: <debug-target>, thread :: <remote-thread>)
 => (return-vals :: <sequence>)

  // Get information about which registers are used to house return values.
  let (argument-register-codes, return-register, function-register)
    = dylan-calling-convention-information(application);

  // Record a pointer to the access-path
  let path = application.debug-target-access-path;

  // If this thread is returning a single value, then read it from the
  // appropriate register. Otherwise, return the buffer of multiple
  // values.

  if (dylan-thread-mv-buffer-live?(path, thread))
    thread-current-mv-vector(application, thread)
  else
    let register = enumeration-code-to-register(path, return-register);
    vector(read-value(path, active-register(path, thread, register)));
  end if;
end method;

