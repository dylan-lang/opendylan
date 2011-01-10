Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
           Paul Howard "stuck his oar in" - May 1997.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// <stack-frame-object>
///
/// All the interesting stuff is in the application-proxy-object.
///
define class <stack-frame-object> (<application-object>)

  slot next-frame-cache-slot :: false-or(<stack-frame-object>),
    init-value: #f;

  slot previous-frame-cache-slot :: false-or(<stack-frame-object>),
    init-value: #f;

  slot local-variables-cache-slot :: false-or(<sequence>),
    init-value: #f;

  // The following slot caches the function associated with the
  // stack frame. This slot holds #f before the cache is filled.
  // When the cache is filled, it holds a pair object. The head of
  // the pair holds a boolean flag indicating that a <function-object>
  // is known for the stack frame. The tail of the pair holds a
  // <function-object> iff the head is #t.

  slot function-cache-slot :: false-or(<pair>),
    init-value: #f;

  slot thread-cache-slot :: false-or(<thread-object>),
    init-value: #f;

  slot stack-frame-top? :: <boolean>,
    init-value: #f;

  slot stack-frame-bottom? :: <boolean>,
    init-value: #f;

  // The following slot caches the source location. The slot holds
  // #f until the cache is created, at which time a single <pair>
  // object is stored. The head of the pair holds one of the following
  // symbols:
  //   #"not-available"     - The cache is constructed, but there's
  //                          no available source location.
  //   #"location-inexact"  - A source location is stored in the
  //                          cache, but it does not precisely 
  //                          correspond to the program counter.
  //   #"location-exact"    - An exact source location is stored in
  //                          the cache.

  slot source-location-cache-slot :: false-or(<pair>),
    init-value: #f;

end class <stack-frame-object>;


/// stack-frame-function
///
/// Returns #f when the stack frame is not a call frame.
///
define open generic stack-frame-function
    (server :: <server>, stack-frame :: <stack-frame-object>)
 => (function :: false-or(<application-code-object>));

/// stack-frame-environment-object
///
define open generic stack-frame-environment-object
    (server :: <server>, stack-frame :: <stack-frame-object>)
 => (object :: false-or(<environment-object>));

/// stack-frame-source-location
///
/// Returns the source-location and whether it is exact.  When it is not
/// exact, the source-location is chosen by the comiler-interface and
/// debugger-manager libraries to be a known source location.  This returns
/// #f, #f when the stack frame is not a call frame.
///
define open generic stack-frame-source-location
    (server :: <server>, stack-frame :: <stack-frame-object>)
 => (location :: false-or(<source-location>), exact? :: <boolean>);


/// stack-frame-thread
///
///
define open generic stack-frame-thread
    (server :: <server>, stack-frame :: <stack-frame-object>)
 => (thread :: <thread-object>);

/// stack-frame-type
///
///
define open generic stack-frame-type
    (server :: <server>, stack-frame :: <stack-frame-object>)
 => (type :: <symbol>);

/// stack-frame-next-frame
///
///
define open generic stack-frame-next-frame
    (server :: <server>, stack-frame :: <stack-frame-object>)
 => (next-one :: false-or(<stack-frame-object>));

/// stack-frame-previous-frame
///
///
define open generic stack-frame-previous-frame
    (server :: <server>, stack-frame :: <stack-frame-object>)
 => (previous-one :: false-or(<stack-frame-object>));

/// stack-frame-local-variables
///
///
define open generic stack-frame-local-variables
    (server :: <server>, stack-frame :: <stack-frame-object>)
 => (locals :: <sequence>);

/// stack-frame-local-variables-count
///
///
define open generic stack-frame-local-variable-count
    (server :: <server>, stack-frame :: <stack-frame-object>)
 => (locals-count :: <integer>);


/// Project dispatching methods
///
/// The functions on this page just check whether there's a server
/// running, and if so, pass the call on to that info server.
/// They also perform some caching operations.


define method stack-frame-function
    (project :: <project-object>, stack-frame :: <stack-frame-object>)
 => (function :: false-or(<application-code-object>))
  unless (stack-frame.function-cache-slot)
    let server = choose-server(project, stack-frame);
    let function = server & stack-frame-function(server, stack-frame);
    stack-frame.function-cache-slot := pair(function ~== #f, function)
  end;
  stack-frame.function-cache-slot.tail;
end method stack-frame-function;

define method environment-object-source-location
    (project :: <project-object>, stack-frame :: <stack-frame-object>)
 => (location :: false-or(<source-location>))
  stack-frame-source-location(project, stack-frame)
end method environment-object-source-location;

define method stack-frame-source-location
    (project :: <project-object>, stack-frame :: <stack-frame-object>)
 => (location :: false-or(<source-location>), exact? :: <boolean>)
  unless (stack-frame.source-location-cache-slot)
    let server = choose-server(project, stack-frame);
    if (server)
      let (location, exact?) =
        stack-frame-source-location(server, stack-frame);
      if (location)
        if (exact?)
          stack-frame.source-location-cache-slot := 
            pair(#"location-exact", location)
        else
          stack-frame.source-location-cache-slot :=
            pair(#"location-inexact", location)
        end if;
      else
        stack-frame.source-location-cache-slot := pair(#"not-available", #f)
      end if;
    else
      stack-frame.source-location-cache-slot := pair(#"not-available", #f);
    end if;
  end unless;
  select (stack-frame.source-location-cache-slot.head)
    #"not-available"      => values(#f, #f);
    #"location-inexact"   => 
       values(stack-frame.source-location-cache-slot.tail, #f);
    #"location-exact"     =>
       values(stack-frame.source-location-cache-slot.tail, #t);
  end select;
end method;

define method stack-frame-environment-object
    (project :: <project-object>, stack-frame :: <stack-frame-object>)
 => (object :: false-or(<environment-object>))
  stack-frame-function(project, stack-frame)
    | begin
	let location = stack-frame-source-location(project, stack-frame);
	location & source-location-environment-object(project, location)
      end
end method stack-frame-environment-object;

define method stack-frame-thread
    (project :: <project-object>, stack-frame :: <stack-frame-object>)
 => (thread :: <thread-object>)
  unless (stack-frame.thread-cache-slot)
    let server = choose-server(project, stack-frame);
    if (server)
      stack-frame.thread-cache-slot := 
        stack-frame-thread(server, stack-frame);
    end if;
  end unless;
  stack-frame.thread-cache-slot
end method;

define method stack-frame-next-frame
    (project :: <project-object>, stack-frame :: <stack-frame-object>)
 => (next-one :: false-or(<stack-frame-object>))
  if (stack-frame.stack-frame-top?)
    #f
  elseif (stack-frame.next-frame-cache-slot)
    stack-frame.next-frame-cache-slot
  else
    let server = choose-server(project, stack-frame);
    if (server)
      let next-one = stack-frame-next-frame(server, stack-frame);
      if (next-one)
        next-one.previous-frame-cache-slot := stack-frame;
        stack-frame.next-frame-cache-slot := next-one;
      else
        stack-frame.stack-frame-top? := #t
      end if
    end if;
    stack-frame.next-frame-cache-slot
  end if
end method;

define method stack-frame-previous-frame
    (project :: <project-object>, stack-frame :: <stack-frame-object>)
 => (previous-one :: false-or(<stack-frame-object>))
  if (stack-frame.stack-frame-bottom?)
    #f
  elseif (stack-frame.previous-frame-cache-slot)
    stack-frame.previous-frame-cache-slot
  else
    let server = choose-server(project, stack-frame);
    if (server)
      let previous-one = stack-frame-previous-frame(server, stack-frame);
      if (previous-one)
        previous-one.next-frame-cache-slot := stack-frame;
        stack-frame.previous-frame-cache-slot := previous-one;
      else
        stack-frame.stack-frame-bottom? := #t
      end if
    end if;
    stack-frame.previous-frame-cache-slot
  end if
end method;

define method stack-frame-type
    (project :: <project-object>, stack-frame :: <stack-frame-object>)
 => (type :: <symbol>)
  let server = choose-server(project, stack-frame, error?: #t);
  stack-frame-type(server, stack-frame)
end method;

define method stack-frame-local-variables
    (project :: <project-object>, stack-frame :: <stack-frame-object>)
 => (vars :: <sequence>)
  unless(stack-frame.local-variables-cache-slot)
    let server = choose-server(project, stack-frame);
    if (server)
      stack-frame.local-variables-cache-slot :=
        stack-frame-local-variables(server, stack-frame);
    else
      stack-frame.local-variables-cache-slot := #[]
    end if
  end unless;
  stack-frame.local-variables-cache-slot
end method;

define method stack-frame-local-variable-count
    (project :: <project-object>, stack-frame :: <stack-frame-object>)
 => (counter :: <integer>)
  if (stack-frame.local-variables-cache-slot)
    size(stack-frame.local-variables-cache-slot)
  else
    let server = choose-server(project, stack-frame);
    if (server)
      stack-frame-local-variable-count(server, stack-frame);
    else
      0
    end if
  end if
end method;

define method environment-object-type-name
    (object :: <stack-frame-object>) => (name :: <string>)
  "Stack frame"
end method environment-object-type-name;
