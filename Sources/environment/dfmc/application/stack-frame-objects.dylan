Module:    dfmc-application
Synopsis:  Serving environment stack frames from the application.
Author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// The application proxy for a <stack-frame-object> is the Debugger Manager
// class <application-stack-frame>.


///// SOURCE-LOCATION-FROM-FRAME-PROXY (Internal function)

define method source-location-from-frame-proxy
    (application :: <dfmc-application>, call-frame :: <call-frame>)
 => (maybe-location :: false-or(<source-location>),
     location-exact? :: <boolean>)
  let target = application.application-target-app;
  let ip = call-frame-instruction-pointer(target, call-frame);
  //---*** andrewa: this currently doesn't work for foreign frames...
  remote-address-source-location
    (target, ip, line-only?: #t, interactive-only?: #f,
     exact-only?: #f)
end method source-location-from-frame-proxy;

define method source-location-from-frame-proxy
    (application :: <dfmc-application>, 
     call-frame :: <application-stack-frame>)
 => (maybe-location :: false-or(<source-location>),
     location-exact? :: <boolean>)
  values(#f, #f)
end method source-location-from-frame-proxy;


///// FUNTION-OBJECT-FROM-FRAME-PROXY (Internal function)

define method function-object-from-frame-proxy
    (application :: <dfmc-application>, 
     call-frame :: <application-stack-frame>)
 => (function == #f)
  #f
end method function-object-from-frame-proxy;

define method function-object-from-frame-proxy
    (application :: <dfmc-application>, call-frame :: <call-frame>)
 => (function :: false-or(<application-code-object>))

  let target = application.application-target-app;
  let (called-symbol, called-object, generic-object)
     = call-frame-function(target, call-frame);
  case
    called-object =>
      // Found a Dylan function
      make-environment-object-for-runtime-value(application, called-object);
    called-symbol =>
      // Found a foreign function
      let value = called-symbol.remote-symbol-address;
      make-environment-object-for-runtime-value
	(application, value, address?: #t);
    otherwise =>
      // What is going on here...?
      #f;
  end
end method function-object-from-frame-proxy;


///// STACK-FRAME-FUNCTION (Environment Protocol Method)
//    Returns a function object for the frame.

define method stack-frame-function
    (application :: <dfmc-application>, sf :: <stack-frame-object>)
 => (func :: false-or(<application-code-object>))
  let target = application.application-target-app;
  with-debugger-transaction (target)
    let project = application.server-project;
    let dm-frame = sf.application-object-proxy;
    let (location, exact?) 
      = source-location-from-frame-proxy(application, dm-frame);
    let definition-from-source
      = location & source-location-environment-object(project, location);
    if (instance?(definition-from-source, <application-code-object>))
      definition-from-source
    else
      function-object-from-frame-proxy(application, dm-frame)
    end
  end
end method stack-frame-function;


///// STACK-FRAME-SOURCE-LOCATION (Environment Protocol Method)
//    Returns a source location for the frame.
//    TODO: Implement this method using DM's "remote-address-source-location"

define method stack-frame-source-location
    (application :: <dfmc-application>, sf :: <stack-frame-object>)
 => (location :: false-or(<source-location>), exact? :: <boolean>)
  let target = application.application-target-app;
  with-debugger-transaction (target)
    let dm-frame = sf.application-object-proxy;
    source-location-from-frame-proxy(application, dm-frame)
  end
end method stack-frame-source-location;


///// STACK-FRAME-THREAD (Environment Protocol Method)
//    Returns a <thread-object> for the thread to which this stack frame
//    belongs.

define method stack-frame-thread
    (application :: <dfmc-application>, sf :: <stack-frame-object>)
 => (thread :: <thread-object>)
  let target = application.application-target-app;
  let dm-frame = sf.application-object-proxy;
  let remote-thread = dm-frame.dm-stack-frame-thread; // From the DM.
  make-environment-object
    (<thread-object>,
     project: application.server-project,
     application-object-proxy: remote-thread);
end method stack-frame-thread;


///// STACK-FRAME-TYPE (Environment Protocol Method)
//    Returns a symbol denoting the type of a stack frame as far as the
//    environment is concerned.

define method stack-frame-type
    (application :: <dfmc-application>, sf :: <stack-frame-object>)
 => (type :: <symbol>)
  let target = application.application-target-app;
  with-debugger-transaction (target)
    let dm-frame = sf.application-object-proxy;
    if (instance?(dm-frame, <call-frame>))
      if (dylan-call-frame?(target, dm-frame))
	#"dylan-call"
      else
	#"foreign-call"
      end if
    elseif (instance?(dm-frame, <unwind-protect-frame>))
      #"cleanup"
    else
      #"unknown"
    end if;
  end
end method stack-frame-type;


///// STACK-FRAME-PREVIOUS-FRAME (Environment Protocol Method)
//    Descends the stack trace.

define method stack-frame-previous-frame
    (application :: <dfmc-application>, sf :: <stack-frame-object>)
 => (prev :: false-or(<stack-frame-object>))
  let prev-dm-frame = #f;
  let target = application.application-target-app;
  let dm-frame = sf.application-object-proxy;

  perform-requiring-debugger-transaction
    (target,
     method ()
       prev-dm-frame := previous-stack-frame(target, dm-frame);
     end method);

  if (prev-dm-frame)
    make-environment-object(<stack-frame-object>,
                            project: application.server-project,
                            application-object-proxy: prev-dm-frame);
  else
    #f
  end if;
end method stack-frame-previous-frame;


///// STACK-FRAME-NEXT-FRAME (Environment Protocol Method)
//    Ascends the stack trace.

define method stack-frame-next-frame
    (application :: <dfmc-application>, sf :: <stack-frame-object>)
 => (prev :: false-or(<stack-frame-object>))

  let nxt-dm-frame = #f;
  let target = application.application-target-app;
  let dm-frame = sf.application-object-proxy;

  perform-requiring-debugger-transaction
    (target,
     method ()
       nxt-dm-frame := next-stack-frame(target, dm-frame);
     end method);

  if (nxt-dm-frame)
    make-environment-object(<stack-frame-object>,
                            project: application.server-project,
                            application-object-proxy: nxt-dm-frame);
  else
    #f
  end if;
end method stack-frame-next-frame;


///// STACK-FRAME-LOCAL-VARIABLES (Environment Protocol Method)
//    Builds <local-variable-object>s for the variables known to be live
//    in the given stack frame.

define method stack-frame-local-variables
    (application :: <dfmc-application>, sf :: <stack-frame-object>)
 => (locvars :: <sequence>)
  let target = application.application-target-app;
  with-debugger-transaction (target)
    let dm-frame = sf.application-object-proxy;
    let var-seq = all-frame-local-variables(application, dm-frame);
    let count = size(var-seq);
    let locvars = make(<vector>, size: count);
    for (i from 0 below count)
      locvars[i] :=
	make-environment-object(<local-variable-object>,
				project: application.server-project,
				application-object-proxy: var-seq[i]);
    end;
    locvars
  end
end method stack-frame-local-variables;


///// STACK-FRAME-LOCAL-VARIABLE-COUNT (Environment Protocol Method)
//    Just returns a counter of the number of live local variables in a
//    stack frame.

define method stack-frame-local-variable-count
    (application :: <dfmc-application>, sf :: <stack-frame-object>)
 => (count :: <integer>)
  let target = application.application-target-app;
  with-debugger-transaction (target)
    let dm-frame = sf.application-object-proxy;
    count-frame-local-variables(application, dm-frame)
  end
end method stack-frame-local-variable-count;
