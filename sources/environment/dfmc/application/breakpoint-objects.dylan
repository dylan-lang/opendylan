Module:    dfmc-application
Synopsis:  <breakpoint-object> environment protocols from the application.
Author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// ------------------------------ Proxies -------------------------------
//    The application proxy for a <breakpoint-object> is ultimately a
//    <debug-point> as defined in the debugger-manager. Below are defined
//    specific subclasses of <debug-point> that are used as the concrete
//    proxies. In general, <function-breakpoint-object> is implemented
//    on top of DM tracepoints, and <source-location-breakpoint-object> is
//    implemented on top of DM breakpoints.


///// <FUNCTION-ENTRY-TRACEPOINT> (Internal Application Proxy Class)
//    A subclass of <ENTRY-TRACEPOINT>, imported from debugger-manager.
//    Used as the proxy for a <function-breakpoint-object>.

define class <function-entry-tracepoint> (<entry-tracepoint>)

  slot function-entry-registered? :: <boolean> = #f;

  constant 
    slot function-entry-breakpoint-object :: <function-breakpoint-object>,
      required-init-keyword: breakpoint-object:;

end class <function-entry-tracepoint>;


///// <FUNCTION-RETURN-TRACEPOINT> (Internal Application Proxy Class)
//    A subclass of <RETURN-TRACEPOINT>, imported from debugger-manager.
//    This is not directly used as a proxy.

define class <function-return-tracepoint> (<return-tracepoint>)
end class <function-return-tracepoint>;


///// <LOCATION-BREAKPOINT> (Internal Application Proxy Class)
//    A subclass of <BREAKPOINT>, imported from the debugger-manager.

define class <location-breakpoint> (<breakpoint>)

  slot location-break-registered? :: <boolean> = #f;

  constant
    slot location-breakpoint-object :: <source-location-breakpoint-object>,
      required-init-keyword: breakpoint-object:;

end class <location-breakpoint>;


///// <CLASS-BREAKPOINT>
//    This unfortunately is not a real debugger-manager breakpoint,
//    it would be nice to fix this ultimately.

define class <class-breakpoint> (<object>)
  slot class-break-registered? :: <boolean> = #f;

  constant slot class-breakpoint-object :: <class-breakpoint-object>,
    required-init-keyword: breakpoint-object:;
end class <class-breakpoint>;


///// ----------------------------- Conditions -------------------------------

///// <BREAKPOINT-ERROR>
//    The class of all breakpoint errors

define abstract class <breakpoint-error> (<format-string-condition>, <error>)
end class <breakpoint-error>;


define class <unknown-breakpoint-address> (<breakpoint-error>)
end class <unknown-breakpoint-address>;

define method make
    (class == <unknown-breakpoint-address>, #key breakpoint)
 => (condition :: <unknown-breakpoint-address>)
  next-method(class,
	      format-string: "Cannot compute address for breakpoint: %=",
	      format-arguments: vector(breakpoint))
end method make;


define class <unknown-class-breakpoint> (<breakpoint-error>)
end class <unknown-class-breakpoint>;

define method make 
    (class == <unknown-class-breakpoint>, #key breakpoint)
 => (condition :: <unknown-class-breakpoint>)
  next-method(class,
	      format-string: "Cannot find class %= for breakpoint: %=",
	      format-arguments: vector(breakpoint.breakpoint-object,
				       breakpoint))
end method make;


///// ----------------------------- Callbacks --------------------------------

///// MAKE-RETURN-TRACEPOINT (Open GF Extension. DM Callback).
//    Given a <function-entry-tracepoint>, create a 
//    <function-return-tracepoint>.

define method make-return-tracepoint
    (application :: <target-application>,
     entry-trace :: <function-entry-tracepoint>,
     thread :: <remote-thread>,
     #rest keys, #key, #all-keys)
 => (return-tracepoint :: <function-return-tracepoint>)
  apply(make, <function-return-tracepoint>, keys)
end method make-return-tracepoint;

//// <BREAKPOINT-INFO>

define class <breakpoint-info> (<object>)
end class <breakpoint-info>;

define class <breakpoint-entry-info> (<breakpoint-info>)
  constant slot info-required-values :: <sequence>,
    required-init-keyword: required-values:;
  constant slot info-rest-value :: false-or(<environment-object>),
    required-init-keyword: rest-value:;
  constant slot info-keyword-values :: false-or(<sequence>) = #f,
    required-init-keyword: keyword-values:;
end class <breakpoint-entry-info>;

define class <breakpoint-return-info> (<breakpoint-info>)
  constant slot info-return-values :: <sequence>,
    required-init-keyword: return-values:;
end class <breakpoint-return-info>;

define method breakpoint-info
    (application :: <dfmc-application>, bp :: <function-breakpoint-object>)
 => (info :: false-or(<breakpoint-info>))
  let values = application.application-function-breakpoint-values;
  element(values, bp, default: #f)
end method breakpoint-info;

define method breakpoint-info-setter
    (info :: <breakpoint-info>, application :: <dfmc-application>,
     breakpoint :: <breakpoint-object>)
 => (info :: <breakpoint-info>)
  add!(application.application-signalling-breakpoints, breakpoint);
  application.application-function-breakpoint-values[breakpoint] := info
end method breakpoint-info-setter;

///// FUNCTION-ENTRY-CALLBACK (Internal. DM Callback).
//    Invoked by the debugger manager when a function entry tracepoint is
//    touched. The tracepoint must correspond to a
//    <function-breakpoint-object>, which will be recorded as
//    one of the "current" stopping breakpoints before entering the
//    debugger transaction.

define method function-entry-callback
    (application :: <dfmc-application>, target :: <target-application>,
     bp :: <function-entry-tracepoint>, thread :: <remote-thread>)
 => (interested? :: <boolean>)
  let breakpoint = bp.function-entry-breakpoint-object;
  let function = breakpoint.breakpoint-object;
  let signature
    = if (breakpoint.breakpoint-entry-point?)
	let proxy = function.application-object-proxy;
	case
	  proxy =>
	    let function-value
	      = runtime-proxy-to-remote-value(application, proxy);
	    remote-method-inspect(target, function-value);
	  instance?(function, <method-object>) =>
	    // Note that we can use the g.f. signature here
	    // because it is only looking at its overall shape.
	    let project = application.server-project;
	    let gf = method-generic-function(project, function);
	    let proxy = gf & gf.application-object-proxy;
	    if (proxy)
	      let function-value
		= runtime-proxy-to-remote-value(application, proxy);
	      remote-generic-function-inspect(target, function-value)
	    else
	      debug-message("Failed to find proxy for method or generic!")
	    end;
	  otherwise =>
	    debug-message("Failed to find generic proxy!");
	end
      end;
  let (required-values, rest-value, keyword-values)
    = if (signature)
	let (required, rest, keywords)
	  = dylan-trace-entry-arguments(target, thread, signature);
	let constructor
	  = curry(make-environment-object-for-runtime-value, 
		  application);
	values(map(constructor, required),
	       rest & constructor(rest),
	       keywords & map(constructor, keywords))
      else
	debug-message("No signature for breakpoint function!");
	values(#[], #f, #[])
      end;
  breakpoint-info(application, breakpoint)
    := make(<breakpoint-entry-info>,
	    required-values: required-values,
	    rest-value:      rest-value,
	    keyword-values:  keyword-values);
  #t
end method function-entry-callback;


///// FUNCTION-RETURN-CALLBACK (Internal. DM Callback).
//    Invoked by the debugger manager when a function return tracepoint is
//    touched. The tracepoint will not directly correspond to a
//    <function-breakpoint-object>, but it should correlate to a
//    <function-entry-tracepoint> which will.

define method function-return-callback
    (application :: <dfmc-application>, target :: <target-application>,
     bp :: <function-return-tracepoint>, thread :: <remote-thread>)
 => (interested? :: <boolean>)
  let entry = bp.corresponding-entry-tracepoint;
  if (entry.function-entry-registered?)
    let breakpoint = entry.function-entry-breakpoint-object;
    let values = dylan-trace-return-values(target, thread);
    let constructor
      = curry(make-environment-object-for-runtime-value, application);
    breakpoint-info(application, breakpoint)
      := make(<breakpoint-return-info>,
	      return-values: map(constructor, values));
    #t;
  else
    #f;
  end
end method function-return-callback;


///// LOCATION-BREAKPOINT-CALLBACK (Internal. DM Callback).
//    Invoked by the debugger when a location breakpoint is touched.
//    This should correspond directly to a
//    <source-location-breakpoint-object> in the environment.

define method location-breakpoint-callback
    (application :: <dfmc-application>, target :: <target-application>,
     bp :: <location-breakpoint>, thread :: <remote-thread>)
 => (interested? :: <boolean>)
  let breakpoint = bp.location-breakpoint-object;
  add!(application.application-signalling-breakpoints, breakpoint);
  #t
end method location-breakpoint-callback;


///// ---------------------------- Calculations ------------------------------

///// CALCULATE-BREAKPOINT-ADDRESS (Internal)
//    Used to calculate the actual runtime instruction address that
//    corresponds to an environment breakpoint. Has separate methods
//    for function breakpoints and location breakpoints.
//    Caller must ensure that a debugger transaction is in effect.


define method calculate-breakpoint-address
    (application :: <dfmc-application>, 
     bp-object :: <source-location-breakpoint-object>,
     #key compilation-context = #f)
 => (address-we-hope :: false-or(<remote-value>))
  let target = application.application-target-app;
  let project = application.server-project;
  let location = bp-object.breakpoint-object;
  let source-record = location.source-location-source-record;
  let project-proxy = source-record.source-record-project;
  let (address, exact)
    = if (project-proxy)
	source-location-remote-address
	  (target, 
	   location, 
	   interactive-only?: #f,
	   compilation-context: project-proxy.project-browsing-context)
      else
	values(#f, #f)
      end;
  address;
end method calculate-breakpoint-address;

define method function-object-breakpoint-address
    (application :: <dfmc-application>, function-object :: <function-object>,
     #key entry-point? :: <boolean> = #f)
 => (address-we-hope :: false-or(<remote-value>))
  let target = application.application-target-app;
  let project = application.server-project;
  let source-location 
    = environment-object-source-location(project, function-object);
  case
    source-location =>
      let source-record = source-location.source-location-source-record;
      let project-proxy = source-record.source-record-project;
      let context = project-proxy & project-proxy.project-browsing-context;
      let (address, exact)
	= source-location-remote-address
	    (target,
	     source-location,
	     interactive-only?:   #f,
	     entry-point-only?:   entry-point?,
	     compilation-context: context);
      address;
    instance?(function-object, <generic-function-object>) =>
      #f;
    otherwise =>
      //---*** We need to handle entry-point? in here too!
      let proxy = ensure-application-proxy(application, function-object);
      if (proxy)
	let function-value = runtime-proxy-to-remote-value(application, proxy);
	let (sig, breakpoint-address, keyword-specifiers)
	  = remote-method-inspect(target, function-value);
	breakpoint-address
      end;
  end
end method function-object-breakpoint-address;

define method calculate-breakpoint-address
    (application :: <dfmc-application>,
     bp-object :: <function-breakpoint-object>,
     #key compilation-context = #f)
  => (address-we-hope :: false-or(<remote-value>))
  let function = bp-object.breakpoint-object;
  ensure-application-proxy(application, function);
  if (instance?(function, <method-object>))
    let project = application.server-project;
    let gf = method-generic-function(project, function);
    gf & ensure-application-proxy(application, gf)
  end;
  ensure-application-proxy(application, function);
  function-object-breakpoint-address
    (application, function,
     entry-point?: bp-object.breakpoint-entry-point?)
end method calculate-breakpoint-address;


///// ---------------- Proxy Instantiation and Registration ----------------

///// REGISTER-PROXY-IF-NECESSARY (Internal)
//    If a breakpoint proxy has not been registered with the debugger
//    manager, then do so.

define method register-proxy-if-necessary
    (application :: <dfmc-application>, 
     proxy :: <function-entry-tracepoint>) => ()
  unless (proxy.function-entry-registered?)
    let target = application.application-target-app;
    register-debug-point(target, proxy);
    proxy.function-entry-registered? := #t;
  end;
end method register-proxy-if-necessary;

define method register-proxy-if-necessary
    (application :: <dfmc-application>, 
     proxy :: <location-breakpoint>) => ()
  unless (proxy.location-break-registered?)
    let target = application.application-target-app;
    register-debug-point(target, proxy);
    proxy.location-break-registered? := #t;
  end;
end method register-proxy-if-necessary;

define method register-proxy-if-necessary
    (application :: <dfmc-application>, 
     proxy :: <class-breakpoint>) => ()
  unless (proxy.class-break-registered?)
    let target = application.application-target-app;
    let thread = application-open-interactor-thread(application);
    let breakpoint = proxy.class-breakpoint-object;
    let class = breakpoint.breakpoint-object;
    let remote-class = class.application-object-proxy;
    if (remote-class
	  & set-application-class-breakpoint
	      (application, thread, remote-class.static-dylan-value))
      proxy.class-break-registered? := #t
    else
      make(<unknown-class-breakpoint>, breakpoint: breakpoint)
    end
  end
end method register-proxy-if-necessary;


///// DEREGISTER-PROXY-IF-NECESSARY (Internal)
//    If the breakpoint proxy is currently registered with the debugger
//    manager, this function will de-register it.

define method deregister-proxy-if-necessary
    (application :: <dfmc-application>, 
     proxy :: <function-entry-tracepoint>) => ()
  if (proxy.function-entry-registered?)
    let target = application.application-target-app;
    deregister-debug-point(target, proxy);
    proxy.function-entry-registered? := #f;
  end;
end method deregister-proxy-if-necessary;

define method deregister-proxy-if-necessary
    (application :: <dfmc-application>, 
     proxy :: <location-breakpoint>) => ()
  if (proxy.location-break-registered?)
    let target = application.application-target-app;
    deregister-debug-point(target, proxy);
    proxy.location-break-registered? := #f;
  end;
end method deregister-proxy-if-necessary;

define method deregister-proxy-if-necessary
    (application :: <dfmc-application>, 
     proxy :: <class-breakpoint>) => ()
  if (proxy.class-break-registered?)
    let target = application.application-target-app;
    let thread = application-open-interactor-thread(application);
    let breakpoint = proxy.class-breakpoint-object;
    let class = breakpoint.breakpoint-object;
    let remote-class = class.application-object-proxy;
    if (remote-class
	  & clear-application-class-breakpoint
	      (application, thread, remote-class.static-dylan-value))
      proxy.class-break-registered? := #f
    else
      make(<unknown-class-breakpoint>, breakpoint: breakpoint)
    end
  end;
end method deregister-proxy-if-necessary;

///// FIND-OR-INSTANTIATE-PROXY (Internal)
//    This returns the appropriate <debug-point> proxy for an environment
//    <breakpoint-object>. This will also transparently create the
//    proxy if one does not exist already.

define method find-or-instantiate-proxy
    (application :: <dfmc-application>, bp :: <function-breakpoint-object>,
     #key compilation-context = #f)
 => (proxy :: <function-entry-tracepoint>)
  if (bp.application-object-proxy)
    bp.application-object-proxy
  else
    let addr 
      = calculate-breakpoint-address
          (application, bp, 
	   compilation-context: compilation-context);
    if (addr)
      let proxy
	= make(<function-entry-tracepoint>,
	       address: addr,
	       callback: curry(function-entry-callback, application),
	       return-callback: curry(function-return-callback, application),
	       breakpoint-object: bp);
      bp.application-object-proxy := proxy;
      proxy;
    else
      error(make(<unknown-breakpoint-address>, breakpoint: bp))
    end;
  end;
end method find-or-instantiate-proxy;

define method find-or-instantiate-proxy
    (application :: <dfmc-application>, 
     bp :: <source-location-breakpoint-object>,
     #key compilation-context = #f)
 => (proxy :: <location-breakpoint>)
  if (bp.application-object-proxy)
    bp.application-object-proxy
  else
    let addr = 
      calculate-breakpoint-address(application, 
                                   bp,
                                   compilation-context: compilation-context);
    if (addr)
      let proxy
	= make(<location-breakpoint>,
	       address: addr,
	       callback: curry(location-breakpoint-callback, application),
	       breakpoint-object: bp);
      bp.application-object-proxy := proxy;
      proxy;
    else
      error(make(<unknown-breakpoint-address>, breakpoint: bp))
    end;
  end;
end method find-or-instantiate-proxy;

define method find-or-instantiate-proxy
    (application :: <dfmc-application>, 
     bp :: <class-breakpoint-object>,
     #key compilation-context = #f)
 => (proxy :: <class-breakpoint>)
  if (bp.application-object-proxy)
    bp.application-object-proxy
  else
    let class = bp.breakpoint-object;
    ensure-application-proxy(application, class);
    let proxy = make(<class-breakpoint>, breakpoint-object: bp);
    bp.application-object-proxy := proxy;
    proxy
  end
end method find-or-instantiate-proxy;

define method reset-breakpoint-failure-recording
    (application :: <dfmc-application>) => ()
  // Just keep stretchy sequences for the three types of state
  // change that the app-server ever cares about.
  application.breakpoint-state-change-failures[#"created"]
     := make(<stretchy-vector>);
  application.breakpoint-state-change-failures[#"destroyed"]
     := make(<stretchy-vector>);
  application.breakpoint-state-change-failures[#"enabled?"]
     := make(<stretchy-vector>);
end method reset-breakpoint-failure-recording;

define method note-all-recorded-breakpoint-failures
    (application :: <dfmc-application>) => ()
  for (breakpoints keyed-by state-change in 
       application.breakpoint-state-change-failures)
    if (breakpoints.size > 0)
      note-breakpoint-state-changes-failed(application.server-project,
                                           breakpoints,
                                           state-change);
    end
  end;
end method note-all-recorded-breakpoint-failures;

define method breakpoint-has-failed-already?
    (application :: <dfmc-application>, bp :: <breakpoint-object>)
 => (well? :: <boolean>)
  block (return)
    for (breakpoints keyed-by state-change in
         application.breakpoint-state-change-failures)
      if (member?(bp, breakpoints))
        return(#t)
      end
    end;
    return(#f)
  end;
end method breakpoint-has-failed-already?;


///// ------------------- Implementations of State Changes --------------------
//    The environment is contracted to call SERVER-NOTE-BREAKPOINT-
//    STATE-CHANGED in order for the app-server to do the necessary work.
//    These are the methods.


///// SERVER-NOTE-BREAKPOINT-STATE-CHANGED (Environment Protocol Methods)

define method server-note-breakpoint-state-changed
    (application :: <dfmc-application>, 
     bp :: <generic-function-breakpoint-object>,
     state-change :: <breakpoint-state>,
     #key use-project-proxy = application.server-project.project-proxy)
 => ()
  #f
end method server-note-breakpoint-state-changed;

define method server-note-breakpoint-state-changed
    (application :: <dfmc-application>, bp :: <breakpoint-object>,
     state-change :: <breakpoint-state>,
     #key use-project-proxy = application.server-project.project-proxy)
 => ()
  unless (breakpoint-has-failed-already?(application, bp))
    let target = application.application-target-app;
    let cc = use-project-proxy & use-project-proxy.project-browsing-context;
    with-debugger-transaction (target)
      block ()
	select (state-change)
	  #"created" =>
	    let proxy = find-or-instantiate-proxy(application, bp,
						  compilation-context: cc);
	    if (bp.breakpoint-enabled?)
	      register-proxy-if-necessary(application, proxy)
	    end;
	    
	  #"destroyed" =>
	    let proxy = find-or-instantiate-proxy(application, bp,
						  compilation-context: cc);
	    deregister-proxy-if-necessary(application, proxy);
	    
	  #"enabled?" =>
	    let proxy = find-or-instantiate-proxy(application, bp,
						  compilation-context: cc);
	    if (bp.breakpoint-enabled?)
	      register-proxy-if-necessary(application, proxy);
	    else
	      deregister-proxy-if-necessary(application, proxy);
	    end;
	    
	  otherwise => #f;
	  
	end
      exception (<breakpoint-error>)
	if (application.application-state == #"running")
	  note-breakpoint-state-changes-failed
	    (application.server-project, vector(bp), state-change);
	else
	  add!(application.breakpoint-state-change-failures[state-change],
	       bp)
	end
      end
    end
  end
end method server-note-breakpoint-state-changed;

///// -------------------- Other Protocol Implementations -------------------

///// CURRENT-STOP-BREAKPOINTS (Environment Protocol Method)
//    Returns the set of breakpoints that resulted in the current debugger
//    transaction.

define method current-stop-breakpoints
    (application :: <dfmc-application>, thread :: <thread-object>)
 => (seq :: <sequence>)
  application.application-signalling-breakpoints | #[]
end method current-stop-breakpoints;


///// APPLICATION-JUST-HIT-BREAKPOINT? (Environment Protocol Method)
//    Returns #t iff the current debugger transaction was brought about
//    (at least in part) by a breakpoint being hit

define method application-just-hit-breakpoint?
    (application :: <dfmc-application>, thread :: <thread-object>)
 => (well? :: <boolean>)
  any?(breakpoint-stop?, current-stop-breakpoints(application, thread))
end method application-just-hit-breakpoint?;
