Module:    dfmc-application
Synopsis:  Mapping from <remote-value> objects to environment objects
Author:    Bill Chiles and Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// INITIALIZE-USER-OBJECT-CLASS-MAPPINGS (Internal Function)
//    Queries the environment for the set of runtime classes whose
//    instances need to be modelled by specialized <user-object>s.
//    Attempts to locate these in the runtime, and stores the ordered
//    sequence of mappings into the <dfmc-application> object so that
//    they can be iterated over by USER-CLASS-FOR-RUNTIME-VALUE.
//    Must be called while the tether is in a debugger transaction.

define method initialize-user-object-class-mappings
    (application :: <dfmc-application>) => ()

  let target = application.application-target-app;
  application.runtime-class-user-class-mappings := make(<stretchy-vector>);

  // The environment-protocols library holds the ordered sequence of
  // class mappings, and exports the USER-OBJECT-CLASS-MAPPINGS 
  // accessor to the servers. Get the mappings now.

  let mappings = user-object-class-mappings();

  // The runtime class for each method is described by a namespace-qualified
  // binding name. It is now our responsibility to find a runtime
  // <remote-value> to correspond to this name.

  for (mapping in mappings)
    let id = mapping.user-class-info-id;
    let (name, modname, libname) =
      definition-id-to-string-triple(id);
    let context = make(<dylan-name-context>,
                       library: libname,
                       module: modname);
    let class-value =
      resolve-dylan-name(target, name, context, indirect?: #f);
    if (class-value)
      add!(application.runtime-class-user-class-mappings,
           pair(class-value, mapping.user-class-info-class));
    end if;
  end for;

  // And set the flag so we never do that more than once!
  application.runtime-class-user-class-mappings-initialized? := #t;

end method;


///// USER-CLASS-FOR-RUNTIME-PROXY
//    Returns the subclass of <user-object> that should be used to
//    model an instance given its proxy.
//    Must be called while the tether is in a debugger transaction.

define method user-class-for-runtime-proxy
    (application :: <dfmc-application>, proxy :: <dylan-runtime-proxy>)
 => (uclass :: subclass(<user-object>))
  let target = application.application-target-app;
  let value = runtime-proxy-to-remote-value(application, proxy);
  let class-proxy =
    exchange-value-proxy-for-browsable-class-proxy(application, proxy);
  if (class-proxy)
    class-proxy-appropriate-user-object-model(application, class-proxy)
  else
    <user-object>
  end if
end method;


///// ENVIRONMENT-CLASS-FOR-RUNTIME-PROXY
//    Returns the class of environment object that needs to be constructed
//    to house this application proxy. This will definitely be a subclass
//    of <application-object>.

define method environment-class-for-runtime-proxy
    (application :: <dfmc-application>, proxy :: <runtime-proxy>,
     #key classification :: false-or(<symbol>) = #f)
 => (eclass :: subclass(<application-object>))
  let target = application.application-target-app;
  let value = runtime-proxy-to-remote-value(application, proxy);
  let classification
    = classification | classify-runtime-value(target, value);
  select (classification)
    #"dylan-large-integer"             => <number-object>;
    #"dylan-machine-word"              => <number-object>;
    #"dylan-unsigned-machine-word"     => <number-object>;
    #"dylan-single-float"              => <number-object>;
    #"dylan-double-float"              => <number-object>;
    #"dylan-string"                    => <string-object>;
    #"dylan-vector"                    => <sequence-object>;
    #"dylan-dimensioned-array"         => <sequence-object>;
    #"dylan-limited-vector"            => <sequence-object>;
    #"dylan-limited-array"             => <sequence-object>;
    #"dylan-list"                      => <sequence-object>;
    #"dylan-deque"                     => <sequence-object>;
    #"dylan-dotted-pair"               => <pair-object>;
    #"dylan-symbol"                    => <symbol-object>;
    #"dylan-class"                     => <class-object>;
    #"dylan-method"                    => <method-object>;
    #"dylan-generic-function"          => <generic-function-object>;
    #"dylan-canonical-true"            => <boolean-object>;
    #"dylan-canonical-false"           => <boolean-object>;
    #"dylan-general-object"            
      => user-class-for-runtime-proxy(application, proxy);
    #"dylan-integer"                   => <integer-object>;
    #"dylan-character"                 => <character-object>;
    #"foreign-function"                => <foreign-function-object>;
    #"foreign-object"                  => <foreign-object>;
    #"dylan-singleton-type"            => <user-object>;
    #"dylan-union-type"                => <user-object>;
    #"dylan-bottom-type"               => <user-object>;
    #"dylan-subclass-type"             => <user-object>;
    #"dylan-stretchy-vector"           => <sequence-object>;
    #"dylan-limited-stretchy-vector"   => <sequence-object>;
    #"dylan-object-table"              => <explicit-key-collection-object>;
    #"dylan-string-table"              => <explicit-key-collection-object>;
    #"dylan-range"                     => <user-object>;
    otherwise                          => <foreign-object>;
  end select;
end method;


///// MAKE-ENVIRONMENT-OBJECT-FOR-RUNTIME-VALUE
//    Creates an environment object of the correct class to house the
//    runtime value, and also interns the proxy.
//    This is an internal function, and callers of it MUST ensure that a
//    debugger transaction is in effect.

define method make-environment-object-for-runtime-value
    (application :: <dfmc-application>, value :: <remote-value>,
     #key address? :: <boolean> = #f)
 => (environment-object :: <application-object>)
  let target = application.application-target-app;
  let classification
    = classify-runtime-value(target, value, address?: address?);
  let canonical-object
    = select (classification)
	#"dylan-canonical-true"    => $true-object;
	#"dylan-canonical-false"   => $false-object;
	#"dylan-canonical-unbound" => $unbound-object;
	otherwise                  => #f;
      end;
  if (canonical-object)
    //---*** andrewa: bad idea if more than one application is running!
    unless (canonical-object.application-object-proxy)
      let proxy 
	= remote-value-to-runtime-proxy
	    (application, value,
	     classification: classification);
      canonical-object.application-object-proxy := proxy;
    end unless;
    canonical-object
  else
    // At the moment, the only "both" object for which ID-based lookup
    // is known not to work is the method object. Therefore, we need
    // to take care here, if the value we are packaging is known to
    // be a method. If we construct it purely within the app-server,
    // its compiler proxy will probably never be found. Even worse,
    // the database server may, by some other route, construct the
    // same object with its own proxy, breaking the interning rule.
    // We introduce a special clause which attempts to first seek out
    // an environment object that corresponds to the method, using
    // the IEP source location as a halfway house. (This is effectively
    // using the source location as an ID). If the project server
    // can deliver an environment object, then the app-server will use
    // it, and install its proxy. Otherwise, the app-server will actually
    // allocate the environment object, proxy and all.

    let environment-object
      = select (classification)
	  #"dylan-method" =>
	    let (sig, iep, kwds) = remote-method-inspect(target, value);
	    let sl = remote-address-source-location(target, iep);
	    sl & source-location-environment-object
	      (application.server-project, sl); // Try to find existing.
	  otherwise =>
	    #f
	end;

    let proxy
      = remote-value-to-runtime-proxy
	  (application, value, 
	   classification: classification,
	   address?: address?);
    if (environment-object
	  & instance?(environment-object, <application-object>))
      environment-object.application-object-proxy := proxy;
      environment-object
    else
      let eclass 
	= environment-class-for-runtime-proxy
	    (application, proxy, classification: classification);
      let environment-object
	= make-environment-object(eclass,
				  project: application.server-project,
				  application-object-proxy: proxy);
      assert(~address?
	       | ~instance?(environment-object, <immediate-application-object>),
	     "Whoops, created an immediate object %= for an address!",
	     environment-object);
      environment-object
    end if;
  end
end method;

define method do-environment-objects-for-runtime-values
    (function :: <function>, application :: <dfmc-application>,
     values :: <sequence>)
 => ()
  for (value :: <remote-value> in values)
    let object = make-environment-object-for-runtime-value(application, value);
    function(object)
  end
end method do-environment-objects-for-runtime-values;

///// APPLICATION-OBJECT-CLASS (Environment Protocol Method)
//    Returns #f for application objects that are not Dylan objects, as
//    per the protocol.

define method application-object-class
    (application :: <dfmc-application>, obj :: <address-object>)
       => (class-obj :: false-or(<class-object>))
  #f
end method;

define method application-object-class
    (application :: <dfmc-application>, obj :: <foreign-object>)
       => (class-obj :: false-or(<class-object>))
  #f
end method;

define method application-object-class
    (application :: <dfmc-application>, obj :: <stack-frame-object>)
       => (class-obj :: false-or(<class-object>))
  #f
end method;

define method application-object-class
    (application :: <dfmc-application>, obj :: <breakpoint-object>)
       => (class-obj :: false-or(<class-object>))
  #f
end method;

define method application-object-class
    (application :: <dfmc-application>, obj :: <variable-object>)
       => (class-obj :: false-or(<class-object>))
  #f
end method;

define method application-object-class
    (application :: <dfmc-application>, obj :: <thread-object>)
       => (class-obj :: false-or(<class-object>))
  #f
end method;

define method application-object-class
    (application :: <dfmc-application>, obj :: <application-object>)
 => (class-obj :: false-or(<class-object>))
  let target = application.application-target-app;
  with-debugger-transaction (target)
    let proxy = obj.application-object-proxy;
    if (proxy)
      let proxy-value = runtime-proxy-to-remote-value(application, proxy);
      let (class-value, incarnation, current-incarnation, immediate?)  
	= dylan-object-class(target, proxy-value);
      ignore(incarnation);
      ignore(current-incarnation);
      ignore(immediate?);

      // The DM function DYLAN-OBJECT-CLASS is permitted to return #f
      // in its first result position. This is not expected, and probably
      // indicates a pathalogical case, but nonetheless we should be
      // prepared to handle it.

      if (class-value)
	let class
	  = make-environment-object-for-runtime-value
	      (application, class-value);
	// Again due to possible pathalogical case, discard any environment
	// object that does not correspond to a class. (Eg. we may have been
	// fooled into treating a foreign object as a dylan object, or possibly
	// the runtime refused to yield enough information to construct the
	// class due to some temporary instability).
	if (instance?(class, <class-object>))
	  class
	end
      end
    end
  end
end method application-object-class;
