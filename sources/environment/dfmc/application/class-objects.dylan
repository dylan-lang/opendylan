Module:    dfmc-application
Synopsis:  <class-object> environment protocols application backend
Author:    Paul Howard.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// SINGLETON-VALUE (Environment Protocol Method)
//    Given an <singleton-object>, constructs an environment object from
//    the value that defines the singleton type.

define method singleton-value
    (application :: <dfmc-application>, singleton-obj :: <singleton-object>)
 => (val :: <environment-object>)

  let target = application.application-target-app;
  with-debugger-transaction (target)
    let proxy = singleton-obj.application-object-proxy;
    let remote-singleton = runtime-proxy-to-remote-value(application, proxy);

    // Call the DM to actually exchange the singleton for its object.

    let remote-singleton-value 
      = remote-singleton-inspect(target, remote-singleton);

    // Re-package to an environment object

    make-environment-object-for-runtime-value
      (application, remote-singleton-value)
  end
end method;


///// DO-DIRECT-SUBCLASSES (Environment Protocol Method)
//    Iterates a function over each direct subclass of a class. Creates
//    an environment object for each one in turn.

define method do-direct-subclasses
    (function :: <function>, application :: <dfmc-application>, 
     class :: <class-object>,
     #key client)
 => ()

  let target = application.application-target-app;

  // Within a debugger transaction, get an exploded view of the class.
  // For each direct subclass, intern a new proxy and build an environment
  // object. Apply the supplied function to it.

  with-debugger-transaction (target)
    let proxy = ensure-application-value-proxy(application, class);
    let proxy-value = runtime-proxy-to-remote-value(application, proxy);
    let (direct-subs, direct-supers, all-supers, 
	 direct-slots, all-slots, direct-meths)
      = remote-class-inspect(target, proxy-value);
    do-environment-objects-for-runtime-values
      (function, application, direct-subs)
  end
end method;


///// DO-DIRECT-SUPERCLASSES (Environment Protocol Method)
//    Iterates a function over each direct superclass of a class. Creates an
//    environment object for each one in turn.

define method do-direct-superclasses
    (function :: <function>, application :: <dfmc-application>, 
     class :: <class-object>,
     #key client)
 => ()

  let target = application.application-target-app;

  // Within a debugger transaction, get an exploded view of the class.
  // For each direct superclass, intern a new proxy and build an environment
  // object. Apply the supplied function to it.

  with-debugger-transaction (target)
    let proxy = ensure-application-value-proxy(application, class);
    let proxy-value = runtime-proxy-to-remote-value(application, proxy);
    let (direct-subs, direct-supers, all-supers, 
	 direct-slots, all-slots, direct-meths)
      = remote-class-inspect(target, proxy-value);
    do-environment-objects-for-runtime-values
      (function, application, direct-supers)
  end
end method;


///// DO-DIRECT-METHODS (Environment Protocol Method)
//    Iterates a function over each direct method on a class. Creates a
//    <method-object> for each one in turn.

define method do-direct-methods
    (function :: <function>, application :: <dfmc-application>, 
     class :: <class-object>,
     #key client)
 => ()

  let target = application.application-target-app;

  // Within a debugger transaction, get an exploded view of the class.
  // For each direct method, intern a new proxy and build an environment
  // object. Apply the supplied function to it.

  with-debugger-transaction (target)
    let proxy = ensure-application-value-proxy(application, class);
    let proxy-value = runtime-proxy-to-remote-value(application, proxy);
    let (direct-subs, direct-supers, all-supers, 
	 direct-slots, all-slots, direct-meths)
      = remote-class-inspect(target, proxy-value);
    do-environment-objects-for-runtime-values
      (function, application, direct-meths)
  end
end method;


///// DO-DIRECT-SLOTS (Environment Protocol Method)
//    Iterates a function over each direct slot in a class. Creates a
//    <slot-object> for each one in turn.

define method do-direct-slots
    (function :: <function>, application :: <dfmc-application>, 
     class :: <class-object>,
     #key client)
 => ()

  let target = application.application-target-app;

  // Within a debugger transaction, get an exploded view of the class.
  // For each direct slot, intern a new proxy and build an environment
  // object. Apply the supplied function to it.

  with-debugger-transaction (target)
    let proxy = ensure-application-value-proxy(application, class);
    let proxy-value = runtime-proxy-to-remote-value(application, proxy);
    let (direct-subs, direct-supers, all-supers, 
	 direct-slots, all-slots, direct-meths)
      = remote-class-inspect(target, proxy-value);
    do-environment-objects-for-runtime-values
      (function, application, direct-slots)
  end
end method;


///// DO-ALL-SUPERCLASSES (Environment Protocol Method)
//    Iterates a function over all superclasses of a class. Creates a
//    <class-object> for each one in turn.

define method do-all-superclasses
    (function :: <function>, application :: <dfmc-application>, 
     class :: <class-object>,
     #key client)
 => ()

  let target = application.application-target-app;

  // Within a debugger transaction, get an exploded view of the class.
  // For each superclass, intern a new proxy and build an environment
  // object. Apply the supplied function to it.

  with-debugger-transaction (target)
    let proxy = ensure-application-value-proxy(application, class);
    let proxy-value = runtime-proxy-to-remote-value(application, proxy);
    let (direct-subs, direct-supers, all-supers, 
	 direct-slots, all-slots, direct-meths)
      = remote-class-inspect(target, proxy-value);
    do-environment-objects-for-runtime-values
      (function, application, all-supers)
  end
end method;


///// DO-ALL-SLOTS (Environment Protocol Method)
//    Iterates a function over all slots of a class. Creates a <slot-object>
//    for each one in turn.

define method do-all-slots
    (function :: <function>, application :: <dfmc-application>, 
     class :: <class-object>,
     #key client)
 => ()

  let target = application.application-target-app;

  // Within a debugger transaction, get an exploded view of the class.
  // For each slot, intern a new proxy and build an environment
  // object. Apply the supplied function to it.

  with-debugger-transaction (target)
    let proxy = ensure-application-value-proxy(application, class);
    let proxy-value = runtime-proxy-to-remote-value(application, proxy);
    let (direct-subs, direct-supers, all-supers, 
	 direct-slots, all-slots, direct-meths)
      = remote-class-inspect(target, proxy-value);
    do-environment-objects-for-runtime-values
      (function, application, all-slots)
  end
end method;


///// DO-USED-DEFINITIONS (Environment Protocol Method)
//    The application server cannot serve this operation, but defines a
//    dummy method anyway.

define method do-used-definitions
    (operation :: <function>, application :: <dfmc-application>,
     cl :: <class-object>,
     #key client = #f, modules = #f, libraries = #f)
  => ()
  // Ef off!
end method;


///// DO-CLIENT-SOURCE-FORMS (Environment Protocol Method)
//    The application server cannot serve this operation, but defines a
//    dummy method anyway.

define method do-client-source-forms
    (operation :: <function>, application :: <dfmc-application>,
     cl :: <class-object>,
     #key client = #f, modules = #f, libraries = #f)
  => ()
  // Ef off!
end method;


///// DO-INIT-KEYWORDS (Environment Protocol Method)
//    The application server cannot serve this operation, but defines a
//    dummy method anyway.

define method do-init-keywords
    (operation :: <function>, application :: <dfmc-application>,
     cl :: <class-object>,
     #key client = #f, inherited? :: <boolean> = #t)
  => ()
  // Ef off!
end method;
