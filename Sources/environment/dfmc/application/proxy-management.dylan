Module:    dfmc-application
Synopsis:  Creation and consolidation of proxy objects for the application
Author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// PROXIES
//    The following list shows what kind of objects are used as application
//    proxies for the various environment object classes.

//    Environment Object Class       Proxy Class                   Defined In
//    ------------------------       -----------                   -----------
//    <stack-frame-object>           <application-stack-frame>     DM
//    <thread-object>                <remote-thread>               Acc. Path.
//    <dylan-object>                 <runtime-value-proxy>         Here
//    <local-variable-object>        <application-local-variable>  Here
//    <breakpoint-object>            <debug-point>                 DM
//    <address-object>               <remote-value>                Acc. Path.
//    <register-object>              <remote-register>             Acc. Path.
//    <component-object>             <remote-library>              Acc. Path.


///// <RUNTIME-PROXY>
//    An object of any kind that is used as the application proxy for an
//    environment object.

define constant <runtime-proxy>
   = type-union(<runtime-value-proxy>,
                /* <runtime-entry-point-proxy>, */
                <application-variable>,
                <application-local-variable>,
                <application-stack-frame>,
                <debug-point>,
                <remote-thread>,
                <remote-library>,
                <remote-register>,
                <remote-value>);


/* --- Not currently used.
///// <RUNTIME-ENTRY-POINT-PROXY>
//    The kind of application proxy that specifies the program-counter
//    co-ordinate of a breakpoint.

define class <runtime-entry-point-proxy> (<object>)

  slot proxy-entry-point-symbol :: <remote-symbol>,
    required-init-keyword: entry-point-symbol:;

  slot proxy-entry-point-offset :: <integer>,
    required-init-keyword: entry-point-offset:;

end class;
*/

///// <RUNTIME-VALUE-PROXY>
//    The kind of application proxy that has something specifically to do
//    with a <remote-value>.

define abstract class <runtime-value-proxy> 
                         (<page-relative-object-table-entry>)
end class;

define sealed domain make (subclass(<runtime-value-proxy>));
define sealed domain initialize (<runtime-value-proxy>);


///// <DYLAN-RUNTIME-PROXY>
//    A proxy for a DYLAN runtime object.

define abstract class <dylan-runtime-proxy> (<runtime-value-proxy>)
end class;


///// <FOREIGN-RUNTIME-PROXY>
//    A proxy for a FOREIGN runtime object. It is assumed that these
//    are static - once we know its value, it never changes. We can't
//    possibly know how some foreign garbage collector works, so we
//    have no way to track foreign objects.

define class <foreign-runtime-proxy> (<runtime-value-proxy>)

  constant slot static-foreign-value :: <remote-value>,
    required-init-keyword: value:;

end class;


///// <FOREIGN-FUNCTION-RUNTIME-PROXY>
//    A proxy for a foreign function runtime object.

define class <foreign-function-runtime-proxy> (<runtime-value-proxy>)

  constant slot static-foreign-symbol :: <remote-symbol>,
    required-init-keyword: symbol:;

end class;

/*---*** andrewa: will be useful, how do we turn it into a source location?
define method foreign-function-location
    (application :: <dfmc-application>, proxy :: <foreign-function-runtime-proxy>,
     #key decorate? = #t)
 => (filename :: false-or(<string>), line :: false-or(<integer>))
  let symbol = proxy.static-foreign-symbol;
  let source-locator-map
    = function-source-location-map
        (application.debug-target-access-path, symbol);
  let filename = source-locator-map & source-filename(source-locator-map);
  let line = source-locator-map & base-linenumber(source-locator-map);
  values(filename, line)
end method;
*/

///// <STATIC-DYLAN-RUNTIME-PROXY>
//    A proxy for a DYLAN runtime object that is known to be static, hence
//    not subject to any relocation by the memory manager.

define class <static-dylan-runtime-proxy> (<dylan-runtime-proxy>)

  constant slot static-dylan-value :: <remote-value>,
    required-init-keyword: value:;

end class;


///// <STATIC-DYLAN-CLASS-PROXY>
//    A proxy for a dylan runtime object known to be a statically-heaped
//    instance of <class>.
//    This proxy additionally caches information that is needed when
//    browsing instances of the class.

define class <static-dylan-class-proxy> (<static-dylan-runtime-proxy>)
  slot class-proxy-environment-class-cached? :: <boolean> = #f;
  slot class-proxy-browser-data-cached? :: <boolean> = #f;
  slot class-proxy-environment-class :: subclass(<environment-object>);
  slot class-proxy-slots :: <sequence>;
  slot class-proxy-navigation :: <string-table>;
  slot class-proxy-repeat :: false-or(<string>);
  slot class-proxy-count-offset :: false-or(<integer>);
  slot class-proxy-element-size :: false-or(<integer>);
  slot class-proxy-element-offset :: false-or(<integer>);
  slot class-proxy-class-slot-count :: <integer>;
  slot class-proxy-incarnation :: false-or(<remote-value>) = #f;
end class;


///// <TRACKED-DYLAN-RUNTIME-PROXY>
//    The proxy for a DYLAN runtime object that is not necessarily known
//    to be static. It is potentially relocatable, and therefore required
//    to be tracked.

define abstract class <tracked-dylan-runtime-proxy> (<dylan-runtime-proxy>)

  constant slot tracked-dylan-value :: <remote-object>,
    required-init-keyword: value:;

end class;

/* --- Currently not used.
   --- The app-server is releasing all non-static objects at the end
       of each debugger transaction.

///// <WEAKLY-TRACKED-DYLAN-RUNTIME-PROXY>
//    This is being tracked, but the tracking is allowed to go stale if
//    the remote application stops referencing the actual runtime object.

define class <weakly-tracked-dylan-runtime-proxy>
                                  (<tracked-dylan-runtime-proxy>)
end class;


///// <STRONGLY-TRACKED-DYLAN-RUNTIME-PROXY>
//    This is being tracked, and we force tracking to remain live, even
//    if the true runtime object becomes garbage in the remote application.

define class <strongly-tracked-dylan-runtime-proxy>
                                 (<tracked-dylan-runtime-proxy>)
end class;
*/

///// APPLICATION-PROXY-PRIMITIVE-NAME
//    A generic function that can generate the primitive name for an
//    application proxy, regardless of what kind of proxy it is.
//    The first method is a default which should never get called once
//    the full implementation is in place.

define method application-proxy-primitive-name
    (application :: <dfmc-application>, proxy :: <object>,
     #key decorate? = #t)
 => (name :: <string>)
  "Undescribed Application Object"
end method;

define method application-proxy-primitive-name
    (application :: <dfmc-application>, proxy :: <runtime-proxy>,
     #key decorate? = #t)
 => (name :: <string>)
  let target = application.application-target-app;
  let value = runtime-proxy-to-remote-value(application, proxy);
  print-dylan-object(target, value, length: 10, level: 3, 
		     decorate?: decorate?)
end method;

define method application-proxy-primitive-name
    (application :: <dfmc-application>, proxy :: <foreign-runtime-proxy>,
     #key decorate? = #t)
 => (name :: <string>)
  format-to-string("{%=}", proxy.static-foreign-value)
end method;

define method application-proxy-primitive-name
    (application :: <dfmc-application>, proxy :: <foreign-function-runtime-proxy>,
     #key decorate? = #t)
 => (name :: <string>)
  let symbol = proxy.static-foreign-symbol;
  let name = symbol.remote-symbol-name;
  let dll-context = symbol.remote-symbol-library;
  if (dll-context)
    format-to-string("%s!%s", dll-context.library-core-name, name)
  else
    name
  end
end method;

define method application-proxy-primitive-name
    (application :: <dfmc-application>, proxy :: <application-stack-frame>,
     #key decorate? = #t)
       => (name :: <string>)
  "Uninterpreted Stack Frame"
end method;

define method application-proxy-primitive-name
    (application :: <dfmc-application>, proxy :: <unwind-protect-frame>,
     #key decorate? = #t)
       => (name :: <string>)
  "Dylan Cleanup Frame"
end method;

define method application-proxy-primitive-name
    (application :: <dfmc-application>, proxy :: <remote-value>,
     #key decorate? = #t)
  => (name :: <string>)
  if (application.application-target-app)
    remote-value-as-string
      (application.application-target-app.debug-target-access-path,
       proxy,
       16)
  else
    "????????"
  end if
end method;

define method application-proxy-primitive-name
    (application :: <dfmc-application>, proxy :: <remote-register>,
     #key decorate? = #t)
  => (name :: <string>)
  proxy.register-name
end method;

define method application-proxy-primitive-name
    (application :: <dfmc-application>, proxy :: <remote-library>,
     #key decorate? = #t)
  => (name :: <string>)
  proxy.library-core-name
end method;

define method application-proxy-primitive-name
    (application :: <dfmc-application>, proxy :: <call-frame>,
     #key decorate? = #t)
       => (name :: <string>)
  let target = application.application-target-app;
  let path = target.debug-target-access-path;
  let (sym, obj, gen) = call-frame-function(target, proxy);
  let byte-offset = call-frame-code-offset(target, proxy);
  let ip = call-frame-instruction-pointer(target, proxy);
  let printed-ip = remote-value-as-string(path, ip, 16);
  if (dylan-call-frame?(target, proxy))
    if (sym)
      format-to-string("%s + 0x%x",
                       sym.remote-symbol-name, byte-offset);
    else
      format-to-string("0x%s (No symbols available)", printed-ip);
    end if
  elseif (sym)
    format-to-string("%s:%s + 0x%x",
                     sym.remote-symbol-library.library-core-name,
                     sym.remote-symbol-name, byte-offset);
  else
    format-to-string("0x%s (No symbols available)", printed-ip);
  end if;
end method;

define method application-proxy-primitive-name
    (application :: <dfmc-application>, proxy :: <remote-thread>,
     #key decorate? = #t)
 => (name :: <string>)
  let state-model = thread-state-model(application, proxy);
  let access-path-name = proxy.thread-name;
  let thread-name = state-model.thread-state-thread-name;
  // Cache the thread name since it can never change, and we need
  // to be able to return it after a thread has died.
  let (dylan-thread?, dm-name)
    = if (thread-name)
	values(#t, thread-name)
      else
	let target = application.application-target-app;
	if (target)
	  let (dylan-thread?, dm-name, dm-object)
	    = remote-thread-information(target, proxy);
	  if (dylan-thread? & dm-name)
	    state-model.thread-state-thread-name := dm-name;
	  end;
	  values(dylan-thread?, dm-name)
	else
	  values(#f, #f)
	end
      end;
  if (dylan-thread?)
    if (dm-name = access-path-name)
      "Anonymous Thread"
    else
      dm-name
    end if
  else
    "Foreign Thread"
  end if;
end method;

// Todo: Maybe split up into more methods for the various <debug-point>
// classes.

define method application-proxy-primitive-name
    (application :: <dfmc-application>, proxy :: <debug-point>,
     #key decorate? = #t)
       => (name :: <string>)
  format-to-string("Debug Point");
end method;

define method application-proxy-primitive-name
    (application :: <dfmc-application>, proxy :: <application-local-variable>,
     #key decorate? = #t)
       => (name :: <string>)
    proxy.local-lexical-name
end method;


///// $STALE-REMOTE-VALUE
//    This is a single (interned) <remote-value> instance that can be used
//    as the value of a tracked object that has gone stale, or an object
//    that couldn't be found at all. (Things get dirty if everything keeps
//    returning false-or all the time).

define constant $stale-remote-value = as-remote-value(0);

///// DEFINITION-ID-TO-STRING-TRIPLE (Internal convenience function)
//    Maps a <definition-id> to the name, module and library.

define method definition-id-to-string-triple (id :: <definition-id>)
 => (binding-name :: <string>,
     module-name :: <string>,
     library-name :: <string>)
  let module-id = id-module(id);
  let library-id = id-library(module-id);
  values (id-name(id), id-name(module-id), id-name(library-id))
end method;


///// RUNTIME-PROXY-TO-REMOTE-VALUE
//    Internal clients of this function must ensure a debugger transaction
//    before calling it.
//    A mapping within the context of a <dfmc-application>.
//    Given any general <runtime-proxy>, returns a <remote-value> for it.
//    This may require looking up the "current" value in the case of
//    a tracked proxy.
//    A <remote-value> is guaranteed to be returned from this function. If
//    the value is not valid, the result will be ID to the constant object
//    $STALE-REMOTE-VALUE.

define method runtime-proxy-to-remote-value
    (application :: <dfmc-application>, proxy :: <runtime-proxy>)
       => (value :: <remote-value>)
  $stale-remote-value
end method;

define method runtime-proxy-to-remote-value
    (application :: <dfmc-application>, proxy :: <remote-value>)
  => (value :: <remote-value>)
  proxy  // This is just identity - no conversion is necessary!
end method;

define method runtime-proxy-to-remote-value
    (application :: <dfmc-application>, proxy :: <call-frame>)
  call-frame-frame-pointer
    (application.application-target-app, proxy);
end method;

define method runtime-proxy-to-remote-value
    (application :: <dfmc-application>, proxy :: <foreign-runtime-proxy>)
       => (value :: <remote-value>)
  proxy.static-foreign-value
end method;

define method runtime-proxy-to-remote-value
    (application :: <dfmc-application>, proxy :: <foreign-function-runtime-proxy>)
       => (value :: <remote-value>)
  proxy.static-foreign-symbol.remote-symbol-address
end method;

define method runtime-proxy-to-remote-value
    (application :: <dfmc-application>, proxy :: <remote-library>)
  proxy.library-base-address
end method;

define method runtime-proxy-to-remote-value
    (application :: <dfmc-application>, 
       proxy :: <static-dylan-runtime-proxy>) => (value :: <remote-value>)
  proxy.static-dylan-value
end method;

define method runtime-proxy-to-remote-value
    (application :: <dfmc-application>,
       proxy :: <tracked-dylan-runtime-proxy>) => (value :: <remote-value>)
  remote-object-value(application.application-target-app, 
                      proxy.tracked-dylan-value) |
  $stale-remote-value
end method;

define method runtime-proxy-to-remote-value
    (application :: <dfmc-application>, proxy :: <application-variable>)
 => (value :: <remote-value>)
  let value = $stale-remote-value;
  block ()
    value := read-dylan-value(application.application-target-app, 
                              proxy.application-variable-address);
  exception (<remote-access-violation-error>)
    value := $stale-remote-value
  end block
end method;

define method runtime-proxy-to-remote-value
    (application :: <dfmc-application>, proxy :: <remote-thread>)
  => (value :: <remote-value>)
  let target = application.application-target-app;
  let (dylan-thread?, thread-name, dylan-thread-object) =
    remote-thread-information(target, proxy);
  dylan-thread-object | $stale-remote-value
end method;


///// APPLICATION-NAME-TO-RUNTIME-PROXY
//    Internal clients of this function must ensure a debugger transaction
//    before calling it.
//    A mapping within the context of a <dfmc-application>.
//    Given any triple of binding-name, module-name and library-name,
//    either finds or generates a <runtime-proxy> instance to describe it
//    in a canonical fashion.

define method application-name-to-runtime-proxy
    (application :: <dfmc-application>,
     binding-name :: <string>, module-name :: <string>, 
     library-name :: <string>,
     #key constant? = #t, address-already-known = #f, value-already-known = #f,
          class? = #f)
 => (proxy :: false-or(<runtime-proxy>))

  local method intern-id () => (id :: <definition-id>)
          make(<definition-id>,
               name: binding-name,
               module: make(<module-id>,
                            name: module-name,
                            library: make(<library-id>,
                                          name: library-name)));
	end method;

  local method generate-new-proxy () => (p :: false-or(<runtime-proxy>))
          let context = make(<dylan-name-context>,
                             library: library-name, module: module-name);
          let target = application.application-target-app;
          let (value, address) =
            block ()
              let (value-d, address-d) =
                resolve-dylan-name(target,
                                   binding-name,
                                   context,
                                   indirect?: #f);
              if (value-d & address-d)
                values(value-d, address-d)
              else
                resolve-dylan-name(target,
                                   binding-name,
                                   context,
                                   indirect?: #t);
              end if
            end block;

          if (address & value)
            if (address = value)
              // If this object has a precise symbolic name, it cannot be
              // subject to any relocations by the MM, and hence we need
              // only generate a static proxy.

              remote-value-to-runtime-proxy(application, value);

            // If an indirection was performed, then the binding name
            // must be a variable. A variable-style proxy must be
            // created: the DM will need to decide whether the variable
            // is thread local, based upon the contents of the value
            // cell.

	    elseif (thread-local-variable?(target, address))
              make(<application-thread-local-variable>,
                   name: binding-name, namespace: context, address: address);
            else
              make(<application-thread-global-variable>,
                   name: binding-name, namespace: context, address: address);
	    end if
	  else
            #f
	  end if;
	end method;

  let known-proxies = application.application-proxy-factory;
  let known-modules =
    element(known-proxies.proxy-factory-known-names, library-name,
            default: #f);

  if (known-modules)
    let known-names = element(known-modules, module-name, default: #f);
    if (known-names)
      let binding-proxy-pair = element(known-names, binding-name, default: #f);
      if (binding-proxy-pair)
        if (tail(binding-proxy-pair))
          tail(binding-proxy-pair);
	else
          let id = head(binding-proxy-pair);
          let proxy = generate-new-proxy();
          tail(binding-proxy-pair) := proxy;
          known-proxies.proxy-factory-proxy-to-id-mappings[proxy] := id;
          proxy;
	end if;
      else
        let proxy = generate-new-proxy();
        let id = intern-id();
        let the-pair = pair(id, proxy);
        add!(known-proxies.proxy-factory-ordered-data, the-pair);
        known-proxies.proxy-factory-proxy-to-id-mappings[proxy] := id;
        known-names[binding-name] := the-pair;
        proxy;
      end if
    else
      let proxy = generate-new-proxy();
      let id = intern-id();
      let the-pair = pair(id, proxy);
      add!(known-proxies.proxy-factory-ordered-data, the-pair);
      known-proxies.proxy-factory-proxy-to-id-mappings[proxy] := id;
      known-names := make(<string-table>);
      known-names[binding-name] := the-pair;
      known-modules[module-name] := known-names;
      proxy;
    end if
  else
    let proxy = generate-new-proxy();
    let id = intern-id();
    let the-pair = pair(intern-id(), proxy);
    add!(known-proxies.proxy-factory-ordered-data, the-pair);
    known-proxies.proxy-factory-proxy-to-id-mappings[proxy] := id;
    let known-names = make(<string-table>);
    known-modules := make(<string-table>);
    known-names[binding-name] := the-pair;
    known-modules[module-name] := known-names;
    known-proxies.proxy-factory-known-names[library-name] := known-modules;
    proxy;
  end if;
end method;


///// REMOTE-VALUE-TO-RUNTIME-PROXY
//    Internals clients of this function must ensure a debugger transaction
//    before calling it.
//    A mapping within the context of a <dfmc-application>.
//    Given any <remote-value>, either finds or generates a <runtime-proxy>
//    instance to describe it in a canonical fashion.

define method remote-value-to-runtime-proxy
    (application :: <dfmc-application>, value :: <remote-value>,
     #key classification :: false-or(<symbol>) = #f,
          address? :: <boolean> = #f)
 => (proxy :: <runtime-proxy>)
  let proxies = application.application-proxy-factory;
  if (address?)
    enquire-object(proxies.static-address-proxies, value)
  else
    enquire-object(proxies.static-proxies, value)
  end
    | enquire-object(proxies.per-transaction-proxies, value)
    | remote-value-to-new-runtime-proxy
        (application, value, 
	 classification: classification,
	 address?: address?)
end method;


define method remote-value-to-new-runtime-proxy
    (application :: <dfmc-application>, value :: <remote-value>,
     #key classification :: false-or(<symbol>) = #f,
          address? :: <boolean> = #f)
 => (proxy :: <runtime-proxy>)

  let target = application.application-target-app;
  let classification
    = classification
        | classify-runtime-value(target, value, address?: address?);

  let is-definition?
    = member?(classification,
	      #[#"dylan-class",
		#"dylan-method",
		#"dylan-generic-function"]);

  let is-primitive?
    = member?(classification,
	      #[#"dylan-integer",
		#"dylan-character"]);
  
  local method intern-value-proxy
	    (style :: <symbol>) => (p :: <runtime-proxy>)
	  let p
	    = select (classification)
		#"dylan-class" =>
		  make(<static-dylan-class-proxy>, value: value);
		#"foreign-function" =>
		  let st = debug-target-symbol-table(target);
		  let symbol 
		    = symbol-table-symbol-relative-address(st, value);
		  make(<foreign-function-runtime-proxy>, symbol: symbol);
		#"foreign-object" =>
		  make(<foreign-runtime-proxy>, value: value);
		otherwise =>
		  make(<static-dylan-runtime-proxy>, value: value);
	      end;
          unless (is-primitive?)
	    let proxy-factory = application.application-proxy-factory;
	    select (style)
	      #"static" =>
		let table
		  = if (address?) 
		      proxy-factory.static-address-proxies
		    else
		      proxy-factory.static-proxies
		    end;
		add-object(table, value, p);
	      #"dynamic" =>
		add-object(proxy-factory.per-transaction-proxies, value, p);
	      otherwise =>
		#f;
	    end
	  end;
	  p
	end method;

  if (is-definition?)
    let (binding-name, name-context, precise?, constant?)
      = find-dylan-name(target, value);
    if (precise? & constant?)
      let proxy = intern-value-proxy(#"static");
      let factory = application.application-proxy-factory;
      let library-name = name-context.context-library;
      let module-name = name-context.context-module;
      let library-id = make(<library-id>, name: library-name);
      let module-id 
	= make(<module-id>, name: module-name, library: library-id);
      let id = make(<definition-id>, name: binding-name, module: module-id);
      factory.proxy-factory-proxy-to-id-mappings[proxy] := id;
      proxy;
    else
      intern-value-proxy(#"dynamic");
    end if
  else
    intern-value-proxy(#"dynamic")
  end if;
end method;


///// EXCHANGE-VALUE-PROXY-FOR-BROWSABLE-CLASS-PROXY
//    Given a <runtime-proxy> modelling an instance of a browsable class
//    (ie, a proxy from a <composite-object>), return a proxy that
//    models the object's class.
//    This function can return #f for instances of non-browsable classes
//    (such as booleans, integers and characters).
//    Must be called from within a debugger transaction.

define method exchange-value-proxy-for-browsable-class-proxy
    (application :: <dfmc-application>, value-proxy :: <dylan-runtime-proxy>)
  => (class-proxy :: false-or(<dylan-runtime-proxy>))
  let factory = application.application-proxy-factory;
  let cache = factory.proxy-factory-last-object-exchanged-for-class;
  if (head(cache) == value-proxy)
    tail(cache)
  else
    let target = application.application-target-app;
    let instance-val = runtime-proxy-to-remote-value(application, value-proxy);
    let (class-val, incarnation, current-incarnation, immediate?) = 
      dylan-object-class(target, instance-val, browsable-only?: #t);
    if (class-val)
      let class-proxy = remote-value-to-runtime-proxy(application, class-val);
      head(cache) := value-proxy;
      tail(cache) := class-proxy;
      class-proxy;
    else
      #f
    end if;
  end if;
end method;


///// ENSURE-APPLICATION-VALUE-PROXY

define method ensure-application-value-proxy
    (application :: <dfmc-application>, object :: <application-object>)
  => (ensured-proxy :: false-or(<runtime-value-proxy>))
  let proxy = object.application-object-proxy;
  let target = application.application-target-app;
  unless (proxy)
    let id = environment-object-id(application.server-project, object);
    if (instance?(id, <definition-id>))
      let (name, module-name, library-name) =
        definition-id-to-string-triple(id);
      let context = make(<dylan-name-context>,
                         module: module-name, library: library-name);
      let (value-d, address-d)
        = resolve-dylan-name(target, name, context, indirect?: #f);
      let (value, address) =
        if (value-d & address-d)
          values(value-d, address-d)
        else
          resolve-dylan-name(target, name, context, indirect?: #t)
        end if;
      if (value)
        proxy := remote-value-to-runtime-proxy(application, value);
        object.application-object-proxy := proxy;
      end if
    end if;
  end unless;
  proxy;
end method;


///// ENSURE-APPLICATION-GLOBAL-VARIABLE-PROXY

define method ensure-application-global-variable-proxy
    (application :: <dfmc-application>, object :: <variable-object>)
  => (ensured-proxy :: false-or(<application-thread-global-variable>))
  let proxy = object.application-object-proxy;
  let target = application.application-target-app;
  unless (instance?(proxy, <application-thread-global-variable>))
    let id = environment-object-id(application.server-project, object);
    if (instance?(id, <definition-id>))
      let (name, module-name, library-name) =
        definition-id-to-string-triple(id);
      let context = make(<dylan-name-context>,
                         module: module-name, library: library-name);
      let (value, address)
        = resolve-dylan-name(target, name, context, indirect?: #t);
      if (address)
        proxy := make(<application-thread-global-variable>,
                      name: name, namespace: context, address: address);
        object.application-object-proxy := proxy;
      end if
    end if;
  end unless;
  proxy;
end method;


///// ENSURE-APPLICATION-THREAD-VARIABLE-PROXY

define method ensure-application-thread-variable-proxy
    (application :: <dfmc-application>, object :: <thread-variable-object>)
  => (ensured-proxy :: false-or(<application-thread-local-variable>))
  let proxy = object.application-object-proxy;
  let target = application.application-target-app;
  unless (instance?(proxy, <application-thread-local-variable>))
    let id = environment-object-id(application.server-project, object);
    if (instance?(id, <definition-id>))
      let (name, module-name, library-name) =
        definition-id-to-string-triple(id);
      let context = make(<dylan-name-context>,
                         module: module-name, library: library-name);
      let (value, address)
        = resolve-dylan-name(target, name, context, indirect?: #t);
      if (address)
        proxy := make(<application-thread-local-variable>,
                      name: name, namespace: context, address: address);
        object.application-object-proxy := proxy;
      end if
    end if;
  end unless;
  proxy;
end method;


///// CLASS-PROXY-APPROPRIATE-USER-OBJECT-MODEL
//    Given a <runtime-proxy> modelling a browsable user-defined class,
//    returns the class <USER-OBJECT>, or a subclass thereof, which should
//    instantiated as the environment model.

define method class-proxy-appropriate-user-object-model
    (application :: <dfmc-application>,
     class-proxy :: <static-dylan-class-proxy>)
  => (eclass :: subclass(<user-object>))
  if (class-proxy.class-proxy-environment-class-cached?)
    class-proxy.class-proxy-environment-class
  else
    unless (application.runtime-class-user-class-mappings-initialized?)
      initialize-user-object-class-mappings(application);
    end unless;
    let class-to-adopt = <user-object>;
    let target = application.application-target-app;
    let the-class = runtime-proxy-to-remote-value(application, class-proxy);
    block (exit)
      for (class-model-pair in application.runtime-class-user-class-mappings)
        let runtime-class = head(class-model-pair);
        let model-class = tail(class-model-pair);
        if (remote-subclass?(target, the-class, runtime-class))
          class-to-adopt := model-class;
          exit();
        end if;
      end for;
    end block;
    class-proxy.class-proxy-environment-class := class-to-adopt;
    class-proxy.class-proxy-environment-class-cached? := #t;
    class-to-adopt;
  end if
end method;

define method class-proxy-appropriate-user-object-model
    (application :: <dfmc-application>, class-proxy :: <dylan-runtime-proxy>)
  => (eclass :: subclass(<user-object>))
  <user-object>
end method;


///// CLASS-PROXY-BROWSER-INFORMATION
//    Given a <runtime-proxy> modelling a browsable class, obtain
//    all navigation information for instances of the class, and 
//    fill in caches.
//    Must be called from within a debugger transaction.

define method class-proxy-browser-information
    (application :: <dfmc-application>,
     class-proxy :: <static-dylan-class-proxy>,
     #key incarnation = #f)
  => (slots :: <sequence>, navigation :: <string-table>,
      repeat :: false-or(<string>), count-offset :: false-or(<integer>),
      element-size :: false-or(<integer>), 
      element-offset :: false-or(<integer>),
      class-slot-count :: <integer>)
  if (class-proxy.class-proxy-browser-data-cached? &
      (class-proxy.class-proxy-incarnation = incarnation))
    values(class-proxy.class-proxy-slots,
           class-proxy.class-proxy-navigation,
           class-proxy.class-proxy-repeat,
           class-proxy.class-proxy-count-offset,
           class-proxy.class-proxy-element-size,
           class-proxy.class-proxy-element-offset,
           class-proxy.class-proxy-class-slot-count)
  else
   let target = application.application-target-app;
   let class-val = runtime-proxy-to-remote-value(application, class-proxy);
   let (slots, navigation, repeat, count-offset, element-size, element-offset,
        class-slot-count)
     = dylan-class-browser-information
          (target, class-val, use-incarnation: incarnation);
   class-proxy.class-proxy-slots := slots;
   class-proxy.class-proxy-navigation := navigation;
   class-proxy.class-proxy-repeat := repeat;
   class-proxy.class-proxy-count-offset := count-offset;
   class-proxy.class-proxy-element-size := element-size;
   class-proxy.class-proxy-element-offset := element-offset;
   class-proxy.class-proxy-browser-data-cached? := #t;
   class-proxy.class-proxy-class-slot-count := class-slot-count;
   class-proxy.class-proxy-incarnation := incarnation;
   values(slots,
          navigation,
          repeat,
          count-offset,
          element-size,
          element-offset,
          class-slot-count)
  end if
end method;

define method class-proxy-browser-information
    (application :: <dfmc-application>,
     class-proxy :: <dylan-runtime-proxy>,
     #key incarnation = #f)
  => (slots :: <sequence>, navigation :: <string-table>,
      repeat :: false-or(<string>), count-offset :: false-or(<integer>),
      element-size :: false-or(<integer>), element-offset :: <integer>,
      class-slot-count :: <integer>)
  let target = application.application-target-app;
  let class-val = runtime-proxy-to-remote-value(application, class-proxy);
  dylan-class-browser-information
     (target, class-val, use-incarnation: incarnation)
end method;

/*  --- Not currently used.
    --  Proxies are disposed on a wholesale basis, rather than
        individually.
///// DISPOSE-RUNTIME-PROXY
//    Discards a proxy for a runtime value. Also tells the environment to
//    invalidate the proxy, and de-registers it with the DM if it is being
//    tracked.
//    Internal function: Assumes caller has set up a debugger transaction!

define method dispose-runtime-proxy
    (application :: <dfmc-application>, proxy :: <runtime-proxy>) => ()
//  invalidate-application-proxy(application.server-project, proxy);
end method;

define method dispose-runtime-proxy
    (application :: <dfmc-application>, 
     proxy :: <tracked-dylan-runtime-proxy>) => ()
  let target = application.application-target-app;
  free-remote-object(target, proxy.tracked-dylan-value);
//  invalidate-application-proxy(application.server-project, proxy);
end method;
*/
