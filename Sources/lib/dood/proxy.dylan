Module:       dood
Synopsis:     The Dylan object-oriented database
Author:       Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//define constant $unbound-proxy-value = list("UNBOUND PROXY");

define open dood-class <dood-proxy> (<dood-mapped-and-owned-object>)
  // weak slot dood-proxy-value, 
  //   init-value: $unbound-proxy-value, init-keyword: object:;
end dood-class;

define inline method lookup-proxy (dood :: <dood>, object) => (false-or-proxy)
  element(dood-disk-objects(dood), object, default: #f);
end method;

define inline method maybe-lookup-proxy
    (dood :: <dood>, object) => (false-or-proxy)
  unless (dood-batch-mode?(dood))
    lookup-proxy(dood, object)
  end unless;
end method;

define method dood-clean-proxies (dood :: <dood>) => ()
  dood-proxies(dood) := make(<stretchy-vector>);
  remove-all-keys!(dood-disk-objects(dood));
end method;

define method install-proxy (dood :: <dood>, object, proxy :: <dood-proxy>)
  dood-disk-objects(dood)[object] := proxy;
  unless (dood-batch-mode?(dood))
    dood-proxies(dood) := add!(dood-proxies(dood), object);
    // dood-proxy-value(proxy) := object;
  end unless;
end method;

define method install-read-proxy
    (dood :: <dood>, object, proxy :: <dood-proxy>)
  unless (dood-batch-mode?(dood))
    install-proxy(dood, object, proxy)
  end unless;
end method;

define inline function dood-as-proxy
    (dood :: <dood>, object, make-proxy :: <function>, #rest arguments)
 => (proxy :: <dood-proxy>)
  // if (dood-batch-mode?(dood))
    maybe-lookup-proxy(dood, object)
      | begin
	  let proxy = apply(make-proxy, dood, object, arguments);
	  install-proxy(dood, object, proxy);
	  proxy
	end
  // else 
  //   apply(make-proxy, dood, object, arguments)
  // end if
end function;

define open generic dood-restore-proxy
    (dood :: <dood>, proxy :: <dood-proxy>) => (memory-object);

define open generic dood-disk-object-default 
    (dood :: <dood>, object) => (disk-object);

define method dood-disk-object-default
    (dood :: <dood>, object) => (disk-object)
  object
end method;

define method dood-disk-object 
    (dood :: <dood>, object) => (disk-object)
  dood-disk-object-default(dood, object);
end method;

define method dood-disk-object
    (dood :: <dood>, object :: <dood-mapped-and-owned-object>) => (object)
  /*
  let external-dood = object-dood(object);
  if (external-dood & external-dood ~== dood) // SANITY CHECK
    if (dood-name(external-dood) == dood-name(dood))
      // break("DIRT FOUND IN DOOD %=", object);
      object-dood(object)  := dood;
      // dood-pointer(object) := #f;
      object
    else
      dood-as-proxy(dood, object, dood-make-cross-proxy, external-dood)
    end if
  else 
    object
  end if
  */
  object
end method;

define method read-object-using-class-at
    (dood :: <dood>, class :: subclass(<dood-proxy>), address :: <address>) 
 => (object)
  let object = dood-read-object-of-at(dood, class, address);
  let value  = dood-restore-proxy(dood, object);
  install-read-proxy(dood, value, object);
  dood-format("REREGISTERING PROXY\n");
  dood-register-read-object(dood, value, address); 
  // dood-proxy-value(object) := value;
  value
end method;

//// BINDINGS

define open class <dood-program-module-proxy> (<dood-proxy>)
  constant slot dood-proxy-library-name, required-init-keyword: library:;
  constant slot dood-proxy-module-name,  required-init-keyword: module:;
end class;

define open class <dood-program-binding-proxy> (<dood-proxy>)
  constant slot dood-proxy-module,        required-init-keyword: module:;
  constant slot dood-proxy-variable-name, required-init-keyword: variable:;
end class;

define function dood-as-program-module-proxy
    (dood :: <dood>, library-name, module-name)
 => (proxy :: <dood-program-module-proxy>)
  let library-name = library-name & as(<symbol>, library-name);
  let module-name  = as(<symbol>, module-name);
  let proxies = dood-module-proxies(dood);
  let proxies :: <dood-table>
    = element(proxies, library-name, default: #f)
        | (element(proxies, library-name) := make(<dood-table>));
  element(proxies, module-name, default: #f)
    | (element(proxies, module-name) 
         := make(<dood-program-module-proxy>,
                 module: module-name, library: library-name))
end function;

define function dood-make-program-binding-proxy
    (dood :: <dood>, object)
 => (proxy :: <dood-program-binding-proxy>)
  let (variable, module, library) = locate-variable(object);
  if (~variable)
    error("Couldn't locate program variable for %=", object);
  end;
  make(<dood-program-binding-proxy>,
       variable: variable, 
       module:   dood-as-program-module-proxy(dood, library, module))
end function;

define method dood-disk-object 
    (dood :: <dood>, object :: <generic-function>)
 => (proxy :: <dood-program-binding-proxy>)
  dood-as-proxy(dood, object, dood-make-program-binding-proxy)
end method;

define method dood-disk-object 
    (dood :: <dood>, object :: <function>)
 => (proxy :: <dood-program-binding-proxy>)
  error("NON GENERIC-FUNCTIONS ARE UNSUPPORTED IN DOOD");
end method;

define method dood-restore-proxy 
    (dood :: <dood>, proxy :: <dood-program-module-proxy>) => (object)
  proxy
end method;

define method dood-restore-proxy 
    (dood :: <dood>, proxy :: <dood-program-binding-proxy>) => (object)
  let mod-proxy = dood-proxy-module(proxy);
  variable-value
    (dood-proxy-variable-name(proxy), 
     dood-proxy-module-name(mod-proxy), 
     dood-proxy-library-name(mod-proxy)); 
end method;

// define constant <dood-class-program-binding-proxy>
//   = <dood-program-binding-proxy>;
// define constant dood-make-class-program-binding-proxy
//   = dood-make-program-binding-proxy;
// define method dood-disk-object 
//     (dood :: <dood>, object :: <class>)
//  => (proxy :: <dood-program-binding-proxy>)
//   dood-as-proxy(dood, object, dood-make-program-binding-proxy)
// end method;

define class <dood-class-program-binding-proxy> (<dood-program-binding-proxy>)
end class;

define function dood-make-class-program-binding-proxy
    (dood :: <dood>, object :: <class>)
 => (proxy :: <dood-class-program-binding-proxy>)
  let (variable, module, library)
    = locate-variable(object); // TODO: BOOTSTRAPPING
  let (variable, module, library)
    = if (variable)
	values(variable, module, library)
      else 
	class->variable(object);
      end if;
  if (~variable)
    error("Couldn't locate program variable for %=", object);
  end;
  make(<dood-class-program-binding-proxy>,
       variable: variable, 
       module:   dood-as-program-module-proxy(dood, library, module))
end function;

define method dood-disk-object 
    (dood :: <dood>, object :: <class>)
 => (proxy :: <dood-class-program-binding-proxy>)
  dood-as-proxy(dood, object, dood-make-class-program-binding-proxy)
end method;

define method dood-restore-proxy 
    (dood :: <dood>, proxy :: <dood-class-program-binding-proxy>) 
 => (object :: <class>)
  let mod-proxy = dood-proxy-module(proxy);
  local method booted-lookup ()
	  variable->class
	    (as(<string>, dood-proxy-variable-name(proxy)), 
	     as(<string>, dood-proxy-module-name(mod-proxy)), 
	     as(<string>, dood-proxy-library-name(mod-proxy))); 
	end method,
        method boot-lookup ()
	  variable-value
	    (dood-proxy-variable-name(proxy), 
	     dood-proxy-module-name(mod-proxy), 
	     dood-proxy-library-name(mod-proxy)); 
	end method;
  block ()
    boot-lookup() | booted-lookup(); // TODO: BOOTSTRAPPING
  exception (<error>)
    booted-lookup();
  end block;
end method;

///
/// SLOT-VALUE
///

define primary class <dood-address-proxy> (<dood-proxy>)
  constant slot proxy-address :: false-or(<address>) = #f,
    init-keyword: address:;
end class;

define class <dood-slot-value-proxy> (<dood-address-proxy>)
end class;

define class <dood-slot-value-proxy-n> (<dood-slot-value-proxy>)
  constant slot proxy-slot-descriptor :: <dood-slot-descriptor>, 
    required-init-keyword: slot-descriptor:;
end class;

/*
define constant $fixed-slot-value-proxy-classes :: <stretchy-object-vector>
  = make(<stretchy-vector>);

define macro fixed-offset-slot-value-proxy-definer
  { define fixed-offset-slot-value-proxy ?:name ?offset:expression }
    => { define class "<dood-slot-value-" ## ?name ## ">"
             (<dood-slot-value-proxy>)
           keyword slot-descriptor:;
         end class;
         define method proxy-slot-descriptor 
             (proxy :: "<dood-slot-value-" ## ?name ## ">")
          => (res :: <dood-slot-descriptor>) 
           ?offset
         end method;
         // HACK: EMULATOR HACK
         size($fixed-slot-value-proxy-classes) := ?offset + 1;
         element($fixed-slot-value-proxy-classes, ?offset) 
           := "<dood-slot-value-" ## ?name ## ">" }
end macro;

define fixed-offset-slot-value-proxy proxy-0 0;
define fixed-offset-slot-value-proxy proxy-1 1;
define fixed-offset-slot-value-proxy proxy-2 2;
define fixed-offset-slot-value-proxy proxy-3 3;
define fixed-offset-slot-value-proxy proxy-4 4;
define fixed-offset-slot-value-proxy proxy-5 5;
define fixed-offset-slot-value-proxy proxy-6 6;
define fixed-offset-slot-value-proxy proxy-7 7;
define fixed-offset-slot-value-proxy proxy-8 8;
define fixed-offset-slot-value-proxy proxy-9 9;
define fixed-offset-slot-value-proxy proxy-10 10;
define fixed-offset-slot-value-proxy proxy-11 11;
define fixed-offset-slot-value-proxy proxy-12 12;
define fixed-offset-slot-value-proxy proxy-13 13;
define fixed-offset-slot-value-proxy proxy-14 14;
define fixed-offset-slot-value-proxy proxy-15 15;
define fixed-offset-slot-value-proxy proxy-16 16;
define fixed-offset-slot-value-proxy proxy-17 17;
define fixed-offset-slot-value-proxy proxy-18 18;
define fixed-offset-slot-value-proxy proxy-19 19;
define fixed-offset-slot-value-proxy proxy-20 20;
define fixed-offset-slot-value-proxy proxy-21 21;
define fixed-offset-slot-value-proxy proxy-22 22;
define fixed-offset-slot-value-proxy proxy-23 23;
define fixed-offset-slot-value-proxy proxy-24 24;
define fixed-offset-slot-value-proxy proxy-25 25;
define fixed-offset-slot-value-proxy proxy-26 26;
define fixed-offset-slot-value-proxy proxy-27 27;
define fixed-offset-slot-value-proxy proxy-28 28;
define fixed-offset-slot-value-proxy proxy-29 29;
*/

define inline method lookup-slot-value-proxy-class 
    (slotd :: <dood-slot-descriptor>) => (res :: <class>)
  // if (slotd >= size($fixed-slot-value-proxy-classes))
    <dood-slot-value-proxy-n>
  // else
  //   element($fixed-slot-value-proxy-classes, slotd)
  // end if;
end method;

define inline method make-slot-value-proxy 
    (dood :: <dood>, address :: <address>,
     disk-offset :: <integer>, slot-descriptor :: <dood-slot-descriptor>)
 => (res :: <dood-slot-value-proxy>)
  make(lookup-slot-value-proxy-class(slot-descriptor), 
       dood:            dood,
       address:         address,
       slot-descriptor: slot-descriptor)
end method;

define sealed domain object-dood (<dood-address-proxy>);
define sealed domain object-dood-setter (<dood>, <dood-address-proxy>);
define sealed domain initialize (<dood-address-proxy>);
define sealed domain make (subclass(<dood-address-proxy>));

define inline function lazy-value? (object) => (well? :: <boolean>)
  instance?(object, <dood-address-proxy>)
end function;

define constant dood-lazy-value? = lazy-value?;

/*
define constant $slot-value-proxies = make(<dood-table>);

define method make-slot-value-proxy 
    (disk-offset :: <integer>, slot-descriptor :: <dood-slot-descriptor>)
 => (res :: <dood-slot-value-proxy>)
  let proxies :: <dood-table>
    = element($slot-value-proxies, disk-offset, default: #f)
        | (element($slot-value-proxies, disk-offset) := make(<table>));
  element(proxies, slot-descriptor, default: #f)
    | (element(proxies, slot-descriptor) 
         := make(<dood-slot-value-proxy>, 
                 slot-offset:     disk-offset,
                 slot-descriptor: slot-descriptor));
end method;
*/

define method make-address-proxy 
    (dood :: <dood>, address :: <address>, disk-offset :: <integer>)
 => (res :: <dood-address-proxy>)
 make(<dood-address-proxy>, 
      dood:    dood,
      address: address + disk-offset + 1)
end method;

define function dood-force-address-proxy 
    (x :: <dood-address-proxy>) => (res)
  let state :: <dood-state> = object-dood-state(x);
  let dood :: <dood>        = dood-dood-state(state);
  with-dood-state (dood, state)
    let address          = proxy-address(x);
    with-saved-position (dood)
      read-object-at(dood, address);
    end with-saved-position
  end with-dood-state;
end function;

define function dood-force-slot-value-proxy 
    (x :: <dood-slot-value-proxy>) => (res)
  let state :: <dood-state> = object-dood-state(x);
  let dood :: <dood>        = dood-dood-state(state);
  with-dood-state (dood, state)
    let address = proxy-address(x);
    with-saved-position (dood)
      read-pointer(dood, address);
    end with-saved-position
  end with-dood-state;
end function;

define inline function dood-maybe-force-address-proxy 
    (x) => (value, forced? :: <boolean>)
  if (lazy-value?(x))
    values(dood-force-address-proxy(x), #t)
  else
    values(x, #f)
  end if
end function;

define inline function dood-maybe-force-slot-value-proxy 
    (x) => (value, forced? :: <boolean>)
  if (lazy-value?(x))
    values(dood-force-slot-value-proxy(x), #t)
  else
    values(x, #f)
  end if
end function;

define variable *trace-allocation?* = #f;

define inline function dood-force-lazy-slot-value-proxy (object, x) => (res)
  if (lazy-value?(x))
    if (*trace-allocation?*)
      let slotds = slot-descriptors(object-class(object));
      depth-format-out("FORCING %= %=\n", object-class(object), 
	     	       slot-getter(element(slotds, proxy-slot-descriptor(x))));
      *print-depth* := *print-depth* + 1;
    end if;
    let value = dood-force-slot-value-proxy(x);
    dood-slot-value(object, proxy-slot-descriptor(x)) := value;
    if (*trace-allocation?*)
      *print-depth* := *print-depth* - 1;
    end if;
    value
  else
    x
  end if
end function;

define inline function dood-lazy-slot-value (object, getter :: <function>)
  dood-force-lazy-slot-value-proxy(object, getter(object))
end function;

define inline function dood-force-disk-slot-value-proxy (object, x) => (res)
  if (lazy-value?(x))
    dood-force-slot-value-proxy(x)
  else
    x
  end if
end function;

define inline function dood-disk-slot-value (object, getter :: <function>) 
  dood-force-disk-slot-value-proxy(object, getter(object));
end function;

//// CROSS DOOD

define open class <dood-cross-proxy> (<dood-proxy>)
  constant slot dood-proxy-dood-name, required-init-keyword: dood-name:;
end class;

define open generic dood-make-cross-proxy
    (dood :: <dood>, object, external-dood :: <dood>) => (object);

//// WRAPPER PROXY

define open dood-class <dood-wrapper-proxy> (<dood-proxy>)
  constant slot dood-wrapper-proxy-object, 
    required-init-keyword: object:;
  // @@@@ REMOVE ME @@@@
  weak constant slot dood-wrapper-proxy-object-address = #f; 
end dood-class;

define method walk-slots
    (dood :: <dood>, info :: <walk-info>, proxy :: <dood-wrapper-proxy>)
  let object = dood-wrapper-proxy-object(proxy);
  let predefined-address
    = element(dood-predefine-addresses(dood), object, default: #f);
  // HACK: SHOULDN'T REALLOCATE
  let address :: <address>
    = predefined-address | dood-allocate-instance(dood, object);
  next-method(); 
  // WRITE OVER WRAPPED OBJECT SLOT WITH REAL OBJECT
  if (walk-info-commit?(info))
    let proxy-address :: <address> = dood-walked-address(dood, proxy);
    dood-write-at(dood, tag-as-address(object, address), proxy-address + 1);
  end if;
  // WALK REAL OBJECT
  unless (predefined-address)
    walk-object(dood, info, object, address);
  end unless;
end method;

define method read-object-using-class-at
    (dood :: <dood>, class :: subclass(<dood-wrapper-proxy>), 
     address :: <address>) 
 => (object)
  dood-forwarding-address(dood) := address;
  with-saved-position (dood)
    let object = read-object(dood);
  end with-saved-position;
  let proxy = next-method();
  proxy
end method;

// eof

