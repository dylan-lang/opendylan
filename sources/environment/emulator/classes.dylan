Module:    emulator-environment-backend
Synopsis:  Emulator Environment Backend
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Class handling

define method environment-object-home-name
    (database :: <emulator-database>, object :: <class-object>)
 => (name :: false-or(<name-object>))
  let name = dylan-class-name(application-object-proxy(object));
  name & ensure-server-object-of-class(database, name, <binding-name-object>)
end method environment-object-home-name;

define method do-direct-subclasses
    (function, database :: <emulator-database>, class :: <class-object>, 
     #key client)
 => ()
  do-server-environment-objects
    (function, database,
     lisp-class-direct-subclasses(application-object-proxy(class)),
     <class-object>)
end method do-direct-subclasses;

define method do-direct-superclasses
    (function, database :: <emulator-database>, class :: <class-object>, 
     #key client)
 => ()
  let lisp-class = application-object-proxy(class);
  select (lisp-class)
    <object> =>
      #[];
    otherwise =>
      do-server-environment-objects
	(function, database,
	 lisp-class-direct-superclasses(lisp-class),
	 <class-object>);
  end
end method do-direct-superclasses;

define method do-direct-methods
    (function, database :: <emulator-database>, class :: <class-object>, 
     #key client)
 => ()
  do-server-environment-objects
    (function, database,
     lisp-class-direct-methods(application-object-proxy(class)),
     <method-object>)
end method do-direct-methods;

define method do-direct-slots
    (function, database :: <emulator-database>, class :: <class-object>, 
     #key client)
 => ()
  do-server-environment-objects
    (function, database,
     lisp-class-direct-slots(application-object-proxy(class)),
     <slot-object>)
end method do-direct-slots;

define method do-all-superclasses
    (function, database :: <emulator-database>, class :: <class-object>, 
     #key client)
 => ()
  //---*** Should this be in the front-end?
  error("do-all-superclasses not implemented yet!")
end method do-all-superclasses;

define method do-all-slots
    (function, database :: <emulator-database>, class :: <class-object>, 
     #key client)
 => ()
  do-server-environment-objects
    (function, database,
     lisp-class-slots(application-object-proxy(class)),
     <slot-object>)
end method do-all-slots;

define method do-init-keywords
    (function, database :: <emulator-database>, class :: <class-object>, 
     #key client)
 => ()
  do-server-environment-objects
    (function, database,
     dylan-class-initargs(application-object-proxy(class)),
     <symbol-object>)
end method do-init-keywords;


/// Slots

define method environment-object-home-name
    (database :: <emulator-database>, object :: <slot-object>)
 => (name :: false-or(<name-object>))
  let name = dylan-slot-contents(compiler-object-proxy(object));
  ensure-server-object-of-class(database, name, <binding-name-object>);
end method environment-object-home-name;

define method slot-getter
    (database :: <emulator-database>, slot :: <slot-object>)
 => (getter :: false-or(<function-object>))
  let gf-proxy = slot-definition-getter(compiler-object-proxy(slot));
  let gf = ensure-server-object(database, gf-proxy); 
  let methods = generic-function-object-methods(database, gf);
  if (size(methods) = 1) methods[0] end
end method slot-getter;

define method slot-setter
    (database :: <emulator-database>, slot :: <slot-object>)
 => (getter :: false-or(<function-object>))
  //---*** Fill this in!
  #f
end method slot-setter;

define method slot-class
    (database :: <emulator-database>, slot :: <slot-object>)
 => (class :: <class-object>)
  let getter = slot-getter(database, slot);
  let specializers = getter & method-specializers(database, getter);
  let class = specializers & (size(specializers) = 1) & specializers[0];
  class | error("Unable to find class for slot %=", slot)
end method slot-class;

define method slot-type
    (database :: <emulator-database>, slot :: <slot-object>)
 => (type :: <type-object>)
  let type = slot-definition-type(compiler-object-proxy(slot));
  case
    dylan-class?(type) =>
      ensure-server-object-of-class(database, type, <class-object>);
    type =>
      unless (instance?(type, <list>) & empty?(type))
        ensure-server-object-of-class(database, type, <type-object>)
      end;
    otherwise =>
      ensure-server-object-of-class(database, <object>, <class-object>);
  end;
end method slot-type;

define method slot-type
    (database :: <emulator-database>, slot :: <slot-object>)
 => (type :: <type-object>)
  let type = slot-definition-type(compiler-object-proxy(slot));
  case
    dylan-class?(type) =>
      ensure-server-object-of-class(database, type, <class-object>);
    type =>
      unless (instance?(type, <list>) & empty?(type))
        ensure-server-object-of-class(database, type, <type-object>)
      end;
    otherwise =>
      ensure-server-object-of-class(database, <object>, <class-object>);
  end;
end method slot-type;

define method slot-init-keyword
    (database :: <emulator-database>, slot :: <slot-object>)
 => (keyword :: false-or(<symbol-object>))
  let initargs
    = slot-definition-initargs(compiler-object-proxy(slot));
  unless (empty?(initargs))
    ensure-server-object-of-class(database, initargs[0], <symbol-object>)
  end
end method slot-init-keyword;

define method slot-init-value 
    (database :: <emulator-database>, slot :: <slot-object>)
 => (init-value)
  //---*** What should we do here?
  #f
end method slot-init-value;

define method slot-init-function
    (database :: <emulator-database>, slot :: <slot-object>)
 => (init-value)
  //---*** What should we do here?
  #f
end method slot-init-function;

define method slot-allocation
    (database :: <emulator-database>, slot :: <slot-object>)
 => (keywords :: <sequence>)
  #()
end method slot-allocation;
