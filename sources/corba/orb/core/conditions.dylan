Module: orb-core
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <undefined-server-function> (<simple-error>)
  keyword format-string: = "Undefined CORBA server function %=";
end class;

define sealed domain make (subclass(<undefined-server-function>));
define sealed domain initialize (<undefined-server-function>);

define method make (class == <undefined-server-function>, #key function)
 => (condition :: <undefined-server-function>)
  next-method(class, format-arguments: vector(function));
end method;

define class <coerce-object-error> (<simple-error>)
  keyword format-string: = "cannot coerce %= to %=, invalid IS-A relationship: %= -> %="
end class;

define sealed domain make (subclass(<coerce-object-error>));
define sealed domain initialize (<coerce-object-error>);

define method make (condition-class == <coerce-object-error>, #key class, object, type-id)
 => (condition :: <coerce-object-error>)
  next-method(condition-class, format-arguments: vector(object, class, object, type-id));
end method;

define class <coerce-any-object-error> (<simple-error>)
  keyword format-string: = "cannot coerce %= to %=, since it does represent an object reference";
end class;

define sealed domain make (subclass(<coerce-any-object-error>));
define sealed domain initialize (<coerce-any-object-error>);

define method make (class == <coerce-any-object-error>, #key any, class)
 => (condition :: <coerce-any-object-error>)
  next-method(class, format-arguments: vector(any, class))
end method;

define class <missing-ior-error> (<simple-error>)
  keyword format-string: = "no ior set for %=";
end class;

define sealed domain make (subclass(<missing-ior-error>));
define sealed domain initialize (<missing-ior-error>);

define method make (class == <missing-ior-error>, #key object)
 => (condition :: <missing-ior-error>)
  next-method(class, format-arguments: vector(object));
end method;

define class <union-branch-error> (<simple-error>)
  keyword format-string: = "Can't find type for %= in %=";
end class;

define sealed domain make (subclass(<union-branch-error>));
define sealed domain initialize (<union-branch-error>);

define method make (class == <union-branch-error>, #key discriminator, typecode)
 => (condition :: <union-branch-error>)
  next-method(class, format-arguments: vector(discriminator, typecode));
end method;

define class <no-matching-context-values> (<simple-error>)
  keyword format-string: = "no matching context values for pattern: %s";
end class;

define sealed domain make (subclass(<no-matching-context-values>));
define sealed domain initialize (<no-matching-context-values>);

define method make (class == <no-matching-context-values>, #key pattern)
 => (condition :: <no-matching-context-values>)
  next-method(class, format-arguments: vector(pattern));
end method;

define class <duplicate-context-name> (<simple-error>)
  keyword format-string: = "Duplicate context name: %s";
end class;

define sealed domain make (subclass(<duplicate-context-name>));
define sealed domain initialize (<duplicate-context-name>);

define method make (class == <duplicate-context-name>, #key name)
 => (condition :: <duplicate-context-name>)
  next-method(class, format-arguments: vector(name));
end method;
