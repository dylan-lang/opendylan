Module: corba-protocol
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <unknown-type-any-coercion-error> (<simple-error>)
  keyword format-string: = "cannot coerce %=, unable to determine native type";
end class;

define sealed domain make (subclass(<unknown-type-any-coercion-error>));
define sealed domain initialize (<unknown-type-any-coercion-error>);

define method make (class == <unknown-type-any-coercion-error>, #key any)
 => (condition :: <unknown-type-any-coercion-error>)
  next-method(class, format-arguments: vector(any));
end method;

define class <unknown-type-any-coercion-restart> (<simple-restart>)
  keyword format-string: = "Coerce to given native type";
  constant slot restart-native-type :: <type>, required-init-keyword: type:;
end class;

define sealed domain make (subclass(<unknown-type-any-coercion-restart>));
define sealed domain initialize (<unknown-type-any-coercion-restart>);

define class <incompatible-type-any-coercion-error> (<simple-error>)
  keyword format-string: = "Cannot coerce %= containing %= to type %=";
end class;

define sealed domain make (subclass(<incompatible-type-any-coercion-error>));
define sealed domain initialize (<incompatible-type-any-coercion-error>);

define method make (condition-class == <incompatible-type-any-coercion-error>, #key any, value, class)
 => (condition :: <incompatible-type-any-coercion-error>)
  next-method(condition-class, format-arguments: vector(any, corba/any/value(any), class));
end method;

define class <inconsistent-any> (<simple-error>)
  keyword format-string: = "inconsistent type and value for Any: %=";
end class;

define method make (condition-class == <inconsistent-any>, #key any)
  => (condition :: <inconsistent-any>)
  next-method(condition-class, format-arguments: vector(any));
end method;
