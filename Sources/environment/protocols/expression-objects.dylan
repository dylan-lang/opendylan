Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Expression objects
///
/// Expression objects represent arbitrary expressions in Dylan, usually
/// on the right-hand side of an assignment.

define open abstract class <expression-object> (<dylan-compiler-object>)
end class <expression-object>;

// Expressions that evaluate to a type
define open class <type-expression-object> (<expression-object>)
end class <type-expression-object>;

// The canonical type expression of arbitrary complexity (only one instance)
define sealed class <complex-type-expression-object> (<type-expression-object>)
end class <complex-type-expression-object>;

define constant $complex-type-expression-object :: <complex-type-expression-object>
  = make(<complex-type-expression-object>,
         compiler-object-proxy: vector(#"complex-type-expression-object"));


/// Naming

//---*** cpage: 1997.12.17  I'm not sure which of these methods is
//              really necessary.

define method environment-object-type-name
    (object :: <expression-object>)  => (name :: <string>)
  "Expression"
end method environment-object-type-name;

define method environment-object-type-name
    (object :: <type-expression-object>)  => (name :: <string>)
  "Type expression"
end method environment-object-type-name;

define method environment-object-type-name
    (object :: <complex-type-expression-object>)  => (name :: <string>)
  "Complex type expression"
end method environment-object-type-name;

define method environment-object-primitive-name
    (server :: <server>, expression :: <complex-type-expression-object>)
 => (name :: <string>)
  "{complex type}"
end method environment-object-primitive-name;

