Module:       warnings-test-suite
Synopsis:     A test suite for compiler warnings
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Unreferenced definitions

define constant $unreferenced-constant = 10;

define variable *unreferenced-variable* = 10;

define open generic unreferenced-generic
    () => ();

define method unreferenced-method
    () => ()
  #f
end method unreferenced-method;

define function unreferenced-function
    () => ()
  #f
end function unreferenced-function;

define class <unreferenced-class> (<object>)
end class <unreferenced-class>;

define macro unreferenced-macro
  { unreferenced-macro (?x:body) }
    => { ?x }
end macro unreferenced-macro;

define function function-with-unreferenced-argument
    (x, y) => (x)
  x
end function function-with-unreferenced-argument;

define function function-with-unreferenced-local
    () => ()
  let unreferenced-local = 100;
  #f
end function function-with-unreferenced-local;

define function unreferenced-expression
    () => ()
  let x = 100;
  let y = 20;
  x = y;
  x
end function unreferenced-expression;

define function unreferenced-value
    (x) => ()
  vector(x)
end function unreferenced-value;

define class <unreferenced-slot-class> (<object>)
  slot unreferenced-slot :: <object>;
  sealed slot unreference-sealed-slot :: <object>;
  constant slot unreferenced-constant-slot :: <object>;
  constant sealed slot unreferenced-constant-sealed-slot :: <object>;
end class <unreferenced-slot-class>;

ignore(function-with-unreferenced-argument,
       function-with-unreferenced-local,
       unreferenced-expression,
       unreferenced-value,
       <unreferenced-slot-class>);


/// Undefined references

define class <class-with-undefined-superclass> (<undefined-superclass>)
end class <class-with-undefined-superclass>;

define function undefined-references
    () => ()
  let x = $undefined-constant;
  undefined-function(undefined-variable)
end function undefined-references;

ignore(<class-with-undefined-superclass>, undefined-references);
