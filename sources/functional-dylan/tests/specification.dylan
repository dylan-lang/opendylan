Module:    functional-dylan-test-suite
Synopsis:  Functional Objects extensions library test suite
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library-spec functional-dylan ()
  module functional-extensions;
  module simple-format;
  module simple-random;
  suite functional-dylan-regressions;
end library-spec functional-dylan;

define module-spec functional-extensions ()
  // The constants
  constant $unsupplied :: <object>;
  constant $unfound    :: <object>;

  // The classes
  sealed class <byte-character> (<character>);
  open abstract class <format-string-condition> (<condition>);
  sealed instantiable class <object-deque> (<deque>);
  sealed instantiable class <object-set> (<set>);
  open abstract instantiable class <set> (<collection>);
  open abstract class <simple-condition> (<condition>);
  open abstract class <stretchy-sequence> (<stretchy-collection>, <sequence>);
  sealed instantiable class <stretchy-object-vector> (<stretchy-vector>);

  // The functions
  open generic-function concatenate! (<sequence>, #"rest") => (<sequence>);
  function condition-to-string (<condition>) => (<string>);
  function debug-message (<string>, #"rest") => ();
  open generic-function difference
      (<sequence>, <sequence>, #"key", #"test") => (<sequence>);
  function false-or (<type>, #"rest") => (<type>);
  function fill-table! (<table>, <sequence>) => (<table>);
  open generic-function find-element
      (<collection>, <function>, #"key", #"skip", #"failure") => (<object>);
  function float-to-string (<float>) => (<string>);
  function found? () => (<boolean>);
  function ignorable (<object>) => ();
  function ignore (<object>) => ();
  function integer-to-string (<integer>, #"key", #"base") => (<string>);
  function number-to-string (<number>) => (<string>);
  function one-of (<type>, #"rest") => (<type>);
  function position
      (<sequence>, <object>, #"key", #"test", #"count") => (<integer>);
  function split
      (<string>, <character>, #"key", #"start", #"end", #"count", #"remove-if-empty")
   => (<sequence>);
  open generic-function remove-all-keys! (<mutable-collection>) => ();
  function string-to-integer
      (<string>, #"key", #"base", #"start", #"end")
   => (<integer>, <integer>);
  function subclass (<class>) => (<type>);
  function supplied? (<object>) => (<boolean>);
  function unfound () => (<object>);
  function unfound? () => (<boolean>);
  function unsupplied () => (<object>);
  function unsupplied? (<object>) => (<boolean>);

  // The macros
  macro-test assert-test;
  macro-test debug-assert-test;
  macro-test iterate-test;
  macro-test table-definer-test;
  macro-test timing-test;
  macro-test when-test;
end module-spec functional-extensions;

define module-spec simple-format ()
  function format-out (<string>, #"rest") => ();
  function format-to-string (<string>, #"rest") => (<string>);
end module-spec simple-format;

define module-spec simple-random ()
  sealed instantiable class <random> (<object>);
  function random (<integer>, #"key", #"random") => (<integer>);
end module-spec simple-random;
