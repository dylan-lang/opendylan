Module:       dylan-test-suite
Synopsis:     Dylan test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define interface-specification-suite dylan-arithmetic-specification-suite ()
  /// Numbers
  open abstract class <number> (<object>);
  sealed abstract class <complex> (<number>);
  sealed abstract class <real> (<complex>);
  sealed abstract class <float> (<real>);
  sealed class <single-float> (<float>);
  sealed class <double-float> (<float>);
  sealed class <extended-float> (<float>);
  sealed abstract class <rational> (<real>);
  sealed class <integer> (<rational>);

  /// Arithmetic functions
  function odd? (<integer>) => (<boolean>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1297";
  function even? (<integer>) => (<boolean>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1297";
  function zero? (<integer>) => (<boolean>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1297";
  function positive? (<integer>) => (<boolean>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1297";
  function negative? (<integer>) => (<boolean>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1297";
  open generic function integral? (<integer>) => (<boolean>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1297";
  open generic function \+ (<object>, <object>) => (#"rest");
  open generic function \* (<object>, <object>) => (#"rest");
  open generic function \- (<object>, <object>) => (#"rest");
  open generic function \/ (<object>, <object>) => (#"rest");
  open generic function negative (<object>) => (#"rest");
  function floor (<real>) => (<integer>, <real>);
  function ceiling (<real>) => (<integer>, <real>);
  function round (<real>) => (<integer>, <real>);
  function truncate (<real>) => (<integer>, <real>);
  function floor/ (<real>, <real>) => (<integer>, <real>);
  function ceiling/ (<real>, <real>) => (<integer>, <real>);
  function round/ (<real>, <real>) => (<integer>, <real>);
  function truncate/ (<real>, <real>) => (<integer>, <real>);
  function modulo (<real>, <real>) => (<real>);
  function remainder (<real>, <real>) => (<real>);
  open generic function \^ (<object>, <object>) => (#"rest");
  open generic function abs (<object>) => (#"rest");
  function logior (#"rest") => (<integer>);
  function logxor (#"rest") => (<integer>);
  function logand (#"rest") => (<integer>);
  function lognot (<integer>) => (<integer>);
  function logbit? (<integer>, <integer>) => (<boolean>);
  function ash (<integer>, <integer>) => (<integer>);
  open generic function lcm (<object>, <object>) => (<object>);
  open generic function gcd (<object>, <object>) => (<object>);
end dylan-arithmetic-specification-suite;

define interface-specification-suite dylan-collections-specification-suite ()
  /// Collections
  open abstract class 
      <collection> (<object>);
  open abstract class 
      <explicit-key-collection> (<collection>);
  open abstract class 
      <sequence> (<collection>);
  open abstract class 
      <mutable-collection> (<collection>);
  open abstract class 
      <mutable-explicit-key-collection>
    (<explicit-key-collection>, <mutable-collection>);
  open abstract class 
      <mutable-sequence>  (<sequence>, <mutable-collection>);
  open abstract class 
      <stretchy-collection> (<collection>);
  open abstract instantiable class
      <array> (<mutable-sequence>);
  open abstract instantiable class 
      <vector> (<array>);
  sealed abstract instantiable class 
      <simple-vector> (<vector>);
  sealed instantiable class 
      <simple-object-vector> (<vector>);
  open abstract instantiable primary class 
      <stretchy-vector> (<vector>, <stretchy-collection>);
  open abstract instantiable primary class 
      <deque> (<mutable-sequence>, <stretchy-collection>);
  sealed abstract instantiable class 
      <list> (<mutable-sequence>);
  sealed instantiable class 
      <pair> (<list>);
  sealed instantiable class 
      <empty-list> (<list>);
  open abstract instantiable primary class 
      <range> (<sequence>);
  open abstract instantiable class 
      <string> (<mutable-sequence>);
  sealed instantiable class 
      <byte-string> (<string>, <vector>);
  //--- <unicode-string> isn't part of our implementation
  // sealed instantiable class
  //    <unicode-string> (<string>, <vector>);
  open abstract instantiable primary class 
      <table> (<mutable-explicit-key-collection>, <stretchy-collection>);
  open abstract instantiable class 
      <object-table> (<table>);

  /// Collection functions
  open generic function empty? (<object>) => (<boolean>);
  open generic function size (<object>) => (#"rest");
  open generic function size-setter (<object>, <object>) => (<object>);
  open generic function rank (<array>) => (<integer>);
  open generic function row-major-index (<array>, #"rest") => (<integer>);
  open generic function dimensions (<array>) => (<sequence>);
  open generic function dimension (<array>, <integer>) => (<integer>);
  open generic function key-test (<collection>) => (<function>);
  open generic function key-sequence (<collection>) => (<sequence>);
  open generic function element 
      (<collection>, <object>, #"key", #"default") => (<object>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  open generic function element-setter
      (<object>, <mutable-collection>, <object>) => (<object>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  open generic function aref (<array>, #"rest") => (<object>);
  open generic function aref-setter (<object>, <array>, #"rest") => (<object>);
  function first (<sequence>, #"key", #"default") => (<object>);
  function second (<sequence>, #"key", #"default") => (<object>);
  function third (<sequence>, #"key", #"default") => (<object>);
  function first-setter (<object>, <mutable-sequence>) => (<object>);
  function second-setter (<object>, <mutable-sequence>) => (<object>);
  function third-setter (<object>, <mutable-sequence>) => (<object>);
  function last (<sequence>, #"key", #"default") => (<object>);
  function last-setter (<object>, <mutable-sequence>) => (<object>);
  function head (<list>) => (<object>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  function tail (<list>) => (<object>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  function head-setter (<object>, <pair>) => (<object>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  function tail-setter (<object>, <pair>) => (<object>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  open generic function add (<sequence>, <object>) => (<sequence>);
  open generic function add! (<sequence>, <object>) => (<sequence>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  open generic function add-new
      (<sequence>, <object>, #"key", #"test") => (<sequence>);
  open generic function add-new!
      (<sequence>, <object>, #"key", #"test") => (<sequence>);
  open generic function remove 
      (<sequence>, <object>, #"key", #"test", #"count") => (<sequence>);
  open generic function remove!
      (<sequence>, <object>, #"key", #"test", #"count") => (<sequence>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  open generic function push (<deque>, <object>) => (<object>);
  open generic function pop (<deque>) => (<object>);
  open generic function push-last (<deque>, <object>) => (<object>);
  open generic function pop-last (<deque>) => (<object>);
  open generic function reverse (<sequence>) => (<sequence>);
  open generic function reverse! (<sequence>) => (<sequence>);
  open generic function sort 
      (<sequence>, #"key", #"test", #"stable") => (<sequence>);
  open generic function sort!
      (<sequence>, #"key", #"test", #"stable") => (<sequence>);

  /// Mapping and reducing
  function do (<function>, <collection>, #"rest") => (singleton(#f));
  function map (<function>, <collection>, #"rest") => (<collection>);
  function map-as 
      (<type>, <function>, <collection>, #"rest") => (<collection>);
  function map-into
      (<mutable-collection>, <function>, <collection>, #"rest") => (<mutable-collection>);
  function any? (<function>, <collection>, #"rest") => (<object>);
  function every? (<function>, <collection>, #"rest") => (<boolean>);
  open generic function reduce 
      (<function>, <object>, <collection>) => (<object>);
  open generic function reduce1
      (<function>, <collection>) => (<object>);
  open generic function choose
      (<function>, <sequence>) => (<sequence>);
  open generic function choose-by
      (<function>, <sequence>, <sequence>) => (<sequence>);
  open generic function member?
      (<object>, <collection>, #"key", #"test") => (<boolean>);
  open generic function find-key
      (<collection>, <function>, #"key", #"skip", #"failure") => (<object>);
  open generic function remove-key!
       (<mutable-explicit-key-collection>, <object>) => (<boolean>);
  open generic function replace-elements!
      (<mutable-collection>, <function>, <function>, #"key", #"count")
   => (<mutable-collection>);
  open generic function fill!
      (<mutable-collection>, <object>, #"key", #"start", #"end")
   => (<mutable-collection>);

  /// Iteration protocols
  open generic function forward-iteration-protocol
      (<collection>)
   => (<object>, <object>, <function>, <function>, <function>, <function>,
       <function>, <function>);
  open generic function backward-iteration-protocol
      (<collection>)
   => (<object>, <object>, <function>, <function>, <function>, <function>,
       <function>, <function>);
  open generic function table-protocol
      (<table>) => (<function>, <function>);
  function merge-hash-ids
      (<integer>, <integer>, #"key", #"ordered")
   => (<integer>);
  function object-hash (<object>, <hash-state>) => (<integer>, <hash-state>);

  /// Set operations
  open generic function intersection
      (<sequence>, <sequence>, #"key", #"test") => (<sequence>);
  open generic function union
      (<sequence>, <sequence>, #"key", #"test") => (<sequence>);
  open generic function remove-duplicates
      (<sequence>, #"key", #"test") => (<sequence>);
  open generic function remove-duplicates!
      (<sequence>, #"key", #"test") => (<sequence>);
  open generic function copy-sequence
      (<sequence>, #"key", #"start", #"end") => (<sequence>);
  function concatenate (<sequence>, #"rest") => (<sequence>);
  function concatenate-as (<type>, <sequence>, #"rest") => (<sequence>);
  open generic function replace-subsequence!
      (<sequence>, <sequence>, #"key", #"start", #"end") => (<sequence>);
  open generic function subsequence-position
      (<sequence>, <sequence>, #"key", #"test", #"count") => (false-or(<integer>));
end dylan-collections-specification-suite;

define interface-specification-suite dylan-conditions-specification-suite ()
  /// Conditions
  open abstract class <condition> (<object>);
  open abstract class <error> (<serious-condition>);
  sealed class <sealed-object-error> (<error>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  open abstract class <serious-condition> (<condition>);
  sealed instantiable class <simple-error> (<error>);
  sealed instantiable class <simple-warning> (<warning>);
  sealed instantiable class <type-error> (<error>);
  open abstract class <warning> (<condition>);

  /// Restarts
  sealed instantiable class <abort> (<restart>);
  open abstract class <restart> (<condition>);
  sealed instantiable class <simple-restart> (<restart>);

  /// Condition functions
  function signal 
      (type-union(<condition>, <string>), #"rest") => (#"rest"),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  function error
      (type-union(<condition>, <string>), #"rest") => (),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  function cerror
      (<string>, type-union(<condition>, <string>), #"rest") => (singleton(#f)),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  function break
      (type-union(<condition>, <string>), #"rest") => (singleton(#f));
  function check-type (<object>, <type>) => (<object>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  function abort () => (),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  open generic function default-handler (<condition>) => (#"rest");
  open generic function restart-query (<restart>) => (#"rest"),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  open generic function return-query (<condition>) => (#"rest"),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  function do-handlers (<function>) => (singleton(#f)),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  open generic function return-allowed? (<condition>) => (<boolean>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  open generic function return-description
      (<condition>) => (type-union(singleton(#f), <string>, <restart>)),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  function condition-format-string
      (type-union(<simple-error>, <simple-warning>, <simple-restart>)) => (<string>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  function condition-format-arguments
      (type-union(<simple-error>, <simple-warning>, <simple-restart>)) => (<sequence>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  function type-error-value (<type-error>) => (<object>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  function type-error-expected-type (<type-error>) => (<type>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
end dylan-conditions-specification-suite;

//--- Bindings not defined by the DRM
//---*** Are there any others?

define test test-define-function ()
  //---*** Fill this in...
end test;

define interface-specification-suite dylan-extensions-specification-suite ()
  constant $minimum-integer :: <integer>;
  constant $maximum-integer :: <integer>;

  test test-define-function;
end;

define interface-specification-suite dylan-core-specification-suite ()
  /// Constants

  //--- $permanent-hash-state isn't part of our implementation
  // constant $permanent-hash-state :: <object>;

  /// The core classes
  sealed class <boolean> (<object>);
  sealed class <character> (<object>);
  sealed instantiable class <class> (<type>);
  open abstract class <object> ();
  sealed instantiable class <singleton> (<type>);
  sealed class <symbol> (<object>);
  sealed abstract class <type> (<object>);

  /// Functions
  sealed abstract class <function> (<object>);
  sealed instantiable class <generic-function> (<function>);
  sealed class <method> (<function>);

  /// Constructing and initializing instances
  open generic function make 
      (<type>, #"rest", #"key", #"all-keys") => (<object>);
  open generic function initialize 
      (<object>, #"key", #"all-keys") => (#"rest");
  open generic function slot-initialized?
      (<object>, <generic-function>) => (<boolean>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  function list 
      (#"rest") => (<list>);
  function pair
      (<object>, <object>) => (<pair>);
  function range
      (#"key", #"from", #"to", #"above", #"below", #"by", #"size") => (<range>);
  function singleton
      (<object>) => (<singleton>);
  function limited
      (<class>, #"key", #"all-keys") => (<type>);
  function type-union
      (<type>, #"rest") => (<type>);
  function vector
      (#"rest") => (<simple-object-vector>);

  /// Equality and comparison functions
  function \~ (<object>) => (<boolean>);
  function \== (<object>, <object>) => (<boolean>);
  function \~== (<object>, <object>) => (<boolean>);
  open generic function \= (<object>, <object>) => (<boolean>);
  function \~= (<object>, <object>) => (<boolean>);
  open generic function \< (<object>, <object>) => (<boolean>);
  function \> (<object>, <object>) => (<boolean>);
  function \<= (<object>, <object>) => (<boolean>);
  function \>= (<object>, <object>) => (<boolean>);
  function min (<object>, #"rest") => (<object>);
  function max (<object>, #"rest") => (<object>);

  /// Coercing and copying functions
  function identity (<object>) => (<object>);
  function values (#"rest") => (#"rest");
  open generic function as (<type>, <object>) => (<object>);
  open generic function as-uppercase (<object>) => (<object>);
  open generic function as-uppercase! (<object>) => (<object>);
  open generic function as-lowercase (<object>) => (<object>);
  open generic function as-lowercase! (<object>) => (<object>);
  open generic function shallow-copy (<object>) => (#"rest");
  open generic function type-for-copy (<object>) => (<type>);

  /// Type functions
  function instance? (<object>, <type>) => (<boolean>);
  function subtype? (<type>, <type>) => (<boolean>);
  function object-class (<object>) => (<class>);
  function all-superclasses (<class>) => (<sequence>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  function direct-superclasses (<class>) => (<sequence>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  function direct-subclasses (<class>) => (<sequence>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";

  /// Functional operations
  function compose (<function>, #"rest") => (<function>);
  function complement (<function>) => (<function>);
  function disjoin (<function>, #"rest") => (<function>);
  function conjoin (<function>, #"rest") => (<function>);
  function curry (<function>, #"rest") => (<function>);
  function rcurry (<function>, #"rest") => (<function>);
  function always (<object>) => (<function>);

  /// Function handling functions
  function apply (<function>, <object>, #"rest") => (#"rest");
  function generic-function-methods (<generic-function>) => (<sequence>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  function add-method
      (<generic-function>, <method>) => (<method>, false-or(<method>));
  function generic-function-mandatory-keywords
      (<generic-function>) => (false-or(<collection>));
  function function-specializers (<function>) => (<sequence>),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  function function-arguments 
      (<function>)
   => (<integer>, <boolean>, type-union(one-of(#f, #"all"), <collection>)),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  function function-return-values
      (<function>)
   => (<sequence>, <type>, type-union(singleton(#t), <type>)),
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";
  function applicable-method?
      (<function>, #"rest") => (<boolean>);
  function sorted-applicable-methods
      (<generic-function>, #"rest")
   => (<sequence>, <sequence>);
  function find-method
      (<generic-function>, <sequence>)
   => (false-or(<method>));
  function remove-method
      (<generic-function>, <method>)
   => (<method>);
end dylan-core-specification-suite;

define suite dylan-test-suite ()
  suite dylan-arithmetic-specification-suite;
  suite dylan-collections-specification-suite;
  suite dylan-collections-test-suite;
  suite dylan-conditions-specification-suite;
  suite dylan-constants-test-suite;
  suite dylan-control-test-suite;
  suite dylan-core-specification-suite;
  suite dylan-core-test-suite;
  suite dylan-extensions-specification-suite;
  suite dylan-linearization-test-suite;
  suite dylan-macros-test-suite;
  suite dylan-numerics-test-suite;
  suite dylan-regressions-test-suite;
end suite;
