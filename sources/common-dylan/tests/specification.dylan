Module:       common-dylan-test-suite
Synopsis:     Common Dylan library test suite
Author:       Andy Armstrong, Eric Kidd
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library-spec common-dylan ()
  module common-extensions;
  module streams-protocol;
  module locators-protocol;
  module finalization;
  module simple-io;
  module simple-random;
  module simple-profiling;
  module transcendentals;
  module byte-vector;
  module machine-words;
  suite common-dylan-regressions;
  suite threads-test-suite;             //---*** NOTE: Should be changed to module test
  suite test-stream-suite;
end library-spec common-dylan;

define module-spec common-extensions ()
  
  // Numerics
  function integer-length (<integer>) => (<integer>);

  // Unsupplied, unfound
  constant $unsupplied :: <object>;
  constant $unfound    :: <object>;
  
  // Collections
  sealed instantiable class <object-deque> (<deque>);
  open abstract class <stretchy-sequence> (<stretchy-collection>, <sequence>);
  sealed instantiable class <stretchy-object-vector> (<stretchy-vector>);
  open generic-function concatenate! (<sequence>, #"rest") => (<sequence>);
  function position
      (<sequence>, <object>, #"key", #"test", #"count") => (<integer>);
  function split
      (<string>, <character>, #"key", #"start", #"end", #"trim?") => (<sequence>);
  open generic-function remove-all-keys! (<mutable-collection>) => ();
  open generic-function difference
      (<sequence>, <sequence>, #"key", #"test") => (<sequence>);
  function fill-table! (<table>, <sequence>) => (<table>);
  open generic-function find-element
      (<collection>, <function>, #"key", #"skip", #"failure") => (<object>);
  macro-test table-definer-test;

  // Conditions
  open abstract class <format-string-condition> (<condition>);
  function condition-to-string (<condition>) => (<string>);
  open abstract class <arithmetic-error> (<error>);
  sealed instantiable class <division-by-zero-error> (<arithmetic-error>);
  sealed instantiable class <arithmetic-overflow-error> (<arithmetic-error>);
  sealed instantiable class <arithmetic-underflow-error> (<arithmetic-error>);
  
  // Assertions & Debugging
  function debug-message (<string>, #"rest") => ();
  macro-test assert-test;
  macro-test debug-assert-test;

  // Types
  function false-or (<type>, #"rest") => (<type>);
  function one-of (<type>, #"rest") => (<type>);
  function subclass (<class>) => (<type>);
  
  // Ignoring
  function ignorable (<object>) => ();
  function ignore (<object>) => ();
  
  // Formatting
  function float-to-string (<float>) => (<string>);
  function integer-to-string (<integer>, #"key", #"base") => (<string>);
  function number-to-string (<number>) => (<string>);
  function string-to-integer
      (<string>, #"key", #"base", #"start", #"end")
   => (<integer>, <integer>);
  function format-to-string (<string>, #"rest") => (<string>);
  
  // Control constructs
  macro-test iterate-test; // the macro iterate
  macro-test when-test;    // the macro when (which we're discussing)

  // Application basics
  function application-name () => (<string>);
  function application-filename () => (false-or(<string>));
  function application-arguments () => (<sequence>);
  function exit-application (<integer>) => ();
  function register-application-exit-function (<function>) => ();
end module-spec common-extensions;

define module-spec streams-protocol ()
  abstract class <stream> (<object>);
  variable *standard-input* :: <stream>;
  variable *standard-output* :: <stream>;
  variable *standard-error* :: <stream>;
  variable *debug-output* :: <stream>;

  // Conditions
  open abstract class <stream-error> (<error>);
  function stream-error-stream (<stream-error>) => (<stream>);
  sealed instantiable class <end-of-stream-error> (<stream-error>);
  sealed instantiable class <incomplete-read-error> (<end-of-stream-error>);
  sealed instantiable class <incomplete-write-error> (<end-of-stream-error>);
  //---*** What should these two functions return?
  function stream-error-sequence (<stream-error>) => (<object>);
  function stream-error-count (<stream-error>) => (<object>);

  // Reading from streams
  open generic-function read-element (<stream>, #"key", #"on-end-of-stream")
    => (<object>);
  open generic-function unread-element (<stream>, <object>)
    => (<object>);
  open generic-function read (<stream>, <integer>, #"key", #"on-end-of-stream")
    => (<object>);
  open generic-function read-into! (<stream>, <integer>, <mutable-sequence>,
				    #"key", #"start", #"on-end-of-stream")
    => (<object>);
  open generic-function peek (<stream>, #"key", #"on-end-of-stream")
    => (<object>);
  open generic-function discard-input (<stream>) => ();
  open generic-function stream-input-available? (<stream>) => (<boolean>);
  open generic-function stream-contents (<stream>, #"key", #"clear-contents?") => (<sequence>);
  open generic-function stream-contents-as
      (<type>, <stream>, #"key", #"clear-contents?")
   => (<sequence>);

  // Writing to streams
  open generic-function write (<stream>, <sequence>, #"key", #"start", #"end")
    => ();
  open generic-function write-element (<stream>, <object>) => ();
  open generic-function force-output (<stream>) => ();
  open generic-function synchronize-output (<stream>) => ();
  open generic-function discard-output (<stream>) => ();

  // Querying streams
  open generic-function stream-open? (<stream>) => (<boolean>);
  open generic-function stream-element-type (<stream>) => (<type>);
  open generic-function stream-at-end? (<stream>) => (<boolean>);
  open generic-function stream-size (<stream>) => (false-or(<integer>));

  // Positioning streams
  open abstract class <positionable-stream> (<stream>);
  open generic-function stream-position (<positionable-stream>)
   => (<object>);
  open generic-function stream-position-setter
      (<object>, <positionable-stream>)
   => (<object>);
  open generic-function adjust-stream-position
      (<positionable-stream>, <integer>, #"key", #"from")
   => (<object>);
end module-spec streams-protocol;

define module-spec locators-protocol ()
  // This may be merged into any other module.
  open abstract instantiable class <locator> (<object>);
  open generic-function supports-open-locator? (<locator>) => (<boolean>);
  open generic-function open-locator (<locator>) => (<stream>);
  open generic-function supports-list-locator? (<locator>) => (<boolean>);
  open generic-function list-locator (<locator>) => (<sequence>);
end module-spec locators-protocol;

define module-spec finalization ()
  // As per Functional Objects documentation.
end module-spec finalization;
  
define module-spec simple-io ()
  // Exports some or all of streams protocol?
  function format-out (<string>, #"rest") => ();
  // ... under construction
end module-spec simple-io;
  
define module-spec simple-random ()
  sealed instantiable class <random> (<object>);
  function random (<integer>, #"key", #"random") => (<integer>);
end module-spec simple-random;

define module-spec simple-profiling ()
  sealed instantiable class <profiling-state> (<table>);
  open generic-function start-profiling-type
    (<profiling-state>, <symbol>) => ();
  open generic-function stop-profiling-type
    (<profiling-state>, <symbol>) => ();
  open generic-function profiling-type-result
    (<profiling-state>, <symbol>, #"key", #"all-keys")
 => (<object>);
  macro-test profiling-test;
  // ... anything else?
end module-spec simple-profiling;

define module-spec byte-vector ()
  sealed instantiable class <byte-vector> (<vector>);
  open generic function copy-bytes
    (<sequence>, <integer>, <sequence>, <integer>, <integer>)
 => ();
  function byte-vector-fill
    (<byte-vector>, <integer>, #"key", #"start", #"end")
 => ();
  function byte-vector-ref
    (<byte-vector>, <integer>) => (<integer>);
  function byte-vector-ref-setter
    (<integer>, <byte-vector>, <integer>) => (<integer>);
end module-spec byte-vector;

define module-spec machine-words ()
  sealed instantiable class <machine-word> (<object>);
  constant $machine-word-size :: <integer>;
  constant $maximum-signed-machine-word :: <machine-word>;
  constant $minimum-signed-machine-word :: <machine-word>;
  constant $maximum-unsigned-machine-word :: <machine-word>;
  constant $minimum-unsigned-machine-word :: <machine-word>;
  function as-unsigned (<type>, <machine-word>) => (<object>);
end module-spec machine-words;

define module-spec transcendentals ()
  /// Constants
  constant $single-pi :: <single-float>;
  constant $double-pi :: <double-float>;
  constant $single-e :: <single-float>;
  constant $double-e :: <double-float>;

  /// The core classes
  /// Functions

  open generic-function sin(<real>) => (<float>);
  open generic-function cos(<real>) => (<float>);
  open generic-function tan(<real>) => (<float>);

// Returns the sine, cosine, or tangent of _x_, respectively.  _x_ is
// given in radians.

// The floating point precision of the result is given by the precision
// of _x_.  The result will be a <single-float> if _x_ is an integer.

  open generic-function asin(<real>) => (<float>); // -1 <= y <= +1
  open generic-function acos(<real>) => (<float>); // -1 <= y <= +1

  open generic-function atan(<real>) => (<float>); // -1 <= y <= +1
  open generic-function atan2(<real>, <real>) => (<float>); // -1 <= y <= +1

// Returns the arc sine or arc cosine of _y_, in radians.  If _y_ is not
// in the range [-1, +1], an error is signalled.

// The floating point precision of the result is given by the precision
// of _y_.  The result will be a <single-float> if _y_ is an integer.

  open generic-function sinh (<real>) => (<float>);
  open generic-function cosh (<real>) => (<float>);
  open generic-function tanh (<real>) => (<float>);

// Returns the hyperbolic sine, hyperbolic cosine, or hyperbolic tangent
// of _x_, respectively.

// The floating point precision of the result is given by the precision
// of _x_.  The result will be a <single-float> if _x_ is an integer.

  open generic-function asinh (<real>) => (<float>);
  open generic-function acosh (<real>) => (<float>);
  open generic-function atanh (<real>) => (<float>);

// Returns the hyperbolic arc sine, hyperbolic arc cosine, or hyperbolic
// arc tangent of _y_, respectively.

// The floating point precision of the result is given by the precision
// of _y_.  The result will be a <single-float> if _y_ is an integer.


  open generic-function log (<real>) => (<float>); //  x > 0

// Returns the natural logarithm of _x_ in base _e_.  If _x_ <= 0, an
// error is signalled.

// The floating point precision of the result is given by the precision
// of _x_.  The result will be a <single-float> if _x_ is an integer.

  open generic-function exp (<real>) => (<float>);

// Returns e raised to the power _x_.

// The floating point precision of the result is given by the precision
// of _x_.


  open generic-function logn (<real>, <real>) => (<float>);
  // x > 0, b > 1

// Returns the logarithm of _x_ in base _b_.  If _b_ <= 1 or _x_ <= 0, an
// error is signalled.

// The floating point precision of the result is given by the precision
// of _x_.  The result will be a <single-float> if _x_ is an integer.


  open generic-function \^ (<real>, <real>) => (<float>);

// Returns _b_ raised to the power _x_.  If _b_ is 0 and _x_ is not
// positive, an error is signalled.  If _b_ is negative and _x_ is not
// an integer, an error is signalled.

// The floating point precision of the result is given by the precision
// of _b_.  The result will be a <single-float> if _b_ is an integer.


//  open 
//    generic-function \^ (b :: <integer>, x :: <integer>) => (y :: <integer>);

// Returns an integer result giving _b_ raised to the power _x_.  If _b_
// is 0 and _x_ is not positive, an error is signalled.  If _x_ is
// negative, an error is signalled.


  open generic-function sqrt (<real>) => (<float>); // x >= 0

// Returns the square root of _x_.  If _x_ < 0, an error is signalled.

// The floating point precision of the result is given by the precision
// of _x_.  The result will be a <single-float> if _x_ is an integer.


  open generic-function isqrt (<integer>) => (<integer>); // x >= 0

  /// Constructing and initializing instances
  /// Equality and comparison functions
  /// Coercing and copying functions
  /// Type functions

  /// Other protocols
//  protocol transcendentals;
end module-spec transcendentals;
