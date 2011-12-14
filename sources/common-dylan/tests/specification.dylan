Module:       common-dylan-test-suite
Synopsis:     Common Dylan library test suite
Author:       Andy Armstrong, Eric Kidd
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
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
  module threads;
  suite common-dylan-regressions;
  suite threads-test-suite; //---*** NOTE: Should be changed to module test
  suite test-stream-suite;
end library-spec common-dylan;

define module-spec common-extensions ()
  // Numerics
  function integer-length (<integer>) => (<integer>);

  function decode-float (<float>) => (<float>, <integer>, <float>);
  function scale-float (<float>, <integer>) => (<float>);
  function float-radix (<float>) => (<integer>);
  function float-digits (<float>) => (<integer>);
  function float-precision (<float>) => (<integer>);
  //function integer-decode-float (<float>) => (<float>, <integer>, <float>);

  constant $single-float-epsilon :: <single-float>;
  constant $double-float-epsilon :: <double-float>;
  //constant $extended-float-epsilon :: <extended-float>;

  constant $minimum-single-float-exponent :: <integer>;
  constant $maximum-single-float-exponent :: <integer>;
  constant $minimum-double-float-exponent :: <integer>;
  constant $maximum-double-float-exponent :: <integer>;
  //constant $minimum-extended-float-exponent :: <integer>;
  //constant $maximum-extended-float-exponent :: <integer>;

  // Unsupplied, unfound
  constant $unsupplied :: <object>;
  function unsupplied () => (<object>);
  function unsupplied? (<object>) => (<boolean>);
  function supplied? (<object>) => (<boolean>);

  constant $unfound    :: <object>;
  function unfound () => (<object>);
  function unfound? (<object>) => (<boolean>);
  function found? (<object>) => (<boolean>);
  
  // Collections
  sealed instantiable class <object-deque> (<deque>);
  open abstract class <stretchy-sequence> (<stretchy-collection>, <sequence>);
  sealed instantiable class <stretchy-object-vector> (<stretchy-vector>);
  sealed instantiable class <string-table> (<table>);
  open generic-function concatenate! (<sequence>, #"rest") => (<sequence>);
  function position
      (<sequence>, <object>, #"key", #"test", #"start", #"end", #"skip")
   => (false-or(<integer>));
  open generic-function split
      (<sequence>, <object>,
       #"key", #"start", #"end", #"count", #"remove-if-empty")
   => (<sequence>);
  function join
    (<sequence>, <sequence>, #"key" #"key", #"conjunction") => (<sequence>);

  open generic-function remove-all-keys! (<mutable-collection>) => ();
  open generic-function difference
      (<sequence>, <sequence>, #"key", #"test") => (<sequence>);
  function fill-table! (<table>, <sequence>) => (<table>);
  open generic-function find-element
      (<collection>, <function>, #"key", #"skip", #"failure") => (<object>);
  macro-test table-definer-test;

  // Conditions
  open abstract class <format-string-condition> (<condition>);
  open abstract primary class <simple-condition> (<condition>);
  open generic-function condition-to-string (<condition>) => (false-or(<string>));
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
  function one-of (<object>, #"rest") => (<type>);
  function subclass (<class>) => (<type>);

  // Ignoring
  function ignorable (#"rest") => ();
  function ignore (#"rest") => ();
  
  // Formatting
  function float-to-string (<float>) => (<string>);
  function integer-to-string (<integer>, #"key", #"base") => (<string>);
  open generic-function number-to-string (<number>) => (<string>);
  function string-to-integer 
      (<string>, #"key", #"base", #"start", #"end", #"default")
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

  // Other
  function false? (<object>) => (<boolean>);
  function true? (<object>) => (<boolean>);
end module-spec common-extensions;

define module-spec streams-protocol ()
  open abstract class <stream> (<object>);

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

  // Other
  open generic-function open-file-stream
    (<object>, #"key", #"all-keys") => (<stream>);
  open generic-function wait-for-io-completion (<stream>) => ();
  open generic-function close
    (<stream>, #"rest", #"key", #"all-keys") => ();
end module-spec streams-protocol;

define module-spec locators-protocol ()
  // This may be merged into any other module.
  open abstract class <locator> (<object>);
  open generic-function supports-open-locator? (<locator>) => (<boolean>);
  open generic-function open-locator (<locator>, #"key", #"all-keys") => (<stream>);
  open generic-function supports-list-locator? (<locator>) => (<boolean>);
  open generic-function list-locator (<locator>) => (<sequence>);
end module-spec locators-protocol;

define module-spec finalization ()
  function automatic-finalization-enabled? () => (<boolean>);
  function automatic-finalization-enabled?-setter (<boolean>) => ();
  function drain-finalization-queue () => ();
  open generic-function finalize (<object>) => ();
  function finalize-when-unreachable (<object>) => (<object>);
end module-spec finalization;
  
define module-spec simple-io ()
  function format-out (<string>, #"rest") => ();
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
  constant <byte> :: <type>;
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

  // Variables
  constant $machine-word-size :: <integer>;
  constant $maximum-signed-machine-word :: <machine-word>;
  constant $minimum-signed-machine-word :: <machine-word>;
  constant $maximum-unsigned-machine-word :: <machine-word>;
  constant $minimum-unsigned-machine-word :: <machine-word>;
  function as-unsigned (<type>, <machine-word>) => (<object>);

  // Basic and signed single word operations
  function %logior (#"rest") => (<machine-word>);
  function %logxor (#"rest") => (<machine-word>);
  function %logand (#"rest") => (<machine-word>);
  function %lognot (<object>) => (<machine-word>);
  function %logbit? (<integer>, <object>) => (<boolean>);
  function %count-low-zeros (<object>) => (<integer>);
  function %count-high-zeros (<object>) => (<integer>);
  function \%+ (<object>, <object>) => (<machine-word>, <boolean>);
  function \%- (<object>, <object>) => (<machine-word>, <boolean>);
  function \%* (<object>, <object>) => (<machine-word>, <machine-word>, <boolean>);
  function %floor/ (<object>, <object>) => (<machine-word>, <machine-word>);
  function %ceiling/ (<object>, <object>) => (<machine-word>, <machine-word>);
  function %round/ (<object>, <object>) => (<machine-word>, <machine-word>);
  function %truncate/ (<object>, <object>) => (<machine-word>, <machine-word>);
  function %divide (<object>, <object>) => (<machine-word>, <machine-word>);
  function %negative (<object>) => (<machine-word>, <boolean>);
  function %abs (<object>) => (<machine-word>, <boolean>);
  function %shift-left (<object>, <integer>) => (<machine-word>, <machine-word>, <boolean>);
  function %shift-right (<object>, <integer>) => (<machine-word>);

  // Overflow signalling operations
  function so%+ (<machine-word>, <machine-word>) => (<machine-word>);
  function so%- (<machine-word>, <machine-word>) => (<machine-word>);
  function so%* (<object>, <object>) => (<machine-word>);
  function so%negative (<object>) => (<machine-word>);
  function so%abs (<object>) => (<machine-word>);
  function so%shift-left (<object>, <integer>) => (<machine-word>);

  // Signed double word operations
  function d%floor/ (<object>, <object>, <object>) => (<machine-word>, <machine-word>);
  function d%ceiling/ (<object>, <object>, <object>) => (<machine-word>, <machine-word>);
  function d%round/ (<object>, <object>, <object>) => (<machine-word>, <machine-word>);
  function d%truncate/ (<object>, <object>, <object>) => (<machine-word>, <machine-word>);
  function d%divide (<object>, <object>, <object>) => (<machine-word>, <machine-word>);

  // Unsigned single word operations
  function u%+ (<object>, <object>) => (<machine-word>, <machine-word>);
  function u%- (<object>, <object>) => (<machine-word>, <machine-word>);
  function u%* (<object>, <object>) => (<machine-word>, <machine-word>);
  function u%divide (<object>, <object>) => (<machine-word>, <machine-word>);
  function u%rotate-left (<object>, <integer>) => (<machine-word>);
  function u%rotate-right (<object>, <integer>) => (<machine-word>);
  function u%shift-left (<object>, <integer>) => (<machine-word>);
  function u%shift-right (<object>, <integer>) => (<machine-word>);
  function u%< (<object>, <object>) => (<boolean>);

  // Unsigned double word operations
  function ud%divide (<object>, <object>, <object>) => (<machine-word>, <machine-word>);
  function ud%shift-left (<object>, <object>, <integer>) => (<machine-word>, <machine-word>);
  function ud%shift-right (<object>, <object>, <integer>) => (<machine-word>, <machine-word>);
end module-spec machine-words;

define module-spec transcendentals ()
  /// Constants
  constant $single-pi :: <single-float>;
  constant $double-pi :: <double-float>;
  constant $single-e :: <single-float>;
  constant $double-e :: <double-float>;

  /// The core classes
  /// Functions

  open generic-function sin(<number>) => (<number>);
  open generic-function cos(<number>) => (<number>);
  open generic-function tan(<number>) => (<number>);

// Returns the sine, cosine, or tangent of _x_, respectively.  _x_ is
// given in radians.

// The floating point precision of the result is given by the precision
// of _x_.  The result will be a <single-float> if _x_ is an integer.

  open generic-function asin(<number>) => (<number>); // -1 <= y <= +1
  open generic-function acos(<number>) => (<number>); // -1 <= y <= +1

  open generic-function atan(<number>) => (<number>); // -1 <= y <= +1
  open generic-function atan2(<number>, <number>) => (<number>); // -1 <= y <= +1

// Returns the arc sine or arc cosine of _y_, in radians.  If _y_ is not
// in the range [-1, +1], an error is signalled.

// The floating point precision of the result is given by the precision
// of _y_.  The result will be a <single-float> if _y_ is an integer.

  open generic-function sinh (<number>) => (<number>);
  open generic-function cosh (<number>) => (<number>);
  open generic-function tanh (<number>) => (<number>);

// Returns the hyperbolic sine, hyperbolic cosine, or hyperbolic tangent
// of _x_, respectively.

// The floating point precision of the result is given by the precision
// of _x_.  The result will be a <single-float> if _x_ is an integer.

  open generic-function asinh (<number>) => (<number>);
  open generic-function acosh (<number>) => (<number>);
  open generic-function atanh (<number>) => (<number>);

// Returns the hyperbolic arc sine, hyperbolic arc cosine, or hyperbolic
// arc tangent of _y_, respectively.

// The floating point precision of the result is given by the precision
// of _y_.  The result will be a <single-float> if _y_ is an integer.


  open generic-function log (<number>) => (<number>); //  x > 0

// Returns the natural logarithm of _x_ in base _e_.  If _x_ <= 0, an
// error is signalled.

// The floating point precision of the result is given by the precision
// of _x_.  The result will be a <single-float> if _x_ is an integer.

  open generic-function exp (<number>) => (<number>);

// Returns e raised to the power _x_.

// The floating point precision of the result is given by the precision
// of _x_.

  function logn (<number>, <number>) => (<number>);
  // x > 0, b > 1

// Returns the logarithm of _x_ in base _b_.  If _b_ <= 1 or _x_ <= 0, an
// error is signalled.

// The floating point precision of the result is given by the precision
// of _x_.  The result will be a <single-float> if _x_ is an integer.


  open generic-function \^ (<number>, <number>) => (<number>);

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


  open generic-function sqrt (<number>) => (<number>); // x >= 0

// Returns the square root of _x_.  If _x_ < 0, an error is signalled.

// The floating point precision of the result is given by the precision
// of _x_.  The result will be a <single-float> if _x_ is an integer.

  function isqrt (<integer>) => (<integer>); // x >= 0

  /// Constructing and initializing instances
  /// Equality and comparison functions
  /// Coercing and copying functions
  /// Type functions

  /// Other protocols
//  protocol transcendentals;
end module-spec transcendentals;

define module-spec threads ()
  // Low-level synchronization
  function sequence-point () => ();
  function synchronize-side-effects () => ();

  // Operations on threads
  sealed instantiable class <thread> (<object>);
  constant $low-priority :: <object>;
  constant $background-priority :: <object>;
  constant $normal-priority :: <object>;
  constant $interactive-priority :: <object>;
  constant $high-priority :: <object>;
  function thread-name (<thread>) => (false-or(<string>));
  function join-thread (<thread>, #"rest") => (<thread>, #"rest");
  class <duplicate-join-error> (<thread-error>);
  function thread-yield () => ();
  function current-thread () => (<thread>);

  // Synchronization protocol
  open abstract class <synchronization> (<object>);
  open generic-function wait-for (<synchronization>, #"key", #"timeout") => (<object>);
  open generic-function release (<synchronization>) => ();
  open generic-function synchronization-name (<synchronization>) => (false-or(<string>));

  // Locks
  open abstract instantiable class <lock> (<synchronization>);
  macro-test with-lock-test;
  class <timeout-expired> (<serious-condition>);

  // Semaphores
  open abstract instantiable primary class <semaphore> (<lock>);
  constant $semaphore-maximum-count-limit :: <object>;
  class <count-exceeded-error> (<error>);

  // Exclusive locks
  open abstract instantiable class <exclusive-lock> (<lock>);
  open generic-function owned? (<exclusive-lock>) => (<boolean>);

  // Recursive locks
  open abstract instantiable primary class <recursive-lock> (<exclusive-lock>);
  
  // Simple locks
  open abstract instantiable primary class <simple-lock> (<exclusive-lock>);

  // Multiple reader / single writer locks
  open abstract instantiable primary class <read-write-lock> (<exclusive-lock>);

  // Notifications
  sealed instantiable class <notification> (<synchronization>);
  function associated-lock (<notification>) => (<simple-lock>);
  class <not-owned-error> (<error>);
  function release-all (<notification>) => ();

  // Timers
  function sleep (<real>) => ();

  // Dynamic binding
  macro-test dynamic-bind-test;

  // Conditional update
  macro-test conditional-update!-test;
  sealed instantiable class <conditional-update-error> (<error>);
  macro-test atomic-decrement!-test;
  macro-test atomic-increment!-test;
end module-spec threads;
