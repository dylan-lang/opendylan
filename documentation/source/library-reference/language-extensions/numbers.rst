********
Integers
********

.. current-library:: dylan
.. current-module:: dylan

.. TODO:
   * This should be combined into language-extensions/
   * Note that content below includes things completely unrelated to numbers.

This document describes the Open Dylan implementation of arithmetic
functions, especially integer arithmetic. It describes a number of
extensions to the Dylan language, which are available from the Dylan
library. It also describes a generic arithmetic facility that, through
the use of other libraries, allows you to extend arithmetic to special
number types, such as "big" (double machine word) integers.

Throughout this document, arguments are instances of the class specified
by the argument name (ignoring any numeric suffixes), unless otherwise
noted. Thus, the arguments *integer*, *integer1*, and *integer2* would
all be instances of the class :drm:`<integer>`.

The goals of the extensions to the Dylan language described in this
document are as follows:

* Provide arithmetic operations that are closed over small integers.

  This allows type inference to propagate small integer declarations more
  widely, because there is no possibility of automatic coercion into some
  more general format.

* Make the arithmetic operations that are closed over small integers
  easily accessible to programmers.

* Allow the Dylan library to be described in such a way that only small
  integers are present by default, moving support for infinite precision
  integer arithmetic to the Big-Integers library, which must be explicitly
  used.

* Support infinite precision integer arithmetic through the Big-Integers
  library.

  .. note:: Using that library in another library does not have a negative
     effect on the correctness or performance of other libraries in the same
     application that do not use it.

* Maintain compatibility with the DRM specification.

  In particular, the extensions support the production of efficient code
  for programs written to be portable with respect to the DRM
  specification. Use of implementation-specific types or operations in
  order to get reasonable efficiency is not required. This precludes
  relegating the :drm:`<integer>` class and *limited-<integer>* types to
  inefficient implementations.

  .. note:: When there are several distinct interfaces with the same name
     but in different modules, the notation *interface* *#* *module* is used
     in this document to remove ambiguity.

* Specify that the class :drm:`<integer>` has a finite,
  implementation-dependent range, bounded by the constants
  ``$minimum-integer`` and ``$maximum-integer``.

  The representation for integers must be at least 28 bits, including the
  sign. That is, the minimum conforming value for ``$maximum-integer`` is ``2 ^
  27 - 1`` and the maximum conforming value for ``$minimum-integer`` is ``-2 ^
  27``.

  .. note:: *Rationale:* Restricting :drm:`<integer>` in this way allows the programmer
     to stay in the efficient range without requiring exact knowledge of what
     that range might be. The full generality of extended precision integers
     is provided by the Big-Integers library, for programmers who actually
     need that functionality.

* Define the type ``<machine-number>`` to be the type union of :drm:`<float>` and
  :drm:`<integer>`.

The Dylan library provides implementations of the generic functions and
functions described in this document. If the result of one of these
operations is specified to be an instance of :drm:`<integer>` and the
mathematically correct result cannot be represented as an :drm:`<integer>`
then an error is signaled. This removes fully generic arithmetic from
the Dylan library. In particular, it removes extended integers, ratios,
and rectangular complex numbers.

Extensions to the dylan Library
===============================

This section describes the extensions to the Dylan library that provide
the arithmetic operations available as standard to your applications.
You do not have to explicitly use any additional libraries to have
access to any of the functionality described in this section. Note that
this section only describes extensions to the Dylan library; for
complete descriptions, you should also refer to the *Dylan Reference
Manual*.

Note that the Common-Dylan library also has these extensions because it
uses the Dylan library.

Ranges
------

The initialization arguments for :drm:`<range>` must all be instances of
``<machine-number>`` rather than :drm:`<real>`.

Specific constructors
---------------------

The following specific constructors are available for use with the class
:drm:`<integer>`.

.. generic-function:: limited

   Defines a new type that represents a subset of the class :drm:`<integer>`.

   :signature: limited *integer-class* #key *min* *max* => *limited-type*

   :parameter integer-class: The singleton(<integer>).
   :parameter min: The lower bound of the range. The default is
     ``$minimum-integer``.
   :parameter max: The upper bound of the range. The default is ``$maximum-integer``.

   :description:

     The *integer-class* argument is the class :drm:`<integer>`, and all other
     arguments are instances of :drm:`<integer>`. The range of :drm:`<integer>` is
     bounded by default.

.. function:: range

   This function is used to specify ranges of numbers.

   :signature: range (#key from:, to:, above:, below:, by:, size:) => <range>

   :description:

      All of the supplied arguments must be instances of ``<machine-number>``.

Equality comparisons
--------------------

The :drm:`=` function compares two objects and returns :drm:`#t` if the values of
the two objects are equal to each other, that is of the same magnitude.

.. generic-function:: =
   :open:

   :signature: = *object1* *object2* => *boolean*

   :description:

      Tests its arguments to see if they are of the same magnitude.

.. method:: =
   :sealed:
   :specializer: <complex>

   Tests its arguments to see if they are of the same magnitude.

   :signature: = *complex1* *complex2* => *boolean*

.. method:: =
   :specializer: <machine-number>

   Tests its arguments to see if they are of the same magnitude.

   :signature: = *machine-number1* *machine-number2* => *boolean*

Magnitude comparisons
---------------------

The Dylan library provides the following interfaces for testing the
magnitude of two numbers:

.. generic-function:: <
   :open:

   Returns :drm:`#t` if its first argument is less than its second argument.

   :signature: < *object1* *object2* => *boolean*

.. method:: <
   :sealed:
   :specializer: <complex>

   Returns :drm:`#t` if its first argument is less than its second argument.

   :signature: < *complex1* *complex2* => *boolean*

.. method:: <
   :specializer: <machine-number>

   Returns :drm:`#t` if its first argument is less than its second argument.

   :signature: < *machine-number1* *machine-number2* => *boolean*

Properties of numbers
---------------------

Various number properties can be tested using the following predicates
in the Dylan library:

.. generic-function:: odd?
   :open:

   Tests whether the argument supplied represents an odd value.

   :signature: odd? *object* => *boolean*

.. method:: odd?
   :sealed:
   :specializer: <complex>

   :signature: odd? *complex* => *boolean*

   :description:

      Tests whether the argument supplied represents an odd value.

.. method:: odd?
   :specializer: <integer>

   Tests whether the argument supplied represents an odd value.

   :signature: odd? *integer* => *boolean*

.. generic-function:: even?
   :open:

   Tests whether the argument supplied represents an even value

   :signature: even? *object* => *boolean*

.. method:: even?
   :sealed:
   :specializer: <complex>

   Tests whether the argument supplied represents an even value

   :signature: even? *complex* => *boolean*

.. method:: even?
   :specializer: <integer>

   Tests whether the argument supplied represents an even value

   :signature: even? *integer* => *boolean*

.. generic-function:: zero?
   :open:

   Tests whether the argument supplied represents a zero value.

   :signature: zero? *object* => *boolean*

.. method:: zero?
   :specializer: <complex>
   :sealed:

   Tests whether the argument supplied represents a zero value.

   :signature: zero? *complex* => *boolean*

.. method:: zero?
   :specializer: <machine-number>

   Tests whether the argument supplied represents a zero value.

   :signature: zero? *machine-number* => *boolean*

.. generic-function:: positive?
   :open:

   Tests whether the argument supplied represents a positive value.

.. method:: positive?
   :sealed:
   :specializer: <complex>

   :signature: positive? *complex*

   :description:

      Tests whether the argument supplied represents a positive value.

.. method:: positive?
   :specializer: <machine-number>

   :signature: positive? *machine-number* => *boolean*

   :description:

      Tests whether the argument supplied represents a positive value.

.. generic-function:: negative?
   :open:

   Tests whether the argument supplied represents a negative value.

   :signature: negative? *object* => *boolean*

.. method:: negative?
   :sealed:
   :specializer: <complex>

   Tests whether the argument supplied represents a negative value.

   :signature: negative? *complex* => *boolean*

.. method:: negative?
   :specializer: <machine-number>

   Tests whether the argument supplied represents a negative value.

   :signature: negative? *machine-number* => *boolean*

.. generic-function:: integral?
   :open:

   Tests whether the argument supplied represents an integral value.

   :signature: integral? *object* => *boolean*

.. method:: integral?
   :sealed:
   :specializer: <complex>

   Tests whether the argument supplied represents an integral value.

   :signature: integral? *complex*

.. method:: integral?
   :specializer: <machine-number>

   Tests whether the argument supplied represents an integral value.

   :signature: integral? *machine-number* => *boolean*

Arithmetic operations
---------------------

The following arithmetic operations are available in the Dylan library:

.. generic-function:: +
   :open:

   Returns the sum of the two supplied arguments. The actual type of
   the value is determined by the contagion rules when applied to the
   arguments.

   :signature: + *object1* *object2* => #rest *object*

.. method:: +
   :sealed:
   :specializer: <complex>, <complex>

   :signature: + *complex1* *complex2*

.. method:: +
   :specializer: <integer>, <complex>

   :signature: + *integer1* *integer2* => *integer*

.. method:: +
   :specializer: <machine-number>, <machine-number>

   :signature: + *machine-number1* *machine-number2* => *machine-number*


.. generic-function:: -
   :open:

   Returns the result of subtracting the second argument from the
   first.  The actual type of the value is determined by the contagion
   rules when applied to the arguments.

   :signature: - *object1* *object2* => #rest *object*

.. method:: -
   :sealed:
   :specializer: <complex>, <complex>

   :signature: - *complex1 complex2*

.. method:: -
   :specializer: <integer>, <integer>

   :signature: - *integer1 integer2* => *integer*

.. method:: -
   :specializer: <machine-number>, <machine-number>

   :signature: - *machine-number1* *machine-number2* => *machine-number*

.. generic-function:: *
   :open:

   Returns the result of multiplying the two arguments. The actual
   type of the value is determined by the contagion rules when applied
   to the arguments.

   :signature: * *object1* *object2* => #rest *object*

.. method:: *
   :sealed:
   :specializer: <complex>, <complex>

   :signature: \* *complex1* *complex2*

.. method:: *
   :specializer: <integer>, <integer>

   :signature: \* *integer1* *integer2* => *integer*

.. method:: *
   :specializer: <machine-number>, <machine-number>

   :signature: \* *machine-number1* *machine-number2* => *machine-number*

.. generic-function:: /
   :open:

   Returns the result of dividing the first argument by the
   second. The actual type of the value is determined by the contagion
   rules when applied to the arguments.

   :signature: / *object1* *object2* => #rest *object*

.. method:: /
   :sealed:
   :specializer: <complex>, <complex>

   :signature: / *complex1* *complex2*

.. method:: /
   :specializer: <float>, <float>

   :signature: / *float1* *float2* => *float*

.. generic-function:: negative
   :open:

   Negates the supplied argument. The returned value is of the same float
   format as the supplied argument.

   :signature: negative *object* => #rest *negative-object*

.. method:: negative
   :sealed:
   :specializer: <complex>

   :signature: negative *complex*

.. method:: negative
   :specializer: <integer>

   :signature: negative *integer* => *negative-integer*

.. method:: negative
   :specializer: <float>

   :signature: negative *float* => *negative-float*

.. generic-function:: floor

   Truncates a number toward negative infinity. The integer part is
   returned as *integer*, the remainder is of the same float format as
   the argument.

   :signature: floor *object* => *integer* *object*

.. method:: floor
   :specializer: <machine-number>

   :signature: floor *machine-number* => *integer* *machine-number*

.. method:: floor
   :specializer: <integer>

   :signature: floor *integer* => *integer* *integer*

.. method:: floor
   :specializer: <float>

   :signature: floor *float* => *integer* *float*

.. generic-function:: ceiling

   Truncates a number toward positive infinity. The integer part is
   returned as *integer*, the remainder is of the same float format as
   the argument.

   :signature: ceiling *machine-number* => *integer* *machine-number*

.. method:: ceiling
   :specializer: <machine-number>

   :signature: ceiling *machine-number* => *integer* *machine-number*

.. method:: ceiling
   :specializer: <integer>

   :signature: ceiling *integer* => *integer* *integer*

.. method:: ceiling
   :specializer: <float>

   :signature: ceiling *float* => *integer* *float*

.. generic-function:: round

   Rounds a number toward the nearest mathematical integer. The
   integer part is returned as *integer*, the remainder is of the same
   float format as the argument. If the argument is exactly between
   two integers, then the result *integer* will be a multiple of two.

   :signature: round *object* => *integer* *object*

.. method:: round
   :specializer: <machine-number>

   :signature: round *machine-number* => *integer* *machine-number*

.. method:: round
   :specializer: <integer>

   :signature: round *integer* => *integer* *integer*

.. method:: round
   :specializer: <float>

   :signature: round *float* => *integer* *float*

.. generic-function:: truncate

   Truncates a number toward zero. The integer part is returned as
   *integer*, the remainder is of the same float format as the
   argument.

   :signature: truncate *machine-number* => *integer* *object*

.. method:: truncate
   :specializer: <machine-number>

   :signature: truncate *machine-number* => *integer* *machine-number*

.. method:: truncate
   :specializer: <integer>

   :signature: truncate *integer* => *integer* *integer*

.. method:: truncate
   :specializer: <float>

   :signature: truncate *float* => *integer* *float*

.. generic-function:: floor/

   Divides the first argument into the second and truncates the result
   toward negative infinity. The integer part is returned as
   *integer*, the type of the remainder is determined by the contagion
   rules when applied to the arguments.

   :signature: floor/ *object1* *object2* => *integer* *machine-number*

.. method:: floor/
   :specializer: <machine-number>, <machine-number>

   :signature: floor/ *machine-number1* *machine-number2* => *integer* *machine-number*

.. method:: floor/
   :specializer: <integer>, <integer>

   :signature: floor/ *integer1* *integer2* => *integer* *integer*


.. generic-function:: ceiling/

   Divides the first argument into the second and truncates the result
   toward positive infinity. The integer part is returned as
   *integer*, the type of the remainder is determined by the contagion
   rules when applied to the arguments.

   :signature: ceiling/ *object1* *object2* => *integer* *object*

.. method:: ceiling/
   :specializer: <machine-number>, <machine-number>

   :signature: ceiling/ *machine-number1* *machine-number2* => *integer* *machine-number*

.. method:: ceiling/
   :specializer: <integer>, <integer>

   :signature: ceiling/ *integer1* *integer2* => *integer* *integer*

.. generic-function:: round/

   Divides the first argument into the second and rounds the result
   toward the nearest mathematical integer. The integer part is
   returned as *integer*, the type of the remainder is determined by
   the contagion rules when applied to the arguments.

   :signature: round/ *object1* *object2* => *integer* *machine-number*

.. method:: round/
   :specializer: <machine-number>, <machine-number>

   :signature: round/ *machine-number1* *machine-number2* => *integer* *machine-number*

.. method:: round/
   :specializer: <integer>, <integer>

   :signature: round/ *integer1* *integer2* => *integer* *integer*


.. generic-function:: truncate/

   Divides the first argument into the second and truncates the result
   toward zero. The integer part is returned as *integer*, the type of
   the remainder is determined by the contagion rules when applied to
   the arguments.

   :signature: truncate/ *machine-number1* *machine-number2* => *integer* *machine-number*

.. method:: truncate/
   :specializer: <integer>, <integer>

   :signature: truncate/ *integer1* *integer2* => *integer* *integer*

.. generic-function:: modulo

   Returns the second value of *floor/ (* *arg1* *,* *arg2* *)*. The
   actual type of the second value is determined by the contagion
   rules when applied to the arguments.

   :signature: modulo *machine-number1* *machine-number2* => *machine-number*

.. method:: modulo
   :specializer: <machine-number>, <machine-number>

   :signature: modulo *machine-number1* *machine-number2* => *machine-number*

.. method:: modulo
   :specializer: <integer>, <integer>

   :signature:  modulo *integer1* *integer2* => *integer*

.. generic-function:: remainder

   Returns the second value of :drm:`truncate/` (* *arg1* *,* *arg2* *)*.The
   actual type of the second value is determined by the contagion
   rules when applied to the arguments.

   :signature: remainder *machine-number1* *machine-number2* => *machine-number*

.. method:: remainder
   :specializer: <integer>, <integer>

   :signature: remainder *integer1* *integer2* => *integer*

   .. generic-function:: ^
      :open:

      Returns the first argument raised to the power of the second
      argument.  The value is of the same float format as the first
      argument. An error is signalled if both arguments are 0.

      :signature: ^ *object1* *object2* => #rest *object*

   .. method:: ^
      :sealed:
      :specializer: <complex>, <complex>

      :signature: ^ *complex1* *complex2*

   .. method:: ^
      :specializer: <integer>, <integer>

      :signature: ^ *integer1* *integer2* => *integer*

   .. method:: ^
      :specializer: <float>, <integer>

      :signature: ^ *float1* *integer2* => *float*

.. generic-function:: abs
   :open:

   Returns the absolute value of the argument. The value is of the
   same float format as the argument.

   :signature: abs *object* => #rest *object*

.. method:: abs
   :sealed:
   :specializer: <complex>

   :signature: abs *complex*

.. method:: abs
   :specializer: <integer>

   :signature: abs *integer* => *integer*

.. method:: abs
   :specializer: <float>

   :signature: abs *float* => *float*

.. function:: logior

   Returns the bitwise inclusive *OR* of its integer arguments.

   :signature: logior #rest *integers* => *integer*

.. function:: logxor

   Returns the bitwise exclusive *OR* of its integer arguments.

   :signature: logxor #rest *integers* => *integer*

.. function:: logand

   Returns the bitwise *AND* of its integer arguments.

   :signature: logand #rest *integers* => *integer*

.. function:: lognot

   Returns the bitwise *NOT* of its integer arguments.

   :signature: lognot *integer1* => *integer2*

.. function:: logbit?

   Tests the value of a particular bit in its integer argument. The
   *index* argument is an instance of :drm:`<integer>`.

   :signature: logbit? *index* *integer* => *boolean*

.. function:: ash

   Performs an arithmetic shift on its first argument.

   :signature: ash *integer1* *count* => *integer*

.. function:: lcm

   Returns the least common multiple of its two arguments.

   :signature: lcm *integer1* *integer2* => *integer*

.. function:: gcd

   Returns the greatest common divisor of its two arguments.

   :signature: gcd *integer1* *integer2* => *integer*

Collections
-----------

The keys for sequences are always instances of :drm:`<integer>`. This means
that certain kinds of collections cannot be sequences; very large (or
unbounded) sparse arrays are an example.

The Table Protocol
------------------

See :doc:`language-differences` for a list of changes to the table protocol.

Iteration Constructs
--------------------

.. macro:: for
   :statement:

   The *start*, *bound*, and *increment* expressions in a numeric
   clause must evaluate to instances of ``<machine-number>`` for this
   macro.

.. _generic-arithmetic-library:

The generic-arithmetic Library
==============================

The Generic-Arithmetic library exports the functions described in this
section from a module called *generic-arithmetic*.

The Generic-Arithmetic library provides a fully extensible version of
all arithmetic operations. If an application only uses
Generic-Arithmetic, these versions of the operators reduce themselves to
be equivalent to those in the Dylan library. But when you use additional
implementation libraries, the arithmetic operators are extended.

The Big-Integers library is one such implementation library. It provides a
implementation of :drm:`<integer>` that uses two machine words to represent
each integer. For example, on a 64-bit machine architecture this is a 128-bit
signed integer.

The standard integer implementation in the Dylan library is actually
part of the following class hierarchy::

  <abstract-integer>
   ├── <integer>
   └── <big-integer>
        └── <double-integer>

(The classes ``<big-integer>`` and ``<double-integer>`` are implementation
classes. You do not need to use them.)

The modules in the Generic-Arithmetic library export ``<abstract-integer>``
with the name :drm:`<integer>`. They also export a full set of arithmetic
operators that use instances of ``<abstract-integer>`` rather than instances
of :drm:`<integer>` (in the Dylan library naming scheme). However, those
operators just fall back to the Dylan library operators until you include an
implementation library, such as Big-Integers, in your application.

When you use the Big-Integers library, the arithmetic operators exported by
Generic-Arithmetic are enhanced to extend their results to 128-bit integers on
64-bit machines or 64-bit integers on 32-bit machine architectures. If a result
is small enough to fit in a Dylan library :drm:`<integer>`, it will be fitted
into one.

Note that the Generic-Arithmetic library uses the same naming
conventions for arithmetic operators as used by the Dylan library. This
means that some renaming is required in modules that require access to
both the basic Dylan interfaces and the interfaces supplied by the
Generic-Arithmetic library. As described earlier, the notation
*interface* *#* *module* is used to denote different interfaces of the
same name, where *interface* is the name of the interface, and *module*
is the name of the module it is exported from.

See `Using special arithmetic features`_ for an example of how to use
an implementation library with Generic-Arithmetic.

Ranges
------

The Generic-Arithmetic library defines the class :class:`<range>`, which is in
most respects functionally equivalent to *<range>#Dylan*, but uses generic
arithmetic operations in its implementation so that the initialization
arguments can be instances of :drm:`<real>`, rather than being restricted to
:class:`<machine-number>`.

Classes
-------

The class ``<abstract-integer>`` is imported and re-exported under the
name *<integer>#generic-arithmetic*.

Specific constructors
---------------------

.. function:: range

   :signature: range #key *from* *to* *above* *below* *by* *size* => *range*

This function is identical to the function *range#Dylan*, except that
all of the supplied arguments must be instances of :drm:`<real>`.

Arithmetic operations
---------------------

The following functions all have Generic-Arithmetic implementations that are
mathematically equivalent to the corresponding implementations defined on
:drm:`<integer>` and documented in the DRM.  See :drm:`Arithmetic operations
<Arithmetic_Operations#HEADING-100-49>` for descriptions of each function as
implemented in the Dylan library.

:drm:`+` *object1* *object2* => #rest *object*

:drm:`-` *object1* *object2* => #rest *object*

:drm:`*` *object1* *object2* => #rest *object*

:drm:`/` *object1* *object2* => #rest *object*

:drm:`negative` *object* => #rest *negative-object*

:drm:`floor` *real1* => *abstract-integer* *real*

:drm:`ceiling` *real1* => *abstract-integer* *real*

:drm:`round` *real1* => *abstract-integer* *real*

:drm:`truncate` *real1* => *abstract-integer* *real*

:drm:`floor/` *real1* *real2* => *abstract-integer* *real*

:drm:`ceiling/` *real1* *real2* => *abstract-integer* *real*

:drm:`round/` *real1* *real2* => *abstract-integer* *real*

:drm:`truncate/` *real1* *real2* => *abstract-integer* *real*

:drm:`modulo` *real1* *real2* => *real*

:drm:`remainder` *real1* *real2* => *real*

:drm:`^` *object1* *object2* => #rest *object*

:drm:`abs` *object1* => #rest *object*

:drm:`logior` #rest *abstract-integer1* => *abstract-integer*

:drm:`logxor` #rest *abstract-integer1* => *abstract-integer*

:drm:`logand` #rest *abstract-integer1* => *abstract-integer*

:drm:`lognot` *abstract-integer1* => *abstract-integer*

:drm:`logbit?` *integer* *abstract-integer* => *boolean*

:drm:`ash` *abstract-integer1* *integer* => *abstract-integer*

:drm:`lcm` *abstract-integer1* *abstract-integer2* => *abstract-integer*

:drm:`gcd` *abstract-integer1* *abstract-integer2* => *abstract-integer*

Iteration constructs
--------------------

While a programmer could make use of generic arithmetic in a :drm:`for` loop
by using explicit-step clauses, this approach leads to a loss of
clarity. The definition of the :drm:`for` macro is complex, so a version that
uses generic arithmetic in numeric clauses is provided, rather than
requiring programmers who want that feature to reconstruct it.

.. macro:: for
   :statement:

   The *start*, *bound*, and *increment* expressions in a numeric clause
   must evaluate to instances of ``<machine-number>`` for this macro.
   Otherwise, this macro is similar to *for#Dylan*.

Exported Modules from the generic-arithmetic Library
----------------------------------------------------

The Generic-Arithmetic library exports several modules that are provided
for the convenience of programmers who wish to create additional modules
based on the *dylan* module plus various combinations of the arithmetic
models.

The dylan-excluding-arithmetic Module
-------------------------------------

The Dylan-Excluding-Arithmetic module imports and re-exports all of the
interfaces exported by the *dylan* module from the Dylan library, except
for the following excluded interfaces:

* :drm:`<integer>`
* :drm:`range`
* :drm:`+` :drm:`-` :drm:`*` :drm:`/`
* :drm:`negative`
* :drm:`floor` :drm:`ceiling` :drm:`round` :drm:`truncate`
* :drm:`floor/` :drm:`ceiling/` :drm:`round/` :drm:`truncate/`
* :drm:`modulo` :drm:`remainder`
* :drm:`^`
* :drm:`abs`
* :drm:`logior` :drm:`logxor` :drm:`logand` :drm:`lognot`
* :drm:`logbit?`
* :drm:`ash`
* :drm:`lcm` :drm:`gcd`
* :drm:`for`

The dylan-arithmetic Module
---------------------------

The Dylan-Arithmetic module imports and re-exports all of the interfaces
exported by the *dylan* module from the Dylan library which are excluded
by the *dylan-excluding-arithmetic* module.

The generic-arithmetic-dylan Module
-----------------------------------

The Generic-Arithmetic-Dylan module imports and reexports all of the
interfaces exported by the *dylan-excluding-arithmetic* module and the
*generic-arithmetic* module.

The *dylan-excluding-arithmetic*, *dylan-arithmetic*, and
*generic-arithmetic* modules provide convenient building blocks for
programmers to build the particular set of global name bindings they
wish to work with. The purpose of the *generic-arithmetic-dylan* module
is to provide a standard environment in which generic arithmetic is the
norm, for those programmers who might want that.

Using Special Arithmetic Features
=================================

As noted in `The Generic-Arithmetic library`_, the Generic-Arithmetic
library provides an extensible protocol for adding specialized arithmetic
functionality to your applications. By using the Generic-Arithmetic
library alongside a special implementation library, you can make the
standard arithmetic operations support number types such as big (double
machine word) integers, or complex numbers.

This section provides an example of extending the basic Dylan arithmetic
features using the Generic-Arithmetic library and the Big-Integers
implementation library.

To use special arithmetic features, a library's ``define library``
declaration must use at least the following libraries:

* common-dylan
* generic-arithmetic
* *special-arithmetic-implementation-library*

So for Big-Integers you would write:

.. code-block:: dylan

    define library foo
      use common-dylan;
      use generic-arithmetic;
      use big-integers;
      ...
    end library foo;

Next you have to declare a module. There are three ways of using
big-integer arithmetic that we can arrange with a suitable module
declaration:

#. Replace all integer arithmetic with the big-integer arithmetic.
#. Use both, with normal arithmetic remaining the default.
#. Use both, with the big-integer arithmetic becoming the default.

To get one of the three different effects described above, you need to
arrange the ``define module`` declaration accordingly. To replace all
integer arithmetic with big-integer arithmetic, include the following in
your ``define module`` declaration:

.. code-block:: dylan

    use generic-arithmetic-common-dylan;

(Note that the module definition should not use the Big-Integers module.
The Big-Integers library is used as a side-effects library only, that
is, it is referenced in the library definition so that it will be
loaded. Its definitions extend the Generic-Arithmetic library.)

If you replace all integer arithmetic with big-integer arithmetic in
this way, there will be performance hits. For instance, loop indices
will have to be checked at run-time to see whether a normal or big
integer representation is being used, and a choice must be made about
the representation for an incremented value.

You can take a different approach that reduces the cost of big-integer
arithmetic. Under this approach you leave normal integer arithmetic
unchanged, and get access to big-integer arithmetic when you need it. To
do this, use the same libraries but instead of using the
``common-dylan-generic-arithmetic`` module, include the following in your
``define module`` declaration:

.. code-block:: dylan

    use common-dylan;
    use generic-arithmetic, prefix: "generic/"; // use any prefix you like

This imports the big-integer arithmetic binding names, but gives them a prefix
``generic/``, using the standard renaming mechanism available in module
declarations. Thus you gain access to big arithmetic using renamed classes and
operations like:

.. code-block:: dylan

    generic/<integer>
    generic/+
    generic/-
    generic/*
    ...

The operations take either instances of :drm:`<integer>` or
``generic/<integer>`` (a subclass of :drm:`<integer>`) and return instances of
``generic/<integer>``.

Note that having imported the big-integer operations under new names,
you have to use prefix rather than infix syntax when calling them. For
example:

.. code-block:: dylan

    generic/+(5, 4);

not:

.. code-block:: dylan

    5 generic/+ 4;

The existing functions like :drm:`+` and :drm:`-` will only accept :drm:`<integer>`
instances and ``generic/<integer>`` instances small enough to be represented as
:drm:`<integer>` instances.

Under this renaming scheme, reduced performance will be confined to the
``generic/`` operations. Other operations, such as loop index increments and
decrements, will retain their efficiency.

Finally, you can make big-integer arithmetic the default but keep normal
arithmetic around for when you need it. Your ``define module``
declaration should contain:

.. code-block:: dylan

    use generic-arithmetic-common-dylan;
    use dylan-arithmetic, prefix: "dylan/"; //use any prefix you like

The big-integers Library
========================

The Big-Integers library exports a module called ``big-integers``, which
imports and re-exports all of the interfaces exported by the
``generic-arithmetic`` module of the Generic-Arithmetic library.

The Big-Integers library modifies the behavior of functions provided by
the Dylan library as described in this section.

Specific Constructors
---------------------

The Big-Integers library extends the functionality of specific
constructors in the Dylan library as follows:

.. function:: limited

   :signature: limited *abstract-integer-class* #key *min* *max* => *limited-type*

Returns a limited integer type, which is a subtype of
``<abstract-integer>``, whose instances are integers greater than or
equal to *min* (if specified) and less than or equal to *max* (if
specified). If no keyword arguments are specified, the result type is
equivalent to ``<abstract-integer>``. The argument
*abstract-integer-class* is the class ``<abstract-integer>``.

If both *min* and *max* are supplied, and both are instances of
:drm:`<integer>`, then the result type is equivalent to calling *limited* on
:drm:`<integer>` with those same bounds.

The Limited Integer Type Protocol is extended to account for limited
``<abstract-integer>`` types.

Instances and subtypes in the Big-Integers library
--------------------------------------------------

In each of the following code snippets, the expression in the first line is
true if and only if all of the expressions following it are true.

.. code:: dylan

   instance?(x, limited(<abstract-integer>, min: y, max: z))

   instance?(x, <abstract-integer>)
   y <= x
   x <= z

.. code:: dylan

   instance?(x, limited(<abstract-integer>, min: y))

   instance?(x, <abstract-integer>)
   y <= x

.. code:: dylan

   instance?(x, limited(<abstract-integer>, max: z))

   instance?(x, <abstract-integer>)
   x <= z

.. code:: dylan

   subtype?(limited(<abstract-integer>, min: w, max: x),
            limited(<abstract-integer>, min: y, max: z))

   w >= y
   x <= z

.. code:: dylan

   subtype?(limited(<abstract-integer>, min: w ...),
            limited(<abstract-integer>, min: y))

   w >= y

.. code:: dylan

   subtype?(limited(<abstract-integer>, max: x ...),
            limited(<abstract-integer>, max: z))

   x <= z

Type-equivalence in the Big-Integers library
--------------------------------------------

In each of the following code snippets, the expression on the first line is
type equivalent to the expression on the second line if and only if the text
following both expressions is true.

.. code:: dylan

   limited(<abstract-integer>, min: y, max: z)

   limited(<integer>, min: y, max: z)

*y* and *z* are both instances of :drm:`<integer>`.

.. code:: dylan

   limited(<abstract-integer>, min: y, max: $maximum-integer)

   limited(<integer>, min: y)

*y* is an instance of :drm:`<integer>`.

.. code:: dylan

   limited(<abstract-integer>, min: $minimum-integer, max: z)

   limited(<integer>, max: z)

*z* is an instance of :drm:`<integer>`.

Type disjointness is modified as follows to account for limited
``<abstract-integer>`` types.

A limited integer type is disjoint from a class if their base types are
disjoint or the class is :drm:`<integer>` and the range of the limited
integer type is disjoint from the range of :drm:`<integer>` (that is, from
*$minimum-integer* to *$maximum-integer*).

Equality comparisons
--------------------

The behavior of equality comparisons in the Dylan library is modified by
the Big-Integers library as follows::

    = *abstract-integer1* *abstract-integer2* => *boolean*
    = *abstract-integer* *float* => *boolean*
    = *float* *abstract-integer* => *boolean*

Magnitude comparisons
---------------------

The behavior of magnitude comparisons in the Dylan library is modified
by the Big-Integers library as follows::

    < *abstract-integer1* *abstract-integer2* => *boolean
    < *abstract-integer* *float* => *boolean*
    < *float* *abstract-integer* => *boolean*

Properties of Numbers
---------------------

The behavior of number property tests in the Dylan library is modified
by the Big-Integers library as follows::

    odd? *abstract-integer* => *boolean*
    even? *abstract-integer* => *boolean*
    zero? *abstract-integer* => *boolean*
    positive? *abstract-integer* => *boolean*
    negative? *abstract-integer* => *boolean*
    integral? *abstract-integer* => *boolean*

.. bigint_arithmetic_operations:

Arithmetic Operations
---------------------

The Big-Integers library modifies the behavior of the functions provided
by the Generic-Arithmetic library as described below.

The actual type of the return value for all the following interfaces is
determined by the contagion rules when applied to the arguments.

::

    + *abstract-integer1* *abstract-integer2* => *abstract-integer*
    + *abstract-integer* *float1* => *float*
    + *float1* *abstract-integer* => *float*

    - *abstract-integer1* *abstract-integer2* => *abstract-integer*
    - *abstract-integer* *float1* => *float*
    - *float1* *abstract-integer* => *float*

    * *abstract-integer1* *abstract-integer2* => *abstract-integer*
    * *abstract-integer* *float1* => *float*
    * *float1* *abstract-integer* => *float*

The return value of the following interface is of the same float format
as the argument::

    negative *abstract-integer* => *negative-abstract-integer*

The second return value of all the following interfaces is of the same
float format as the argument::

    floor *abstract-integer* => *abstract-integer1* *abstract-integer2*
    floor *float1* => *abstract-integer* *float*

    ceiling *abstract-integer* => *abstract-integer1* *abstract-integer2*
    ceiling *float1* => *abstract-integer* *float*

    round *abstract-integer* => *abstract-integer1* *abstract-integer2*
    round *float1* => *abstract-integer* *float*

    truncate *abstract-integer* => *abstract-integer1* *abstract-integer2*
    truncate *float1* => *abstract-integer* *float*

The second return value of all the following interfaces is of the same
float format as the first argument::

    floor/ *abstract-integer1* *abstract-integer2* => *abstract-integer3* *abstract-integer4*
    floor/ *float1* *abstract-integer1* => *abstract-integer2* *float2*

    ceiling/ *abstract-integer1* *abstract-integer2* => *abstract-integer3* *abstract-integer4*
    ceiling/ *float1* *abstract-integer1* => *abstract-integer2* *float2*

    round/ *abstract-integer1* *abstract-integer2* => *abstract-integer3* *abstract-integer4*
    round/ *float1* *abstract-integer1* => *abstract-integer2* *float2*

    truncate/ *abstract-integer1* *abstract-integer2* => *abstract-integer3* *abstract-integer4
    truncate/ *float1* *abstract-integer1* => *abstract-integer2* *float2*

The second return value of the following interfaces is of the same float
format as the second argument::

    floor/ *abstract-integer1* *float1* => *abstract-integer2* *float2*
    ceiling/ *abstract-integer1* *float1* => *abstract-integer2* *float2*
    round/ *abstract-integer1* *float1* => *abstract-integer2* *float2*
    truncate/ *abstract-integer1* *float1* => *abstract-integer2* *float2*

The return value of the following interfaces is of the same float format
as the first argument::

    modulo *float1* *abstract-integer* => *float*
    remainder *float1* *abstract-integer* => *float*

The return value of the following interfaces is of the same float format
as the second argument::

    modulo *abstract-integer1* *abstract-integer2* => *abstract-integer*
    modulo *abstract-integer* *float1* => *float*
    remainder *abstract-integer1* *abstract-integer2* => *abstract-integer*
    remainder *abstract-integer* *float1* => *float*

The behavior of the following miscellaneous interfaces is also modified
by the Big-Integers library::

    ^ *abstract-integer1* *integer* => *abstract-integer
    abs *abstract-integer1* => *abstract-integer*
    logior #rest *abstract-integer1* => *abstract-integer*
    logxor #rest *abstract-integer1* => *abstract-integer*
    logand #rest *abstract-integer1* => *abstract-integer*
    lognot *abstract-integer1* => *abstract-integer*
    logbit? *integer* *abstract-integer* => *boolean*
    ash *abstract-integer1* *integer* => *abstract-integer*
    lcm *abstract-integer1* *abstract-integer2* => *abstract-integer*
    gcd *abstract-integer1* *abstract-integer2* => *abstract-integer*

