************************
The Machine Words Module
************************

Introduction
============

This chapter describes the Open Dylan implementation of machine
words. It describes a number of extensions to the Dylan language, which
are available from the Dylan library.

Throughout this chapter, arguments are instances of the class specified
by the argument name, unless otherwise noted. Thus, the arguments
*machine-word* and *integer* are instances of ``<machine-word>`` and
``<integer>``, respectively.

The class ``<machine-word>`` is a sealed subclass of ``<object>``, defined
in the Dylan library. The class ``<machine-word>`` represents a limited
range of integral values. The representation used has the natural size
suggested by the implementation architecture. (On the PC, a
``<machine-word>`` is 32 bits wide.) The class ``<machine-word>`` is
disjoint from all other classes specified by the Dylan language.

The ``\\==`` function compares instances of ``<machine-word>`` by value.

Useful functions from the Dylan module
======================================

This section describes additional methods defined in the Dylan module
that pertain to ``<machine-word>``. Note that this section only describes
extensions to the Dylan library; for complete descriptions, you should
also refer to the *Dylan Reference Manual*.

Note that the Common Dylan library also has these extensions because it
uses the Dylan library.

odd?
----

Sealed Method

.. code-block:: dylan

    odd? (m :: <machine-word>) => _ :: <boolean>

even?
-----

Sealed Method

.. code-block:: dylan

    even? (m :: <machine-word>) => _ :: <boolean>

zero?
-----

Sealed Method

.. code-block:: dylan

    zero? (m :: <machine-word>) => _ :: <boolean>

Cannot be used as the name of a result. It is not a valid Dylan name.

positive?
---------

Sealed Method

.. code-block:: dylan

    positive? (m :: <machine-word>) => _ :: <boolean>

negative?
---------

Sealed Method

.. code-block:: dylan

    negative? (m :: <machine-word>) => _ :: <boolean>

These functions return a result based on interpreting *m* as a signed
integer value.

\=
--

Sealed Method

.. code-block:: dylan

    = (m1 :: <machine-word>, m2 :: <machine-word>) => _ :: <boolean>
    = (i1 :: <abstract-integer>, m2 :: <machine-word>) => _ :: <boolean>
    = (m1 :: <machine-word>, i2 :: <abstract-integer>) => _ :: <boolean>

The comparison is performed with the ``<machine-word>`` arguments
interpreted as signed integer values.

<
-

Sealed Method

.. code-block:: dylan

    < (m1 :: <machine-word>, m2 :: <machine-word>) => _ :: <boolean>
    < (i1 :: <abstract-integer>, m2 :: <machine-word>) => _ :: <boolean>
    < (m1 :: <machine-word>, i2 :: <abstract-integer>) => _ :: <boolean>

The comparison is performed with the ``<machine-word>`` arguments
interpreted as signed integer values.

as
--

Sealed Method

.. code-block:: dylan

    as(t == <integer>, m :: <machine-word>) => _ :: <integer>

The result is an ``<integer>`` with the same value as ``m`` when interpreted
as a signed integer value. An error is signaled if the value of ``m``
cannot be represented as an instance of ``<integer>``.

as
--

Sealed Method

.. code-block:: dylan

    as(t == <abstract-integer>, m :: <machine-word>) => _ :: <abstract-integer>

The result is an ``<abstract-integer>`` with the same value as ``m`` when
interpreted as a signed integer value.

(The uses for an instance of ``<abstract-integer>`` that is not also an
instance of ``<integer>`` are rather limited without the Generic-Arithmetic
library.)

as
--

Sealed Method

.. code-block:: dylan

    as(t == <machine-word>, i :: <abstract-integer>) => _ :: <machine-word>

If the value of *i* is outside the machine word range, then the result
consists of the low ``$machine-word-size`` bits of the twos-complement
representation of *i*. If any of the discarded bits differ from the
sign of *i*, then an error is signaled.

limited
-------

Sealed Method

.. code-block:: dylan

    limited(t == <machine-word>,
            #key signed? :: boolean,
            min :: <machine-word>, max :: <machine-word>)
      => _ :: <type>

If the *signed?* argument is true (the default) then the *min* and *max*
arguments are interpreted as signed values. When *signed?* is false, the
*min* and *max* arguments are interpreted as unsigned values. The
default value for each of min and max depends on the value of *signed?*.
The defaults are taken from the corresponding minimum and maximum
machine word values (see `$maximum-signed-machine-word`_ and related
constants below).

For convenience, the values of *min* and/or *max* may also be instances
of ``<abstract-integer>``, in which case they are coerced to instances of
``<machine-word>`` as if by using *as*.

The MACHINE-WORDS module
========================

This section contains a reference entry for each item exported from the
Machine-Words module, which is exported by the Common Dylan library.

<machine-word>
--------------

Sealed Class

Summary

The class of objects that can represent a limited range of integral
values.

Superclasses

``<object>``

Init-keywords

None.

Library

*dylan*

Module

*machine-word*

Description

The class ``<machine-word>`` represents a limited range of integral
values. The representation used has the natural size suggested by the
implementation architecture. The class ``<machine-word>`` is disjoint from
all other classes specified by the Dylan language.

Operations

The ``<machine-words>`` class provides the operations described below and
in `Useful functions from the Dylan module`_.

Variables
---------

The following variables are exported from the Machine-Words module.

$machine-word-size
^^^^^^^^^^^^^^^^^^

Constant

.. code-block:: dylan

    $machine-word-size :: <integer>

The number of bits in the representation of a ``<machine-word>``.

$maximum-signed-machine-word
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Constant

.. code-block:: dylan

    $maximum-signed-machine-word :: <machine-word>

The largest machine word, when interpreted as a signed integer value.

$minimum-signed-machine-word
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Constant

.. code-block:: dylan

    $minimum-signed-machine-word :: <machine-word>

The smallest machine word, when interpreted as a signed integer value.

$maximum-unsigned-machine-word
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Constant

.. code-block:: dylan

    $maximum-unsigned-machine-word :: <machine-word>

The largest machine word, when interpreted as an unsigned integer value.

$minimum-unsigned-machine-word
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Constant

.. code-block:: dylan

    $minimum-unsigned-machine-word :: <machine-word>

The smallest machine word, when interpreted as an unsigned integer
value.

as-unsigned
^^^^^^^^^^^

Function

.. code-block:: dylan

    as-unsigned (t :: *type*, m :: <machine-word>) => *result*

The value of *m* is interpreted as an unsigned value and converted to an
instance of ``<abstract-integer>``, then the result of that conversion is
converted to type *t* using *as*.

Basic and signed single word operations
---------------------------------------

For all of the following functions, all arguments that are specified as
being specialized to ``<machine-word>`` accept an instance of
``<abstract-integer>``, which is then coerced to a ``<machine-word>``
before performing the operation.

%logior
^^^^^^^

Function

.. code-block:: dylan

    %logior (*#rest* *machine-words*) => (r :: <machine-word>)

%logxor
^^^^^^^

Function

.. code-block:: dylan

    %logxor (*#rest* *machine-words*) => (r :: <machine-word>)

%logand
^^^^^^^

Function

.. code-block:: dylan

    %logand (*#rest* *machine-words*) => (r :: m*achine-word*)

%lognot
^^^^^^^

Function

.. code-block:: dylan

    %lognot (m :: <machine-word>) => (r :: <machine-word>)

These four functions have the same semantics as *logior*, *logxor*,
*logand*, and *lognot* in the Dylan library, but they operate on
``<machine-word>`` s instead of ``<integer>`` s.

%logbit?
^^^^^^^^

Function

.. code-block:: dylan

    %logbit? (index :: <integer>, m :: <machine-word>) => (set? ::
      <boolean>)

Returns true iff the indexed bit (zero based, counting from the least
significant bit) of *m* is set. An error is signaled unless *0 <= index
< $machine-word-size*.

%count-low-zeros
^^^^^^^^^^^^^^^^

Function

.. code-block:: dylan

    %count-low-zeros (m :: <machine-word>) => (c :: <integer>)

Returns the number of consecutive zero bits in *m* counting from the
least significant bit.

.. note:: This is the position of the least significant non-zero bit in
   *m*. So if *i* is the result, then *%logbit?(i, m)* is true, and for
   all values of *j* such that *0 <= j < i*, *%logbit?(j, m)* is false.

%count-high-zeros
^^^^^^^^^^^^^^^^^

Function

.. code-block:: dylan

    %count-high-zeros (m :: <machine-word>) => (c :: <integer>)

Returns the number of consecutive zero bits in *m* counting from the
most significant bit.

.. note:: The position of the most significant non-zero bit in *m* can be
   computed by subtracting this result from *$machine-word-size - 1*. So
   if *i* is the result and *p = ($machine-word-size - i - 1)*, then
   *%logbit?(p, m)* is true, and for all values of *j* such that *p < j <
   $machine-word-size*, *%logbit?(j, m)* is false.

%+
^^

Function

.. code-block:: dylan

    %+ (m1 :: <machine-word>, m2 :: <machine-word>) => (sum ::
    <machine-word>, overflow? :: <boolean>)

Signed addition.

%-
^^

Function

.. code-block:: dylan

    %- (m1 :: <machine-word>, m2 :: <machine-word>) => (difference ::
    <machine-word>, overflow? :: <boolean>)

Signed subtraction.

%\*
^^^

Function

.. code-block:: dylan

    %\* (m1 :: <machine-word>, m2 :: <machine-word>) => (low ::
      <machine-word>, high :: <machine-word>, overflow? :: <boolean>)

Signed multiplication. The value of *overflow?* is false iff the *high*
word result is a sign extension of the *low* word result.

%floor/
^^^^^^^

Function

.. code-block:: dylan

    %floor/ (dividend :: <machine-word>, divisor :: <machine-word>) =>
      (quotient :: <machine-word>, remainder :: <machine-word>)

%ceiling/
^^^^^^^^^

Function

.. code-block:: dylan

    %ceiling/ (dividend :: <machine-word>, divisor :: <machine-word>) =>
      quotient :: <machine-word>, remainder :: <machine-word>

%round/
^^^^^^^

Function

.. code-block:: dylan

    %round/ (dividend :: <machine-word>, divisor :: <machine-word>)=>
      (quotient :: <machine-word>, remainder :: <machine-word>)

%truncate/
^^^^^^^^^^

Function

.. code-block:: dylan

    %truncate/ (dividend :: <machine-word>, divisor :: <machine-word>) =>
      (quotient :: <machine-word>, remainder :: <machine-word>)

%divide
^^^^^^^

Function

.. code-block:: dylan

    %divide (dividend :: <machine-word>, divisor :: <machine-word>) =>
      (quotient :: <machine-word>, remainder :: <machine-word>)

The functions *%divide*, *%floor/*, *%ceiling/*, *%round/*, and
*%truncate/* all perform signed division of the dividend by the divisor,
returning a quotient and remainder such that:

.. code-block:: dylan

    (quotient * divisor + remainder = dividend)

When the division is inexact (in other words, when the remainder is not
zero), the kind of rounding depends on the operation:

-  *`%floor/`_* The quotient is rounded toward
   negative infinity.
-  *`%ceiling/`_* The quotient is rounded toward
   positive infinity.
-  *`%round/`_* The quotient is rounded toward
   the nearest integer. If the mathematical quotient is exactly halfway
   between two integers, then the resulting quotient is rounded to the
   nearest even integer.
-  *`%truncate/`_* The quotient is rounded toward
   zero.
-  *`%divide`_* If both operands are
   non-negative, then the quotient is rounded toward zero. If either
   operand is negative, then the direction of rounding is unspecified,
   as is the sign of the remainder.

For all of these functions, an error is signaled if the value of the
divisor is zero or if the correct value for the quotient exceeds the
machine word range.

%negative
^^^^^^^^^

Function

.. code-block:: dylan

    %negative (m :: <machine-word>) => (r :: <machine-word>, overflow? ::
      <boolean>)

%abs
^^^^

Function

.. code-block:: dylan

    %abs (m :: <machine-word>) => (r :: <machine-word>, overflow? ::
      <boolean>)

%shift-left
^^^^^^^^^^^

Function

.. code-block:: dylan

    %shift-left (m :: <machine-word>, count :: <integer>) => (low ::
      <machine-word>, high :: <machine-word>, overflow? :: <boolean>)

Arithmetic left shift of *m* by count. An error is signaled unless *0 <=
count < $machine-word-size*. The value of *overflow?* is false iff the
high word result is a sign extension of the low word result.

%shift-right
^^^^^^^^^^^^

Function

.. code-block:: dylan

    %shift-right (m :: <machine-word>, count :: <integer>) => (r ::
      <machine-word>)

Arithmetic right shift of *m* by *count*. An error is signaled unless
*0 <= count < $machine-word-size*.

Overflow signalling operations
------------------------------

For all of the following functions, all arguments that are specified as
being specialized to ``<machine-word>`` accept an instance of
``<abstract-integer>``, which is then coerced to a ``<machine-word>``
before performing the operation.

so%+
^^^^

Function

.. code-block:: dylan

    so%+ (m1 :: <machine-word>, m2 :: <machine-word>) => (sum ::
      <machine-word>)

Signed addition. An error is signaled on overflow.

so%-
^^^^

Function

.. code-block:: dylan

    so%- (m1 :: <machine-word>, m2 :: <machine-word>) => (difference ::
      <machine-word>)

Signed subtraction. An error is signaled on overflow.

so%\*
^^^^^

Function

.. code-block:: dylan

    so%\* (m1 :: <machine-word>, m2 :: <machine-word>) => (product ::
      <machine-word>)

Signed multiplication. An error is signaled on overflow.

so%negative
^^^^^^^^^^^

Function

.. code-block:: dylan

    so%negative (m :: <machine-word>) => (r :: <machine-word>)

Negation. An error is signaled on overflow.

so%abs
^^^^^^

Function

.. code-block:: dylan

    so%abs (m :: <machine-word>) => (r :: <machine-word>)

Absolute value. An error is signaled on overflow.

so%shift-left
^^^^^^^^^^^^^

Function

.. code-block:: dylan

    so%shift-left (m :: <machine-word>, count :: <integer>) => (r ::
      <machine-word>)

Arithmetic left shift of *m* by *count*. An error is signaled unless *0
<= count < $machine-word-size*. An error is signaled on overflow.

Signed double word operations
-----------------------------

For all of the following functions, all arguments that are specified as
being specialized to ``<machine-word>`` accept an instance of
``<abstract-integer>``, which is then coerced to a ``<machine-word>``
before performing the operation.

d%floor/
^^^^^^^^

Function

.. code-block:: dylan

    d%floor/ (dividend-low :: <machine-word>, dividend-high ::
      <machine-word>, divisor :: <machine-word>) => (quotient ::
      <machine-word>, remainder :: <machine-word>)

d%ceiling/
^^^^^^^^^^

Function

.. code-block:: dylan

    d%ceiling/ (dividend-low :: <machine-word>, dividend-high ::
      <machine-word>, divisor :: <machine-word>) => (quotient ::
      <machine-word>, remainder :: <machine-word>)

d%round/
^^^^^^^^

Function

.. code-block:: dylan

    d%round/ (dividend-low :: <machine-word>, dividend-high ::
      <machine-word>, divisor :: <machine-word>) => (quotient ::
      <machine-word>, remainder :: <machine-word>)

d%truncate/
^^^^^^^^^^^

Function

.. code-block:: dylan

    d%truncate/ (dividend-low :: <machine-word>, dividend-high ::
      <machine-word>, divisor :: <machine-word>) => (quotient ::
      <machine-word>, remainder :: <machine-word>)

d%divide
^^^^^^^^

Function

.. code-block:: dylan

    d%divide (dividend-low :: <machine-word>, dividend-high ::
      <machine-word>, divisor :: <machine-word>) => (quotient ::
      <machine-word>, remainder :: <machine-word>)

The functions *d%divide*, *d%floor/*, *d%ceiling/*, *d%round/*, and
*d%truncate/* all perform signed division of the double word dividend by
the divisor, returning a quotient and remainder such that

.. code-block:: dylan

    (quotient * divisor + remainder = dividend)

When the division is inexact (in other words, when the remainder is not
zero), the kind of rounding depends on the operation:

-  *`d%floor/`_* The quotient is rounded toward
   negative infinity.
-  *`d%ceiling/`_* The quotient is rounded toward
   positive infinity.
-  *`d%round/`_* The quotient is rounded toward
   the nearest integer. If the mathematical quotient is exactly halfway
   between two integers then the resulting quotient is rounded to the
   nearest even integer.
-  *`d%truncate/`_* The quotient is rounded
   toward zero.
-  *`d%divide`_* If both operands are
   non-negative, then the quotient is rounded toward zero. If either
   operand is negative, then the direction of rounding is unspecified,
   as is the sign of the remainder.

For all of these functions, an error is signaled if the value of the
divisor is zero or if the correct value for the quotient exceeds the
machine word range.

Unsigned single word operations
-------------------------------

For all of the following functions, all arguments that are specified as
being specialized to ``<machine-word>`` accept an instance of
``<abstract-integer>``, which is then coerced to a ``<machine-word>``
before performing the operation.

u%+
^^^

Function

.. code-block:: dylan

    u%+ (m1 :: <machine-word>, m2 :: <machine-word>) => (sum ::
      <machine-word>, carry :: <machine-word>)

Unsigned addition. The value represented by *carry* is either 0 or 1.

u%-
^^^

Function

.. code-block:: dylan

    u%- (m1 :: <machine-word>, m2 :: <machine-word>) => (difference ::
      <machine-word>, borrow :: <machine-word>)

Unsigned subtraction. The value represented by *borrow* is either 0 or
1.

u%\*
^^^^

Function

.. code-block:: dylan

    u%\* (m1 :: <machine-word>, m2 :: <machine-word>) => (low ::
      <machine-word>, high :: <machine-word>)

Unsigned multiplication.

u%divide
^^^^^^^^

Function

.. code-block:: dylan

    u%divide (dividend :: <machine-word>, divisor :: <machine-word>) =>
      (quotient :: <machine-word>, remainder :: <machine-word>)

Performs unsigned division of the dividend by the divisor, returning a
quotient and remainder such that

.. code-block:: dylan

    (quotient * divisor + remainder = dividend)

An error is signaled if the value of the *divisor* is zero.

u%rotate-left
^^^^^^^^^^^^^

Function

.. code-block:: dylan

    u%rotate-left (m :: <machine-word>, count :: <integer>) => (r ::
      <machine-word>)

Logical left rotation of *m* by *count*. An error is signaled unless *0
<= count < $machine-word-size*.

u%rotate-right
^^^^^^^^^^^^^^

Function

.. code-block:: dylan

    u%rotate-right (m :: <machine-word>, count :: <integer>) => (r ::
      <machine-word>)

Logical right rotation of *m* by *count*. An error is signaled unless
*0 <= count < $machine-word-size*.

u%shift-left
^^^^^^^^^^^^

Function

.. code-block:: dylan

    u%shift-left (m :: <machine-word>, count :: <integer>) => (r ::
      <machine-word>)

Logical left shift of *m* by *count*. An error is signaled unless *0 <=
count < $machine-word-size*.

u%shift-right
^^^^^^^^^^^^^

Function

.. code-block:: dylan

    u%shift-right (m :: <machine-word>, count :: <integer>) => (r ::
      <machine-word>)

Logical right shift of *m* by *count*. An error is signaled unless *0
<= count < $machine-word-size*.

u%<
^^^

Function

.. code-block:: dylan

    u%< (m1 :: <machine-word>, m2 :: <machine-word>) => (smaller? ::
      <boolean>)

Unsigned comparison.

Unsigned double word operations
-------------------------------

For all of the following functions, all arguments that are specified as
being specialized to ``<machine-word>`` accept an instance of
``<abstract-integer>``, which is then coerced to a ``<machine-word>``
before performing the operation.

ud%divide
^^^^^^^^^

Function

.. code-block:: dylan

    ud%divide (dividend-low :: <machine-word>, dividend-high ::
      <machine-word>, divisor :: <machine-word>) => (quotient ::
      <machine-word>, remainder :: <machine-word>)

Performs unsigned division of the double word dividend by the *divisor*,
returning a *quotient* and *remainder* such that

.. code-block:: dylan

    (quotient * divisor + remainder = dividend)

An error is signaled if the value of the *divisor* is zero or if the
correct value for the *quotient* exceeds the machine word range.

ud%shift-left
^^^^^^^^^^^^^

Function

.. code-block:: dylan

    ud%shift-left (low :: <machine-word>, high :: <machine-word>, count ::
      <integer>) => (low :: <machine-word>, high :: <machine-word>)

Logical left shift by *count* of the double word value represented by
*low* and *high*. An error is signaled unless *0 <= count <
$machine-word-size*.

ud%shift-right
^^^^^^^^^^^^^^

Function

.. code-block:: dylan

    ud%shift-right (low :: <machine-word>, high :: <machine-word>, count
      :: <integer>) => (low :: <machine-word>, high :: <machine-word>)

Logical right shift by *count* of the double word value represented by
*low* and *high*. An error is signaled unless *0 <= count <
$machine-word-size*.
