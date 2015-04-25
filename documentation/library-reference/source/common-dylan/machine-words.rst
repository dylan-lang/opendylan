************************
The machine-words Module
************************

.. current-library:: common-dylan
.. current-module:: machine-words

Introduction
============

This chapter describes the Open Dylan implementation of machine
words. It describes a number of extensions to the Dylan language, which
are available from the Dylan library.

Throughout this chapter, arguments are instances of the class specified
by the argument name, unless otherwise noted. Thus, the arguments
*machine-word* and *integer* are instances of ``<machine-word>`` and
:drm:`<integer>`, respectively.

The class ``<machine-word>`` is a sealed subclass of :drm:`<object>`, defined
in the Dylan library. The class ``<machine-word>`` represents a limited
range of integral values. The representation used has the natural size
suggested by the implementation architecture. (When running a 32 bit OS,
a ``<machine-word>`` is 32 bits wide. When running a 64 bit OS, then
``<machine-word>`` is 64 bits wide.) The class ``<machine-word>`` is
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

.. function:: odd?

   :signature: odd? m => r

   :parameter m: An instance of :class:`<machine-word>`
   :value r: An instance of :class:`<boolean>`

.. function:: even?

   :signature: even? m => r

   :parameter m: An instance of :class:`<machine-word>`
   :value r: An instance of :class:`<boolean>`

.. function:: zero?

   :signature: zero? m => r

   :parameter m: An instance of :class:`<machine-word>`
   :value r: An instance of :class:`<boolean>`

.. note:: Cannot be used as the name of a result. It is not a valid Dylan name.

.. function:: positive?

   :signature: positive? m => r

   :parameter m: An instance of :class:`<machine-word>`
   :value r: An instance of :class:`<boolean>`

.. function:: negative?

   :signature: negative? m => r

   :parameter m: An instance of :class:`<machine-word>`
   :value r: An instance of :class:`<boolean>`

    negative? (m :: <machine-word>) => _ :: <boolean>

These functions return a result based on interpreting ``m`` as a signed
integer value.

.. function:: \=

   :signature: = m1 m2 => r
   :signature: = i1 m2 => r
   :signature: = m1 i2 => r

   :parameter m1: An instance of :class:`<machine-word>`
   :parameter m2: An instance of :class:`<machine-word>`
   :parameter i1: An instance of :class:`<abstract-integer>`
   :parameter i2: An instance of :class:`<abstract-integer>`
   :value r: An instance of :class:`<boolean>`

   :description:

     The comparison is performed with the :class:`<machine-word>` arguments
     interpreted as signed integer values.

.. function:: <

   :signature: < m1 m2 => r
   :signature: < i1 m2 => r
   :signature: < m1 i2 => r

   :parameter m1: An instance of :class:`<machine-word>`
   :parameter m2: An instance of :class:`<machine-word>`
   :parameter i1: An instance of :class:`<abstract-integer>`
   :parameter i2: An instance of :class:`<abstract-integer>`
   :value r: An instance of :class:`<boolean>`

   :description:

     The comparison is performed with the :class:`<machine-word>` arguments
     interpreted as signed integer values.

.. function:: as

   :signature: as t == <integer> m => r

   :parameter m: An instance of :class:`<machine-word>`
   :value r: An instance of :class:`<integer>`

   :description:

     The result is an :class:`<integer>` with the same value as ``m`` when
     interpreted as a signed integer value. An error is signaled if the value
     of ``m`` cannot be represented as an instance of :class:`<integer>`.

.. function:: as

   :signature: as t == <abstract-integer> m => r

   :parameter m: An instance of :class:`<machine-word>`
   :value r: An instance of :class:`<abstract-integer>`

   :description:

     The result is an :class:`<abstract-integer>` with the same value as ``m``
     when interpreted as a signed integer value.

     (The uses for an instance of :class:`<abstract-integer>` that is not also
     an instance of :class:`<integer>` are rather limited without the
     Generic-Arithmetic library.)

.. function:: as

   :signature: as t == <machine-word> i => r

   :parameter i: An instance of :class:`<abstract-integer>`
   :value r: An instance of :class:`<machine-word>`

   :description:

     If the value of ``i`` is outside the machine word range, then the result
     consists of the low :const:`$machine-word-size` bits of the twos-complement
     representation of ``i``. If any of the discarded bits differ from the
     sign of ``i``, then an error is signaled.

.. function:: limited

   :signature: limited t == <machine-word> #key signed? min max => r

   :parameter #key signed?: An instance of :class:`<boolean>`. Defaults to
                            ``#t``
   :parameter #key min: An instance of :class:`<machine-word>`
   :parameter #key max: An instance of :class:`<machine-word>`
   :value r: An instance of :class:`<type>`

   :description:

     If the ``signed?`` argument is true (the default) then the ``min`` and
     ``max`` arguments are interpreted as signed values. When ``signed?`` is
     false, the ``min`` and ``max`` arguments are interpreted as unsigned
     values. The default value for each of min and max depends on the value of
     ``signed?``.  The defaults are taken from the corresponding minimum and
     maximum machine word values (see :const:`$maximum-signed-machine-word` and
     related constants below).

     For convenience, the values of ``min`` and/or ``max`` may also be
     instances of ``<abstract-integer>``, in which case they are coerced to
     instances of ``<machine-word>`` as if by using *as*.

The MACHINE-WORDS module
========================

This section contains a reference entry for each item exported from the
Machine-Words module, which is exported by the Common Dylan library.

.. class:: <machine-word>
   :sealed:

   :summary:
     The class of objects that can represent a limited range of integral
     values.

   :supers: :drm:`<object>`

   :description:

     The class :class:`<machine-word>` represents a limited range of integral
     values. The representation used has the natural size suggested by the
     implementation architecture. The class :class:`<machine-word>` is
     disjoint from all other classes specified by the Dylan language.
     

   :operations:

     The :class:`<machine-word>` class provides the operations described below
     and in `Useful functions from the Dylan module`_.

Variables
---------

The following variables are exported from the Machine-Words module.

.. constant:: $machine-word-size

   :type: :class:`<integer>`

   :description:

     The number of bits in the representation of a :class:`<machine-word>`.

.. constant:: $maximum-signed-machine-word

   :type: :class:`<machine-word>`

   :description:

     The largest machine word, when interpreted as a signed integer value.

.. constant:: $minimum-signed-machine-word

   :type: :class:`<machine-word>`

   :description:

     The smallest machine word, when interpreted as a signed integer value.

.. constant:: $maximum-unsigned-machine-word

   :type: :class:`<machine-word>`

   :description:

     The largest machine word, when interpreted as an unsigned integer value.

.. constant:: $minimum-unsigned-machine-word

   :type: :class:`<machine-word>`

   :description:

     The smallest machine word, when interpreted as an unsigned integer
     value.

.. function:: as-unsigned

   :signature: as-unsigned t m => result

   :parameter t: A type
   :parameter m: An instance of :class:`<machine-word>`
   :value result: An istance of ``t``

   :description:

     The value of ``m`` is interpreted as an unsigned value and converted to an
     instance of :class:`<abstract-integer>`, then the result of that conversion
     is converted to type ``t`` using ``as``.

Basic and signed single word operations
---------------------------------------

For all of the following functions, all arguments that are specified as
being specialized to ``<machine-word>`` accept an instance of
``<abstract-integer>``, which is then coerced to a ``<machine-word>``
before performing the operation.

.. function:: %logior

   :signature: %logior #rest *machine-words* => r

   :parameter #rest *machine-words*: An instance of :class:`<machine-word>`
   :value r: An instance of :class:`<machine-word>`

.. function:: %logxor

   :signature: %logxor #rest *machine-words* => r

   :parameter #rest *machine-words*: An instance of :class:`<machine-word>`
   :value r: An instance of :class:`<machine-word>`

.. function:: %logand

   :signature: %logand #rest *machine-words* => r

   :parameter #rest *machine-words*: An instance of :class:`<machine-word>`
   :value r: An instance of :class:`<machine-word>`

.. function:: %lognot

   :signature: %lognot m => r

   :parameter m: An instance of :class:`<machine-word>`
   :value r: An instance of :class:`<machine-word>`

These four functions have the same semantics as *logior*, *logxor*,
*logand*, and *lognot* in the Dylan library, but they operate on
``<machine-word>`` s instead of :drm:`<integer>` s.

.. function:: %logbit?

   :signature: %logbit? index m => set?

   :parameter index: An instance of :class:`<integer>`
   :parameter m: An instance of :class:`<machine-word>`
   :value set?: An instance of :class:`<boolean>`

   :description:

     Returns true iff the indexed bit (zero based, counting from the least
     significant bit) of ``m`` is set. An error is signaled unless ``0 <= index
     < $machine-word-size``.

.. function:: %count-low-zeros

   :signature: %count-low-zeros m => c

   :parameter m: An instance of :class:`<machine-word>`
   :value c: An instance of :class:`<integer>`

   :description:

     Returns the number of consecutive zero bits in ``m`` counting from the
     least significant bit.

.. note:: This is the position of the least significant non-zero bit in
   ``m``. So if ``i`` is the result, then ``%logbit?(i, m)`` is true, and for
   all values of ``j`` such that ``0 <= j < i``, ``%logbit?(j, m)`` is false.

.. function:: %count-high-zeros

   :signature: %count-high-zeros m => c

   :parameter m: An instance of :class:`<machine-word>`
   :parameter c: An instance of :class:`<integer>`

   :description:

     Returns the number of consecutive zero bits in ``m`` counting from the
     most significant bit.

.. note:: The position of the most significant non-zero bit in ``m`` can be
   computed by subtracting this result from ``$machine-word-size - 1``. So
   if ``i`` is the result and ``p = ($machine-word-size - i - 1)``, then
   ``%logbit?(p, m)`` is true, and for all values of ``j`` such that ``p < j <
   $machine-word-size*, *%logbit?(j, m)`` is false.

.. function:: %+

   :signature: %+ m1 m2 => sum overflow?

   :parameter m1: An instance of :class:`<machine-word>`
   :parameter m2: An instance of :class:`<machine-word>`
   :value sum: An instance of :class:`<machine-word>`
   :value overflow?: An instance of :class:`<boolean>`

   :description:

     Signed addition.

.. function:: %-

   :signature: %- m1 m2 => difference overflow?

   :parameter m1: An instance of :class:`<machine-word>`
   :parameter m2: An instance of :class:`<machine-word>`
   :value difference: An instance of :class:`<machine-word>`
   :value overflow?: An instance of :class:`<boolean>`

   :description:

     Signed subtraction.

.. function:: %\*

   :signature: %\* m1 m2 => low high overflow?

   :parameter m1: An instance of :class:`<machine-word>`
   :parameter m2: An instance of :class:`<machine-word>`
   :value low: An instance of :class:`<machine-word>`
   :value high: An instance of :class:`<machine-word>`
   :value overflow?: An instance of :class:`<boolean>`

   :description:

     Signed multiplication. The value of ``overflow?`` is false iff the
     ``high`` word result is a sign extension of the ``low`` word result.

.. function:: %floor/

   :signature: %floor/ dividend divisor => quotient remainder

   :parameter dividend: An instance of :class:`<machine-word>`
   :parameter divisor: An instance of :class:`<machine-word>`
   :value quotient: An instance of :class:`<machine-word>`
   :value remainder: An instance of :class:`<machine-word>`

.. function:: %ceiling/

   :signature: %ceiling/ dividend divisor => quotient remainder

   :parameter dividend: An instance of :class:`<machine-word>`
   :parameter divisor: An instance of :class:`<machine-word>`
   :value quotient: An instance of :class:`<machine-word>`
   :value remainder: An instance of :class:`<machine-word>`

.. function:: %round/

   :signature: %round/ dividend divisor => quotient remainder

   :parameter dividend: An instance of :class:`<machine-word>`
   :parameter divisor: An instance of :class:`<machine-word>`
   :value quotient: An instance of :class:`<machine-word>`
   :value remainder: An instance of :class:`<machine-word>`

.. function:: %truncate/

   :signature: %truncate/ dividend divisor => quotient remainder

   :parameter dividend: An instance of :class:`<machine-word>`
   :parameter divisor: An instance of :class:`<machine-word>`
   :value quotient: An instance of :class:`<machine-word>`
   :value remainder: An instance of :class:`<machine-word>`

.. function:: %divide

   :signature: %divide/ dividend divisor => quotient remainder

   :parameter dividend: An instance of :class:`<machine-word>`
   :parameter divisor: An instance of :class:`<machine-word>`
   :value quotient: An instance of :class:`<machine-word>`
   :value remainder: An instance of :class:`<machine-word>`

The functions :func:`%divide`, :func:`%floor/`, :func:`%ceiling/`,
:func:`%round/`, and :func:`%truncate/` all perform signed division of the
dividend by the divisor, returning a quotient and remainder such that:

.. code-block:: dylan

    (quotient * divisor + remainder = dividend)

When the division is inexact (in other words, when the remainder is not
zero), the kind of rounding depends on the operation:

-  :func:`%floor/` The quotient is rounded toward
   negative infinity.
-  :func:`%ceiling/` The quotient is rounded toward
   positive infinity.
-  :func:`%round/` The quotient is rounded toward
   the nearest integer. If the mathematical quotient is exactly halfway
   between two integers, then the resulting quotient is rounded to the
   nearest even integer.
-  :func:`%truncate/` The quotient is rounded toward
   zero.
-  :func:`%divide` If both operands are
   non-negative, then the quotient is rounded toward zero. If either
   operand is negative, then the direction of rounding is unspecified,
   as is the sign of the remainder.

For all of these functions, an error is signaled if the value of the
divisor is zero or if the correct value for the quotient exceeds the
machine word range.

.. function:: %negative

   :signature: %negative m => r overflow?

   :parameter m: An instance of :class:`<machine-word>`
   :value r: An instance of :class:`<machine-word>`
   :value overflow?: An instance of :class:`<boolean>`

.. function:: %abs

   :signature: %abs m => r overflow?

   :parameter m: An instance of :class:`<machine-word>`
   :value r: An instance of :class:`<machine-word>`
   :value overflow?: An instance of :class:`<boolean>`

.. function:: %shift-left

   :signature: %shift-left m count => low high overflow?

   :parameter m: An instance of :class:`<machine-word>`
   :parameter count: An instance of :class:`<integer>`
   :value low: An instance of :class:`<machine-word>`
   :value high: An instance of :class:`<machine-word>`
   :value overflow?: An instance of :class:`<boolean>`

   :description:

     Arithmetic left shift of ``m`` by count. An error is signaled unless ``0
     <= count < $machine-word-size``. The value of ``overflow?`` is false iff
     the high word result is a sign extension of the low word result.

.. function:: %shift-right

   :signature: %shift-right m count => r

   :parameter m: An instance of :class:`<machine-word>`
   :parameter count: An instance of :class:`<integer>`
   :value r: An instance of :class:`<machine-word>`

   :description:

     Arithmetic right shift of ``m`` by ``count``. An error is signaled unless
     ``0 <= count < $machine-word-size``.

Overflow signalling operations
------------------------------

For all of the following functions, all arguments that are specified as
being specialized to ``<machine-word>`` accept an instance of
``<abstract-integer>``, which is then coerced to a ``<machine-word>``
before performing the operation.

.. function:: so%+

   :signature: so%+ m1 m2 => sum

   :parameter m1: An instance of :class:`<machine-word>`
   :parameter m2: An instance of :class:`<machine-word>`
   :value sum: An instance of :class:`<machine-word>`

   :description:

     Signed addition. An error is signaled on overflow.

.. function:: so%-

   :signature: so%- m1 m2 => difference

   :parameter m1: An instance of :class:`<machine-word>`
   :parameter m2: An instance of :class:`<machine-word>`
   :value difference: An instance of :class:`<machine-word>`

   :description:

     Signed subtraction. An error is signaled on overflow.

.. function:: so%\*

   :signature: so%\* m1 m2 => product

   :parameter m1: An instance of :class:`<machine-word>`
   :parameter m2: An instance of :class:`<machine-word>`
   :value product: An instance of :class:`<machine-word>`

   :description:

     Signed multiplication. An error is signaled on overflow.

.. function:: so%negative

   :signature: so%negative m => r

   :parameter m: An instance of :class:`<machine-word>`
   :value r: An instance of :class:`<machine-word>`

   :description:

      Negation. An error is signaled on overflow.

.. function:: so%abs

   :signature: so%abs m => r

   :parameter m: An instance of :class:`<machine-word>`
   :value r: An instance of :class:`<machine-word>`

   :description:

     Absolute value. An error is signaled on overflow.

.. function:: so%shift-left

   :signature: so%shift-left m count => r

   :parameter m: An instance of :class:`<machine-word>`
   :parameter count: An instance of :class:`<integer>`
   :value r: An instance of :class:`<machine-word>`

   :description:

     Arithmetic left shift of ``m`` by ``count``. An error is signaled unless
     ``0 <= count < $machine-word-size``. An error is signaled on overflow.

Signed double word operations
-----------------------------

For all of the following functions, all arguments that are specified as
being specialized to ``<machine-word>`` accept an instance of
``<abstract-integer>``, which is then coerced to a ``<machine-word>``
before performing the operation.

.. function:: d%floor/

   :signature: d%floor/ dividend-low dividend-high divisor => quotient
               remainder

   :parameter dividend-low: An instance of :class:`<machine-word>`
   :parameter dividend-high: An instance of :class:`<machine-word>`
   :parameter divisor: An instance of :class:`<machine-word>`
   :value quotient: An instance of :class:`<machine-word>`
   :value remainder: An instance of :class:`<machine-word>`

.. function:: d%ceiling/

   :signature: d%ceiling/ dividend-low dividend-high divisor => quotient
               remainder

   :parameter dividend-low: An instance of :class:`<machine-word>`
   :parameter dividend-high: An instance of :class:`<machine-word>`
   :parameter divisor: An instance of :class:`<machine-word>`
   :value quotient: An instance of :class:`<machine-word>`
   :value remainder: An instance of :class:`<machine-word>`

.. function:: d%round/

   :signature: d%round/ dividend-low dividend-high divisor => quotient
               remainder

   :parameter dividend-low: An instance of :class:`<machine-word>`
   :parameter dividend-high: An instance of :class:`<machine-word>`
   :parameter divisor: An instance of :class:`<machine-word>`
   :value quotient: An instance of :class:`<machine-word>`
   :value remainder: An instance of :class:`<machine-word>`

.. function:: d%truncate/

   :signature: d%truncate/ dividend-low dividend-high divisor => quotient
               remainder

   :parameter dividend-low: An instance of :class:`<machine-word>`
   :parameter dividend-high: An instance of :class:`<machine-word>`
   :parameter divisor: An instance of :class:`<machine-word>`
   :value quotient: An instance of :class:`<machine-word>`
   :value remainder: An instance of :class:`<machine-word>`

.. function:: d%divide

   :signature: d%divide dividend-low dividend-high divisor => quotient
               remainder

   :parameter dividend-low: An instance of :class:`<machine-word>`
   :parameter dividend-high: An instance of :class:`<machine-word>`
   :parameter divisor: An instance of :class:`<machine-word>`
   :value quotient: An instance of :class:`<machine-word>`
   :value remainder: An instance of :class:`<machine-word>`

The functions :func:`d%divide`, :func:`d%floor/`, :func:`d%ceiling/`,
:func:`d%round/`, and :func:`d%truncate/` all perform signed division of the
double word dividend by the divisor, returning a quotient and remainder such
that

.. code-block:: dylan

    (quotient * divisor + remainder = dividend)

When the division is inexact (in other words, when the remainder is not
zero), the kind of rounding depends on the operation:

-  :func:`d%floor/` The quotient is rounded toward
   negative infinity.
-  :func:`d%ceiling/` The quotient is rounded toward
   positive infinity.
-  :func:`d%round/` The quotient is rounded toward
   the nearest integer. If the mathematical quotient is exactly halfway
   between two integers then the resulting quotient is rounded to the
   nearest even integer.
-  :func:`d%truncate/` The quotient is rounded
   toward zero.
-  :func:`d%divide` If both operands are
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

.. function:: u%+

   :signature: u%+ m1 m2 => sum carry

   :parameter m1: An instance of :class:`<machine-word>`
   :parameter m2: An instance of :class:`<machine-word>`
   :value sum: An instance of :class:`<machine-word>`
   :value carry: An instance of :class:`<machine-word>`

   :description:

     Unsigned addition. The value represented by ``carry`` is either 0 or 1.

.. function:: u%-

   :signature: u%- m1 m2 => sum borrow

   :parameter m1: An instance of :class:`<machine-word>`
   :parameter m2: An instance of :class:`<machine-word>`
   :value sum: An instance of :class:`<machine-word>`
   :value borrow: An instance of :class:`<machine-word>`

   :description:

     Unsigned subtraction. The value represented by ``borrow`` is either 0 or
     1.

.. function:: u%\*

   :signature: u%\* m1 m2 => low high

   :parameter m1: An instance of :class:`<machine-word>`
   :parameter m2: An instance of :class:`<machine-word>`
   :value low: An instance of :class:`<machine-word>`
   :value high: An instance of :class:`<machine-word>`

   :description:

     Unsigned multiplication.

.. function:: u%divide

   :signature: u%divide dividend divisor => quotient remainder

   :parameter dividend: An instance of :class:`<machine-word>`
   :parameter divisor: An instance of :class:`<machine-word>`
   :value quotient: An instance of :class:`<machine-word>`
   :value remainder: An instance of :class:`<machine-word>`

   :description:

     Performs unsigned division of the dividend by the divisor, returning a
     quotient and remainder such that

     .. code-block:: dylan

         (quotient * divisor + remainder = dividend)

     An error is signaled if the value of the ``divisor`` is zero.

.. function:: u%rotate-left

   :signature: u%rotate-left m count => r

   :parameter m: An instance of :class:`<machine-word>`
   :parameter count: An instance of :class:`<integer>`
   :value r: An instance of :class:`<machine-word>`

   :description:

     Logical left rotation of ``m`` by ``count``. An error is signaled unless
     ``0 <= count < $machine-word-size``.

.. function:: u%rotate-right

   :signature: u%rotate-right m count => r

   :parameter m: An instance of :class:`<machine-word>`
   :parameter count: An instance of :class:`<integer>`
   :value r: An instance of :class:`<machine-word>`

   :description:

     Logical right rotation of ``m`` by ``count``. An error is signaled unless
     ``0 <= count < $machine-word-size``.

.. function:: u%shift-left

   :signature: u%shift-left m count => r

   :parameter m: An instance of :class:`<machine-word>`
   :parameter count: An instance of :class:`<integer>`
   :value r: An instance of :class:`<machine-word>`

   :description:

     Logical left shift of ``m`` by ``count``. An error is signaled unless ``0
     <= count < $machine-word-size``.

.. function:: u%shift-right

   :signature: u%shift-right m count => r

   :parameter m: An instance of :class:`<machine-word>`
   :parameter count: An instance of :class:`<integer>`
   :value r: An instance of :class:`<machine-word>`

   :description:

     Logical right shift of ``m`` by ``count``. An error is signaled unless ``0
     <= count < $machine-word-size``.

.. function:: u%<

   :signature: u%< m1 m2 => smaller?

   :parameter m1: An instance of :class:`<machine-word>`
   :parameter m2: An instance of :class:`<machine-word>`
   :value smaller?: An instance of :class:`<boolean>`

   :description:

     Unsigned comparison.

Unsigned double word operations
-------------------------------

For all of the following functions, all arguments that are specified as
being specialized to ``<machine-word>`` accept an instance of
``<abstract-integer>``, which is then coerced to a ``<machine-word>``
before performing the operation.

.. function:: ud%divide

   :signature: ud%divide dividend-low dividend-high divisor => quotient remainder

   :parameter dividend-low: An instance of :class:`<machine-word>`
   :parameter dividend-high: An instance of :class:`<machine-word>`
   :parameter divisor: An instance of :class:`<machine-word>`
   :value quotient: An instance of :class:`<machine-word>`
   :value remainder: An instance of :class:`<machine-word>`

   :description:

     Performs unsigned division of the double word dividend by the ``divisor``,
     returning a ``quotient`` and ``remainder`` such that

     .. code-block:: dylan

         (quotient * divisor + remainder = dividend)

     An error is signaled if the value of the ``divisor`` is zero or if the
     correct value for the ``quotient`` exceeds the machine word range.

.. function:: ud%shift-left

   :signature: ud%shift-left low high count => low high

   :parameter low: An instance of :class:`<machine-word>`
   :parameter high: An instance of :class:`<machine-word>`
   :parameter count: An instance of :class:`<integer>`
   :value low: An instance of :class:`<machine-word>`
   :value high: An instance of :class:`<machine-word>`

   :description:

     Logical left shift by ``count`` of the double word value represented by
     ``low`` and ``high``. An error is signaled unless ``0 <= count <
     $machine-word-size``.

.. function:: ud%shift-right

   :signature: ud%shift-right low high count => low high

   :parameter low: An instance of :class:`<machine-word>`
   :parameter high: An instance of :class:`<machine-word>`
   :parameter count: An instance of :class:`<integer>`
   :value low: An instance of :class:`<machine-word>`
   :value high: An instance of :class:`<machine-word>`

   :description:

     Logical right shift by ``count`` of the double word value represented by
     ``low`` and ``high``. An error is signaled unless ``0 <= count <
     $machine-word-size``.
