**************************
The transcendentals Module
**************************

.. current-library:: common-dylan
.. current-module:: transcendentals

Introduction
============

The *transcendentals* module, exported from the *common-dylan* library,
provides a set of open generic functions for ANSI C-like behavior over
real numbers. The restrictions and error cases described in this chapter
are intended to be the same as they are in ANSI C.

The single module, *transcendentals*, exports these generic functions
and methods.

Because implementation of these functions might be by a standard library
for transcendentals accessed by a foreign function interface, the exact
precision and algorithms (and hence, the exact results) for all of these
functions is explicitly unspecified. Note, however, that a program
expects the following, even in libraries that are implemented by calling
foreign libraries:

-  Domain and range errors should be signalled as Dylan errors.
-  Floating point precision *contagion* must obey Dylan rules (that is,
   functions called on single precision values return single precision
   results, and functions on double precision values return double
   precision results)

The Transcendentals module
==========================

This section contains a reference entry for each item exported from the
*common-dylan* library’s *transcendentals* module.

.. generic-function:: ^

   :summary: 
     Returns its first argument, raised to the power indicated by its second
     argument.

   :signature: ^ b x => y

   :parameter b: An instance of type :class:`<real>`.
   :parameter x: An instance of type :class:`<real>`.
   :value y: An instance of type :class:`<real>`.

   :description:

     Returns ``b`` raised to the power ``x``. If ``b`` is ``0`` and ``x`` is not
     positive, an error is signalled. If ``b`` is negative and ``x`` is not an
     integer, an error is signalled.

     If ``b`` and ``x`` are both integers, the result is an integer. If ``x`` is
     negative, an error is signalled.

     The floating point precision is given by the precision of ``b``. The
     result is a single-float if ``b`` is an integer.

   :seealso:

     - :func:`exp`

.. generic-function:: acos

   :summary:
     Returns the arc cosine of its argument.

   :signature: acos x => y

   :parameter x: an instance of type :class:`<real>`. The angle, in radians.
                 If ``x`` is not in the range ``[-1,+1]``, an error is signalled.
   :value y: An instance of type :class:`<float>`.

   :description:

     Returns the arc cosine of its argument. The floating point precision of
     the result is given by the precision of ``x``. The result is a
     single-float if ``x`` is an integer.

   :seealso:

     - :func:`asin`
     - :func:`atan`

.. generic-function:: acosh

   :summary:
     Returns the hyperbolic arc cosine of its argument.

   :signature: acosh x => y

   :parameter x: An instance of type :class:`<real>`. The angle, in radians.
   :value y: An instance of type :class:`<float>`.

   :description:

     Returns the hyperbolic arc cosine of its argument. The floating point
     precision of the result is given by the precision of ``x``. The result is
     a single-float if ``x`` is an integer.

   :seealso:

     - :func:`asinh`
     - :func:`atanh`

.. generic-function:: asin

   :summary:
     Returns the arc sine of its argument.

   :signature: asin x => y

   :parameter x: An instance of type :class:`<real>`. The angle, in radians.
                 If ``x`` is not in the range `[-1,+1]`, an error is signalled.
   :value y: An instance of type :class:`<float>`.

   :description:

     Returns the arc sine of its argument. The floating point precision of
     the result is given by the precision of ``x``. The result is a
     single-float if ``x`` is an integer.

   :seealso:

     - :func:`acos`
     - :func:`atan`

.. generic-function:: asinh

   :summary:
     Returns the hyperbolic arc sine of its argument.

   :signature: asinh x => y

   :parameter x: An instance of type :class:`<real>`. The angle, in radians.
   :value y: An instance of type :class:`<float>`.

   :description:

     Returns the hyperbolic arc sine of its argument. The floating point
     precision of the result is given by the precision of ``x``. The result is
     a single-float if ``x`` is an integer.

   :seealso:

     - :func:`acosh`
     - :func:`atanh`

.. generic-function:: atan

   :summary:
     Returns the arc tangent of its argument.

   :signature: atan x => y

   :parameter x: An instance of type :class:`<real>`. The angle, in radians.
                 If ``x`` is not in the range `[-1,+1]`, an error is signalled.
   :value y: An instance of type :class:`<float>`.

   :description:

     Returns the arc tangent of its argument. The floating point precision of
     the result is given by the precision of *x*. The result is a
     single-float if *x* is an integer.

   :seealso:

     - :func:`acos`
     - :func:`asin`

.. generic-function:: atan2

   :summary:
     Returns the arc tangent of one angle divided by another.

   :signature: atan2 x y => z

   :parameter x: An instance of type :class:`<real>`. The first angle, in radians.
   :parameter y: An instance of type :class:`<real>`. The second angle, in radians.
   :value z: An instance of type :class:`<float>`.

   :description:

     Returns the arc tangent of ``x`` divided by ``y``. ``x`` may be zero if ``y``
     is not zero. The signs of ``x`` and ``y`` are used to derive what quadrant
     the angle falls in.

     The floating point precision of the result is given by the precision of
     ``x``/``y``. The result is a single-float if ``x/y`` is an integer.

.. generic-function:: atanh

   :summary:
     Returns the hyperbolic arc tangent of its argument.

   :signature: atanh x => y

   :parameter x: An instance of type :class:`<real>`. The angle, in radians.
   :value y: An instance of type :class:`<float>`.

   :description:

     Returns the hyperbolic arc tangent of its argument. The floating point
     precision of the result is given by the precision of ``x``. The result is
     a single-float if ``x`` is an integer.

   :seealso:

     - :func:`acosh`
     - :func:`asinh`

.. generic-function:: cos

   :summary:
     Returns the cosine of its argument.

   :signature: cos x => y

   :parameter x: An instance of type :class:`<real>`. The angle, in radians.
   :value y: An instance of type :class:`<float>`.

   :description:

     Returns the cosine of its argument. The floating point precision of the
     result is given by the precision of ``x``. The result is a single-float
     if ``x`` is an integer.

   :seealso:

     - :func:`sin`
     - :func:`tan`

.. generic-function:: cosh

   :summary:
     Returns the hyperbolic cosine of its argument.

   :signature: cosh x => y

   :parameter x: An instance of type :class:`<real>`. The angle, in radians.
   :value y: An instance of type :class:`<float>`.

   :description:

     Returns the hyperbolic cosine of its argument. The floating point
     precision of the result is given by the precision of ``x``. The result is
     a single-float if ``x`` is an integer.

   :seealso:

     - :func:`sinh`
     - :func:`tanh`


.. constant:: $double-e

    :summary:
      The value of *e*, the base of natural logarithms, as a double precision
      floating point number.

    :type: <double-float>

    :superclass: <float>

    :description:

      The value of *e*, the base of natural logarithms, as a double precision
      floating point number.

   :seealso:

     - :const:`$single-e`

.. constant:: $double-pi

   :summary:
     The value of *π* as a double precision floating point number.

   :type: <double-float>

   :superclass: <float>

   :description:

     The value of *π* as a double precision floating point number.

   :seealso:

     - :const:`$single-pi`

.. generic-function:: exp

   :summary:
     Returns *e*, the base of natural logarithms, raised to the power
     indicated by its argument.

   :signature: exp x => y

   :parameter x: An instance of type :class:`<real>`.
   :value y: An instance of type :class:`<float>`.

   :description:

     Returns *e*, the base of natural logarithms, raised to the power ``x``.
     The floating point precision is given by the precision of ``x``.

   :seealso:

     - :func:`^`
     - :func:`log`

.. generic-function:: isqrt

   :summary:
     Returns the integer square root of its argument.

   :signature: isqrt x => y

   :parameter x: An instance of type :class:`<integer>`.
   :value y: An instance of type :class:`<integer>`.

   :description:

     Returns the integer square root of ``x``, that is the greatest integer
     less than or equal to the exact positive square root of ``x``. If ``x`` <
     ``0``, an error is signalled.

   :seealso:

     - :func:`sqrt`

.. generic-function:: log

   :summary:
     Returns the natural logarithm of its argument.

   :signature: log x => y

   :parameter x: An instance of type :class:`<real>`.
   :value y: An instance of type :class:`<float>`.

   :description:

     Returns the natural logarithm of ``x`` to the base e. If x <= 0 <= 1, an
     error is signalled. The floating point precision of the result is given
     by the precision of ``x``. The result is a single-float if ``x`` is an
     integer.

   :seealso:

     - :func:`exp`
     - :func:`logn`

.. generic-function:: logn

   :summary:
     Returns the logarithm of its argument to the given base.

   :signature: logn x b => y

   :parameter x: An instance of :class:`<number>`
   :parameter b: An instance of :class:`<number>`. The base.
   :value y: An instance of :class:`<number>`.

   :description:

     Returns the logarithm of ``x`` to the base ``b``. If x <= 0 <= 1,
     an error is signalled. The floating point precision of the result is
     given by the precision of ``x``. The result is a single-float if
     ``x`` is an integer.

   :seealso:

     - :func:`logn`
     - :func:`exp`

.. generic-function:: sin

   :summary:
     Returns the sine of its argument.

   :signature: sin x => y

   :parameter x: An instance of type :class:`<real>`. The angle, in radians.
   :value y: An instance of type :class:`<float>`.

   :description:

     Returns the sine of its argument. The floating point precision of the
     result is given by the precision of ``x``. The result is a single-float
     if ``x`` is an integer.

   :seealso:

     - :func:`cos`
     - :func:`tan`

.. constant:: $single-e

   :summary:
     The value of *e*, the base of natural logarithms, as a single precision
     floating point number.

   :type: <single-float>

   :superclass: <float>

   :description:

     The value of *e*, the base of natural logarithms, as a single precision
     floating point number.

   :seealso:

     - :const:`$double-e`

.. constant:: $single-pi

   :summary:
     The value of *π* as a single precision floating point number.

   :type: <single-float>

   :superclass: <float>

   :description:

     The value of *π* as a single precision floating point number.

   :seealso:

     - :const:`$double-pi`

.. generic-function:: sinh

   :summary:
     Returns the hyperbolic sine of its argument.

   :signature: sinh x => y

   :parameter x: An instance of type :class:`<real>`. The angle, in radians.
   :value y: An instance of type :class:`<float>`.

   :description:

     Returns the hyperbolic sine of its argument. The floating point
     precision of the result is given by the precision of ``x``. The result is
     a single-float if ``x`` is an integer.

   :seealso:

     - :func:`cosh`
     - :func:`tanh`

.. generic-function:: sqrt

   :summary:
     Returns the square root of its argument.

   :signature: sqrt x => y

   :parameter x: An instance of type :class:`<real>`.
   :value y: An instance of type :class:`<float>`.

   :description:

     Returns the square root of x. If x is less than zero an error is
     signalled. The floating point precision of the result is given by the
     precision of ``x``. The result is a single-float if ``x`` is an integer.

   :seealso:

     - :func:`isqrt`

.. generic-function:: tan

   :summary:
     Returns the tangent of its argument.

   :signature: tan x => y

   :parameter x: An instance of type :class:`<real>`. The angle, in radians.
   :value y: An instance of type :class:`<float>`.

   :description:

     Returns the tangent of its argument. The floating point precision of the
     result is given by the precision of ``x``. The result is a single-float
     if ``x`` is an integer.

   :seealso:

     - :func:`cos`
     - :func:`sin`

.. generic-function:: tanh

   :summary:
     Returns the hyperbolic tangent of its argument.

   :signature: tanh x => y

   :parameter x: An instance of type :class:`<real>`. The angle, in radians.
   :parameter y: An instance of type :class:`<float>`.

   :description:

     Returns the hyperbolic tangent of its argument. The floating point
     precision of the result is given by the precision of ``x``. The result is
     a single-float if ``x`` is an integer.

   :seealso:

     - :func:`cosh`
     - :func:`sinh`
