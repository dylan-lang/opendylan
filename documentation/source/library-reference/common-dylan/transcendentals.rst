**************************
The transcendentals Module
**************************

.. current-library:: common-dylan
.. current-module:: transcendentals

.. module:: transcendentals

The *transcendentals* module, exported from the :doc:`common-dylan <index>`
library, provides a set of generic functions for ANSI C-like behavior over real
numbers. The restrictions and error cases described in this document are
intended to be the same as they are in ANSI C.

Because implementation of these functions might be by a standard library
for transcendentals accessed by a foreign function interface, the exact
precision and algorithms (and hence, the exact results) for all of these
functions is explicitly unspecified.

Note, however, that a program may expect the following, even in libraries that
are implemented by calling foreign libraries:

- Domain and range errors should be signalled as Dylan errors.
- Floating point precision *contagion* must obey Dylan rules. That is,
  functions called on single precision values return single precision results,
  and functions on double precision values return double precision results.
  When a function (e.g., :drm:`^`, :gf:`atan2`, etc.) accepts two arguments, if
  either argument is a double precision value then the result is also double
  precision.

As a rule this module does not automatically convert integer values to floating
point values. Callers should do so explicitly, so as to choose the appropriate
floating point type for their needs.

Complex numbers are not implemented. If the result of calling any
*transcendentals* function would be a complex number :drm:`<error>` is
signalled.

Reference
=========

This section contains a reference entry for each item exported from the
*transcendentals* module.

Constants
---------

.. constant:: $single-e

   The value of *e*, the base of natural logarithms, as a single precision
   floating point number.

   :type: :drm:`<single-float>`

.. constant:: $double-e

   The value of *e*, the base of natural logarithms, as a double precision
   floating point number.

   :type: :drm:`<double-float>`

.. constant:: $single-pi

   The value of *π* as a single precision floating point number.

   :type: :drm:`<single-float>`

.. constant:: $double-pi

   The value of *π* as a double precision floating point number.

   :type: :drm:`<double-float>`


Functions
---------

.. method:: ^
   :specializer: <single-float>, <single-float>

   Single precision floating point implementation of :drm:`^`.

   :signature: **base** ^ **exponent** => y

   Returns **base** raised to the power **exponent** as a
   :drm:`<single-float>`. If **base** is ``0`` and **exponent** is not
   positive, an error is signalled. If **base** is negative and **exponent** is
   not an integer, an error is signalled.

.. method:: ^
   :specializer: <double-float>, <double-float>

   Double precision floating point implementation of :drm:`^`.

   :signature: **base** ^ **exponent** => y

   Returns **base** raised to the power **exponent** as a
   :drm:`<double-float>`. If **base** is ``0`` and **exponent** is not
   positive, an error is signalled. If **base** is negative and **exponent** is
   not an integer, an error is signalled.

.. method:: ^
   :specializer: <single-float>, <double-float>
   :no-contents-entry:

   Converts the first argument to :drm:`<double-float>` and calls
   :meth:`^(<double-float>, <double-float>)`.

.. method:: ^
   :specializer: <double-float>, <single-float>
   :no-contents-entry:

   Converts the second argument to :drm:`<double-float>` and calls
   :meth:`^(<double-float>, <double-float>)`.


.. generic-function:: acos
   :open:

   :signature: acos(x) => y
   :parameter x: An instance of type :drm:`<number>`. The angle, in radians.
                 If **x** is not in the range ``[-1,1]``, an error is signalled.
   :value y: An instance of type :drm:`<number>`.

   Returns the arc cosine of its argument. The floating point precision of the
   result is given by the precision of **x**.

   :seealso: :gf:`asin`, :gf:`atan`

.. method:: acos
   :specializer: <single-float>
   :no-contents-entry:

   Single precision floating point implementation of :gf:`acos`. Returns a
   :drm:`<single-float>`.

.. method:: acos
   :specializer: <double-float>
   :no-contents-entry:

   Double precision floating point implementation of :gf:`acos`. Returns a
   :drm:`<double-float>`.


.. generic-function:: acosh
   :open:

   :signature: acosh(x) => y
   :parameter x: An instance of type :drm:`<number>`. The angle, in radians.
   :value y: An instance of type :drm:`<number>`.

   Returns the hyperbolic arc cosine of its argument. The floating point
   precision of the result is given by the precision of **x**.

   :seealso: :gf:`asinh`, :gf:`atanh`

.. method:: acosh
   :specializer: <single-float>
   :no-contents-entry:

   Single precision floating point implementation of :gf:`acosh`. Returns a
   :drm:`<single-float>`.

.. method:: acosh
   :specializer: <double-float>
   :no-contents-entry:

   Double precision floating point implementation of :gf:`acosh`. Returns a
   :drm:`<double-float>`.


.. generic-function:: asin

   :signature: asin(x) => y

   :parameter x: An instance of type :drm:`<number>`. The angle, in radians.
                 If **x** is not in the range `[-1,+1]`, an error is signalled.
   :value y: An instance of type :drm:`<number>`.

   Returns the arc sine of its argument. The floating point precision of
   the result is given by the precision of **x**.

   :seealso: :gf:`acos`, :gf:`atan`

.. method:: asin
   :specializer: <single-float>
   :sealed:
   :no-contents-entry:

   Single precision floating point implementation of :gf:`asin`. Returns a
   :drm:`<single-float>`.

.. method:: asin
   :specializer: <double-float>
   :sealed:
   :no-contents-entry:

   Double precision floating point implementation of :gf:`asin`. Returns a
   :drm:`<double-float>`.


.. generic-function:: asinh

   :signature: asinh(x) => y

   :parameter x: An instance of type :drm:`<number>`. The angle, in radians.
   :value y: An instance of type :drm:`<number>`.

   Returns the hyperbolic arc sine of its argument. The floating point
   precision of the result is given by the precision of **x**.

   :seealso: :gf:`acosh`, :gf:`atanh`

.. method:: asinh
   :specializer: <single-float>
   :sealed:
   :no-contents-entry:

   Single precision floating point implementation of :gf:`asinh`. Returns a
   :drm:`<single-float>`.

.. method:: asinh
   :specializer: <double-float>
   :sealed:
   :no-contents-entry:

   Double precision floating point implementation of :gf:`asinh`. Returns a
   :drm:`<double-float>`.


.. generic-function:: atan

   :signature: atan(x) => y

   :parameter x: An instance of type :drm:`<number>`. The angle, in radians.
                 If **x** is not in the range `[-1,+1]`, an error is signalled.
   :value y: An instance of type :drm:`<number>`.

   Returns the arc tangent of its argument. The floating point precision of the
   result is given by the precision of *x*.

   :seealso: :gf:`acos`, :gf:`asin`

.. method:: atan
   :specializer: <single-float>
   :sealed:
   :no-contents-entry:

   Single precision floating point implementation of :gf:`atan`. Returns a
   :drm:`<single-float>`.

.. method:: atan
   :specializer: <double-float>
   :sealed:
   :no-contents-entry:

   Double precision floating point implementation of :gf:`atan`. Returns a
   :drm:`<double-float>`.


.. generic-function:: atan2

   :signature: atan2(x, y) => z

   :parameter x: An instance of type :drm:`<number>`. The first angle, in radians.
   :parameter y: An instance of type :drm:`<number>`. The second angle, in radians.
   :value z: An instance of type :drm:`<number>`.

   Returns the arc tangent of **x** divided by **y**. **x** may be zero if
   **y** is not zero. The signs of **x** and **y** are used to derive what
   quadrant the angle falls in.

.. method:: atan2
   :specializer: <single-float>, <single-float>
   :sealed:
   :no-contents-entry:

   Single precision floating point implementation of :gf:`atan2`. Returns a
   :drm:`<single-float>`.

.. method:: atan2
   :specializer: <double-float>, <double-float>
   :sealed:
   :no-contents-entry:

   Double precision floating point implementation of :gf:`atan2`. Returns a
   :drm:`<double-float>`.

.. method:: atan2
   :specializer: <single-float>, <double-float>
   :sealed:
   :no-contents-entry:

   Converts the first argument to :drm:`<double-float>` and calls
   :meth:`atan2(<double-float>, <double-float>)`.

.. method:: atan2
   :specializer: <double-float>, <single-float>
   :sealed:
   :no-contents-entry:

   Converts the second argument to :drm:`<double-float>` and calls
   :meth:`atan2(<double-float>, <double-float>)`.


.. generic-function:: atanh

   :signature: atanh(x) => y

   :parameter x: An instance of type :drm:`<number>`. The angle, in radians.
   :value y: An instance of type :drm:`<number>`.

   Returns the hyperbolic arc tangent of its argument. The floating point
   precision of the result is given by the precision of **x**.

   :seealso: :gf:`acosh`, :gf:`asinh`

.. method:: atanh
   :specializer: <single-float>
   :sealed:
   :no-contents-entry:

   Single precision floating point implementation of :gf:`atanh`. Returns a
   :drm:`<single-float>`.

.. method:: atanh
   :specializer: <double-float>
   :sealed:
   :no-contents-entry:

   Double precision floating point implementation of :gf:`atanh`. Returns a
   :drm:`<double-float>`.


.. generic-function:: cos

   :signature: cos(x) => y

   :parameter x: An instance of type :drm:`<number>`. The angle, in radians.
   :value y: An instance of type :drm:`<number>`.

   Returns the cosine of its argument. The floating point precision of the
   result is given by the precision of **x**.

   :seealso: :gf:`sin`, :gf:`sincos`, :gf:`tan`

.. method:: cos
   :specializer: <single-float>
   :sealed:
   :no-contents-entry:

   Single precision floating point implementation of :gf:`cos`. Returns a
   :drm:`<single-float>`.

.. method:: cos
   :specializer: <double-float>
   :sealed:
   :no-contents-entry:

   Double precision floating point implementation of :gf:`cos`. Returns a
   :drm:`<double-float>`.


.. generic-function:: cosh

   :signature: cosh(x) => y

   :parameter x: An instance of type :drm:`<number>`. The angle, in radians.
   :value y: An instance of type :drm:`<number>`.

   Returns the hyperbolic cosine of its argument. The floating point
   precision of the result is given by the precision of **x**.

   :seealso: :gf:`sinh`, :gf:`tanh`

.. method:: cosh
   :specializer: <single-float>
   :sealed:
   :no-contents-entry:

   Single precision floating point implementation of :gf:`cosh`. Returns a
   :drm:`<single-float>`.

.. method:: cosh
   :specializer: <double-float>
   :sealed:
   :no-contents-entry:

   Double precision floating point implementation of :gf:`cosh`. Returns a
   :drm:`<double-float>`.


.. generic-function:: exp

   :signature: exp(x) => y

   :parameter x: An instance of type :drm:`<number>`.
   :value y: An instance of type :drm:`<number>`.

   Returns *e*, the base of natural logarithms, raised to the power **x**.  The
   floating point precision of the result is given by the precision of **x**.

   :seealso: :drm:`^`, :func:`ilog2`, :gf:`log`, :func:`logn`

.. method:: exp
   :specializer: <single-float>
   :sealed:
   :no-contents-entry:

   Single precision floating point implementation of :gf:`exp`. Returns a
   :drm:`<single-float>`.

.. method:: exp
   :specializer: <double-float>
   :sealed:
   :no-contents-entry:

   Double precision floating point implementation of :gf:`exp`. Returns a
   :drm:`<double-float>`.


.. generic-function:: hypot

   :signature: hypot(x, y) => z

   :parameter x: An instance of type :drm:`<number>`.
   :parameter y: An instance of type :drm:`<number>`.
   :value z: An instance of type :drm:`<number>`.

   Returns the Euclidean distance without unnecessary overflow or underflow.

.. method:: hypot
   :specializer: <single-float>, <single-float>
   :no-contents-entry:

   Returns the Euclidean distance as a :drm:`<single-float>` without
   unnecessary overflow or underflow.

.. method:: hypot
   :specializer: <double-float>, <double-float>
   :no-contents-entry:

   Returns the Euclidean distance as a :drm:`<double-float>` without
   unnecessary overflow or underflow.

.. method:: hypot
   :specializer: <single-float>, <double-float>
   :no-contents-entry:

   Converts the first argument to :drm:`<double-float>` and calls
   :meth:`hypot(<double-float>, <double-float>)`.

.. method:: hypot
   :specializer: <double-float>, <single-float>
   :no-contents-entry:

   Converts the second argument to :drm:`<double-float>` and calls
   :meth:`hypot(<double-float>, <double-float>)`.


.. function:: isqrt

   :signature: isqrt(x) => y

   :parameter x: An instance of type :drm:`<integer>`.
   :value y: An instance of type :drm:`<integer>`.

   Returns the integer square root of **x**, that is the greatest integer less
   than or equal to the exact positive square root of **x**. If ``x < 0``, an
   error is signalled.

   :seealso: :gf:`sqrt`


.. generic-function:: log

   Returns the natural logarithm of its argument.

   :signature: log(x) => y

   :parameter x: An instance of type :drm:`<number>`.
   :value y: An instance of type :drm:`<number>`.

   Returns the natural logarithm of **x** to the base *e*. If ``x <= 0``, an
   error is signalled. The floating point precision of the result is given by
   the precision of **x**.

   :seealso: :gf:`exp`, :func:`ilog2`, :func:`logn`

.. method:: log
   :specializer: <single-float>
   :no-contents-entry:

   :signature: log(x) => y

   :parameter x: An instance of type :drm:`<single-float>`.
   :value y: An instance of type :drm:`<single-float>`.

   Returns the natural logarithm of **x** to the base *e* as a
   :drm:`<single-float>`.

.. method:: log
   :specializer: <double-float>
   :no-contents-entry:

   :signature: log(x) => y

   :parameter x: An instance of type :drm:`<double-float>`.
   :value y: An instance of type :drm:`<double-float>`.

   Returns the natural logarithm of **x** to the base *e* as a
   :drm:`<single-float>`.


.. function:: logn

   Returns the logarithm of its argument to the given base.

   :signature: logn(x, base) => y

   :parameter x: An instance of :drm:`<number>`
   :parameter base: The base. An instance of :drm:`<number>`.
   :value y: An instance of :drm:`<number>`.

   Returns the logarithm of **x** to the base **base**. If ``x <= 0`` or ``base
   <= 1``, an error is signalled. The floating point precision of the result is
   given by the precision of **x**.

   .. note:: In practice both **x** and **base** must be instances of
             :drm:`<float>` since they are passed directly to :gf:`log`,
             which only has methods on :drm:`<float>`.

   :seealso: :gf:`exp`, :gf:`log`, :func:`ilog2`


.. function:: ilog2

   :signature: ilog2(x) => y

   :parameter x: An instance of :drm:`<integer>`.
   :value y: An instance of :drm:`<integer>`.

   Returns the integer base 2 logarithm of **x**, truncated to an
   :drm:`<integer>`.  That is, it returns the greatest integer less than or
   equal to the exact base 2 logarithm of **x**.

   :seealso: :gf:`exp`, :func:`logn`, :gf:`log`


.. generic-function:: sin

   :signature: sin(x) => y

   :parameter x: An instance of type :drm:`<number>`. The angle, in radians.
   :value y: An instance of type :drm:`<number>`.

   Returns the sine of its argument. The floating point precision of the result
   is given by the precision of **x**.

   :seealso: :gf:`cos`, :gf:`sincos`, :gf:`tan`

.. method:: sin
   :specializer: <single-float>
   :sealed:
   :no-contents-entry:

   Single precision floating point implementation of :gf:`sin`. Returns a
   :drm:`<single-float>`.

.. method:: sin
   :specializer: <double-float>
   :sealed:
   :no-contents-entry:

   Double precision floating point implementation of :gf:`sin`. Returns a
   :drm:`<double-float>`.


.. generic-function:: sincos

   :signature: sincos(x) => (sin, cos)

   :parameter x: An instance of type :drm:`<number>`. The angle, in radians.
   :value sin: An instance of type :drm:`<number>`. The result of ``sin(x)``.
   :value cos: An instance of type :drm:`<number>`. The result of ``cos(x)``.

   Returns both the sine and the cosine of its argument. The floating point
   precision of the results is given by the precision of **x**. In some
   implementations :gf:`sincos` may have better performance than calling
   ``sin(x)`` and ``cos(x)`` separately.

   :seealso: :gf:`cos`, :gf:`sin`

.. method:: sincos
   :specializer: <single-float>
   :sealed:
   :no-contents-entry:

   Single precision floating point implementation of :gf:`sincos`. Returns a
   :drm:`<single-float>`.

.. method:: sincos
   :specializer: <double-float>
   :sealed:
   :no-contents-entry:

   Double precision floating point implementation of :gf:`sincos`. Returns a
   :drm:`<double-float>`.


.. generic-function:: sinh

   :signature: sinh(x) => y

   :parameter x: An instance of type :drm:`<number>`. The angle, in radians.
   :value y: An instance of type :drm:`<number>`.

   Returns the hyperbolic sine of its argument. The floating point precision of
   the result is given by the precision of **x**.

   :seealso: :gf:`cosh`, :gf:`tanh`

.. method:: sinh
   :specializer: <single-float>
   :sealed:
   :no-contents-entry:

   Single precision floating point implementation of :gf:`sinh`. Returns a
   :drm:`<single-float>`.

.. method:: sinh
   :specializer: <double-float>
   :sealed:
   :no-contents-entry:

   Double precision floating point implementation of :gf:`sinh`. Returns a
   :drm:`<double-float>`.


.. generic-function:: sqrt

   :signature: sqrt(x) => y

   :parameter x: An instance of type :drm:`<number>`.
   :value y: An instance of type :drm:`<number>`.

   Returns the square root of **x**. If **x** is less than zero an error is
   signalled. The floating point precision of the result is given by the
   precision of **x**.

   :seealso: :gf:`isqrt`

.. method:: sqrt
   :specializer: <single-float>
   :sealed:
   :no-contents-entry:

   Single precision floating point implementation of :gf:`sqrt`. Returns a
   :drm:`<single-float>`.

.. method:: sqrt
   :specializer: <double-float>
   :sealed:
   :no-contents-entry:

   Double precision floating point implementation of :gf:`sqrt`. Returns a
   :drm:`<double-float>`.


.. generic-function:: tan

   :signature: tan(x) => y

   :parameter x: An instance of type :drm:`<number>`. The angle, in radians.
   :value y: An instance of type :drm:`<number>`.

   Returns the tangent of **x**. The floating point precision of the result is
   given by the precision of **x**.

   :seealso: :gf:`cos`, :gf:`sin`

.. method:: tan
   :specializer: <single-float>
   :sealed:
   :no-contents-entry:

   Single precision floating point implementation of :gf:`tan`. Returns a
   :drm:`<single-float>`.

.. method:: tan
   :specializer: <double-float>
   :sealed:
   :no-contents-entry:

   Double precision floating point implementation of :gf:`tan`. Returns a
   :drm:`<double-float>`.


.. generic-function:: tanh

   :signature: tanh(x) => y

   :parameter x: An instance of type :drm:`<number>`. The angle, in radians.
   :value y: An instance of type :drm:`<number>`.

   Returns the hyperbolic tangent of **x**. The floating point precision
   of the result is given by the precision of **x**.

   :seealso: :gf:`cosh`, :gf:`sinh`

.. method:: tanh
   :specializer: <single-float>
   :sealed:
   :no-contents-entry:

   Single precision floating point implementation of :gf:`tanh`. Returns a
   :drm:`<single-float>`.

.. method:: tanh
   :specializer: <double-float>
   :sealed:
   :no-contents-entry:

   Double precision floating point implementation of :gf:`tanh`. Returns a
   :drm:`<double-float>`.
