**************************
The transcendentals Module
**************************

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

^
~

G.f. method
-----------

Summary

Returns its first argument, raised to the power indicated by its second
argument.

Signature

^ *b* *x* => *y*

Arguments

*b* An instance of type ``<real>``.

*x* An instance of type ``<real>``.

Values

*y* An instance of type ``<float>``.

Description

Returns *b* raised to the power *x*. If *b* is *0* and *x* is not
positive, an error is signalled. If *b* is negative and *x* is not an
integer, an error is signalled.

If *b* and *x* are both integers, the result is an integer. If *x* is
negative, an error is signalled.

The floating point precision is given by the precision of *b*. The
result is a single-float if *b* is an integer.

See also

See the function `exp`_.

acos
----

G.f. method
-----------

Summary

Returns the arc cosine of its argument.

Signature

acos *x* => *y*

Arguments

*x* An instance of type ``<real>``. The angle, in radians. If *x* is not
in the range *[-1,+1]*, an error is signalled.

Values

*y* An instance of type ``<float>``.

Description

Returns the arc cosine of its argument. The floating point precision of
the result is given by the precision of *x*. The result is a
single-float if *x* is an integer.

See also

See the functions `asin`_ and `atan`_.

acosh
-----

G.f. method
-----------

Summary

Returns the hyperbolic arc cosine of its argument.

Signature

acosh *x* => *y*

Arguments

*x* An instance of type ``<real>``. The angle, in radians.

Values

*y* An instance of type ``<float>``.

Description

Returns the hyperbolic arc cosine of its argument. The floating point
precision of the result is given by the precision of *x*. The result is
a single-float if *x* is an integer.

See also

See the functions `asinh`_ and `atanh`_.

asin
----

G.f. method
-----------

Summary

Returns the arc sine of its argument.

Signature

asin *x* => *y*

Arguments

*x* An instance of type ``<real>``. The angle, in radians. If *x* is not
in the range *[-1,+1]*, an error is signalled.

Values

*y* An instance of type ``<float>``.

Description

Returns the arc sine of its argument. The floating point precision of
the result is given by the precision of *x*. The result is a
single-float if *x* is an integer.

See also

See the functions `acos`_ and `atan`_.

asinh
-----

G.f. method
-----------

Summary

Returns the hyperbolic arc sine of its argument.

Signature

asinh *x* => *y*

Arguments

*x* An instance of type ``<real>``. The angle, in radians.

Values

*y* An instance of type ``<float>``.

Description

Returns the hyperbolic arc sine of its argument. The floating point
precision of the result is given by the precision of *x*. The result is
a single-float if *x* is an integer.

See also

See the functions `acosh`_ and `atanh`_.

atan
----

G.f. method
-----------

Summary

Returns the arc tangent of its argument.

Signature

atan *x* => *y*

Arguments

*x* An instance of type ``<real>``. The angle, in radians. If *x* is not
in the range *[-1,+1]*, an error is signalled.

Values

*y* An instance of type ``<float>``.

Description

Returns the arc tangent of its argument. The floating point precision of
the result is given by the precision of *x*. The result is a
single-float if *x* is an integer.

See also

See the functions `acos`_ and `asin`_.

atan2
-----

G.f. method
-----------

Summary

Returns the arc tangent of one angle divided by another.

Signature

atan2 *x* *y* => *z*

Arguments

*x* An instance of type ``<real>``. The first angle, in radians.

*y* An instance of type ``<real>``. The second angle, in radians.

Values

*z* An instance of type ``<float>``.

Description

Returns the arc tangent of *x* divided by *y*. x may be zero if y is
not zero. The signs of x and y are used to derive what quadrant the
angle falls in.

The floating point precision of the result is given by the precision of
*x* /y. The result is a single-float if *x/y* is an integer.

atanh
-----

G.f. method
-----------

Summary

Returns the hyperbolic arc tangent of its argument.

Signature

atanh *x* => *y*

Arguments

*x* An instance of type ``<real>``. The angle, in radians.

Values

*y* An instance of type ``<float>``.

Description

Returns the hyperbolic arc tangent of its argument. The floating point
precision of the result is given by the precision of *x*. The result is
a single-float if *x* is an integer.

See also

See the functions `acosh`_ and `asinh`_.

cos
---

G.f. method
-----------

Summary

Returns the cosine of its argument.

Signature

cos *x* => *y*

Arguments

*x* An instance of type ``<real>``. The angle, in radians.

Values

*y* An instance of type ``<float>``.

Description

Returns the cosine of its argument. The floating point precision of the
result is given by the precision of *x*. The result is a single-float
if *x* is an integer.

See also

See the functions `sin`_ and `tan`_.

cosh
----

G.f. method
-----------

Summary

Returns the hyperbolic cosine of its argument.

Signature

cosh *x* => *y*

Arguments

*x* An instance of type ``<real>``. The angle, in radians.

Values

*y* An instance of type ``<float>``.

Description

Returns the hyperbolic cosine of its argument. The floating point
precision of the result is given by the precision of *x*. The result is
a single-float if *x* is an integer.

See also

See the functions `sinh`_ and `tanh`_.

$double-e
---------

Constant
--------

Summary

The value of *e*, the base of natural logarithms, as a double precision
floating point number.

Type

``<double-float>``

Superclass

``<float>``

Description

The value of *e*, the base of natural logarithms, as a double precision
floating point number.

See also

See the constant `$single-e`_.

$double-pi
----------

Constant
--------

Summary

The value of π as a double precision floating point number.

Type

``<double-float>``

Superclass

``<float>``

Description

The value of π as a double precision floating point number.

See also

See the constant `$single-pi`_.

exp
---

G.f. method
-----------

Summary

Returns *e*, the base of natural logarithms, raised to the power
indicated by its argument.

Signature

exp *x* => *y*

Arguments

*x* An instance of type ``<real>``.

Values

*y* An instance of type ``<float>``.

Description

Returns *e*, the base of natural logarithms, raised to the power *x*.
The floating point precision is given by the precision of *x*.

See also

See the functions `^`_ and `log`_.

isqrt
-----

G.f. method
-----------

Summary

Returns the integer square root of its argument.

Signature

isqrt *x* => *y*

Arguments

*x* An instance of type ``<integer>``.

Values

*y* An instance of type ``<integer>``.

Description

Returns the integer square root of *x*, that is the greatest integer
less than or equal to the exact positive square root of *x*. If *x* <
*0*, an error is signalled.

See also

See the function `sqrt`_.

log
---

G.f. method
-----------

Summary

Returns the natural logarithm of its argument.

Signature

log *x* => *y*

Arguments

*x* An instance of type *<real>.*

Values

*y* An instance of type ``<float>``.

Description

Returns the natural logarithm of *x* to the base e. If x <= 0 <= 1, an
error is signalled. The floating point precision of the result is given
by the precision of *x*. The result is a single-float if *x* is an
integer.

See also

See also `exp`_, and `logn`_.

logn
----

G.f. method
-----------

Summary

Returns the logarithm of its argument to the given base.

Signature

logn number, base

Arguments

number

*base* A number greater than *1*.

Description

Returns the logarithm of *number* to the base *base*. If x <= 0 <= 1,
an error is signalled. The floating point precision of the result is
given by the precision of *number*. The result is a single-float if
*number* is an integer.

See also

See also `log`_, and `exp`_.

sin
---

G.f. method
-----------

Summary

Returns the sine of its argument.

Signature

sin *x* => *y*

Arguments

*x* An instance of type ``<real>``. The angle, in radians.

Values

*y* An instance of type ``<float>``.

Description

Returns the sine of its argument. The floating point precision of the
result is given by the precision of *x*. The result is a single-float
if *x* is an integer.

See also

See the functions `cos`_ and `tan`_.

$single-e
---------

Constant
--------

Summary

The value of *e*, the base of natural logarithms, as a single precision
floating point number.

Type

<single-float>

Superclass

<float>

Description

The value of *e*, the base of natural logarithms, as a single precision
floating point number.

See also

See the constant `$double-e`_.

$single-pi
----------

Constant
--------

Summary

The value of π as a single precision floating point number.

Type

<single-float>

Superclass

<float>

Description

The value of π as a single precision floating point number.

See also

See the constant `$double-pi`_.

sinh
----

G.f. method
-----------

Summary

Returns the hyperbolic sine of its argument.

Signature

sinh *x* => *y*

Arguments

*x* An instance of type ``<real>``. The angle, in radians.

Values

*y* An instance of type ``<float>``.

Description

Returns the hyperbolic sine of its argument. The floating point
precision of the result is given by the precision of *x*. The result is
a single-float if *x* is an integer.

See also

See the functions `cosh`_ and `tanh`_.

sqrt
----

G.f. method
-----------

Summary

Returns the square root of its argument.

Signature

sqrt *x* => *y*

Arguments

*x* An instance of type ``<real>``. The angle, in radians.

Values

*y* An instance of type ``<float>``.

Description

Returns the square root of x. If x is less than zero an error is
signalled. The floating point precision of the result is given by the
precision of *x*. The result is a single-float if *x* is an integer.

See also

See the function `isqrt`_.

tan
---

G.f. method
-----------

Summary

Returns the tangent of its argument.

Signature

tan *x* => *y*

Arguments

*x* An instance of type ``<real>``. The angle, in radians.

Values

*y* An instance of type ``<float>``.

Description

Returns the tangent of its argument. The floating point precision of the
result is given by the precision of *x*. The result is a single-float
if *x* is an integer.

tanh
----

G.f. method
-----------

Summary

Returns the hyperbolic tangent of its argument.

Signature

tanh *x* => *y*

Arguments

*x* An instance of type ``<real>``. The angle, in radians.

Values

*y* An instance of type ``<float>``.

Description

Returns the hyperbolic tangent of its argument. The floating point
precision of the result is given by the precision of *x*. The result is
a single-float if *x* is an integer.

See also

See the functions `cosh`_ and `sinh`_.
