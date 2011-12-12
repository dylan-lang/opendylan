***********************
Expressions & Variables
***********************

Dylan identifiers may contain a greater variety of characters
than those of C or Pascal. Specifically, variable names may contain all
alphanumeric characters, plus the symbols ``! & * < = >
| ^ $ % @ _ - + ~ ? /``. Identifiers may not begin with the
symbols ``- + ~ ? /``, although identifiers may begin
with numbers, provided they contain at least two alphabetic characters
in a row. As in Pascal, variable names are not case sensitive.

.. todo:: Need DRM footnote here.

This means that ``(a - b)`` subtracts one variable
from another, whereas ``(a-b)`` simply returns the value
of the hyphenated variable named ``a-b``. Because of this,
infix operators, such as addition, subtraction and equality, must be
surrounded by whitespace.

As in C++, Dylan infix operators may also be referred to as
functions. In C++, ``(a + b)`` could also be written
as ``operator+(a, b)``. In Dylan, the same expression
could be written ``\+(a, b)``. In both languages,
programmers can use this flexibility to define operators for custom
numeric classes.

Naming Conventions
==================

Dylan uses the extra characters permitted in variable names to
support a number of standard naming conventions, as shown in this table:

+-----------------+-----------------------------------------------------+
| ``<string>``    | a class                                             |
+-----------------+-----------------------------------------------------+
| ``insert!``     | mutative function (modifies argument destructively) |
+-----------------+-----------------------------------------------------+
| ``empty?``      | predicate function (tests one or more arguments and |
|                 | returns either true or false)                       |
+-----------------+-----------------------------------------------------+
| ``write-line``  | a two word name                                     |
+-----------------+-----------------------------------------------------+
| ``$name``       | constant                                            |
+-----------------+-----------------------------------------------------+
| ``*name*``      | module-level variable                               |
+-----------------+-----------------------------------------------------+

True and False
==============

Dylan represents true as ``#t`` and false as ``#f``. When evaluated in a
Boolean context, all values other than ``#f`` return true. Thus, the number
zero -- and other common "false" values -- evaluate as
true in Dylan.

The Nature of Variables
=======================

Dylan variables differ from those found in C and Pascal. Instead
of *holding* their values, Dylan variables
*refer* to them. Conceptually, they resemble a
cross between pointers and C++ references. Like references, Dylan
variables may be evaluated without any indirection. Like pointers,
they may be set to point to new objects whenever the programmer
desires.

Furthermore, there's only one of any given numeric value in a
Dylan program, at least from the programmer's point of view. All
variables which refer to the integer 2 -- or, in Dylan-speak, are
:term:`bound` to the integer 2 -- point to the
exact same thing.

.. code-block:: dylan

    let x = 2; // creates x and binds it to 2
    x := 3;    // rebinds x to the value 3
    let y = x; // creates y, and binds it to
               // whatever x is bound to

If two variables are bound to one object with internal
structure, the results may surprise C and Pascal programmers.

.. code-block:: dylan

    let car1 = make(<car>); // bind car1 to a
                                  // new car object
    car1.odometer := 10000;       // set odometer
    let car2 = car1;              // bind new name
    car2.odometer := 0;           // reset odometer
    car1.odometer;                // evaluates to 0!

As long as one or more variables refer to an object, it
continues to exist. However, as soon as the last reference either
goes out of scope or gets rebound, the object becomes :term:`garbage`.
Since there's no way that the program could ever refer to the object
again, the :term:`garbage collector` feels free to reuse the memory
which once held it.

Note that Dylan variables *must* be bound to a
particular value when they are declared. In the name of type safety
and implementation efficiency, every variable must refer to some
well-defined object.

Assignment, Equality and Identity
=================================

Dylan uses all three of the "equals" operators
found in C and Pascal, albeit in a different fashion. The Pascal
assignment operator, ``:=``, rebinds Dylan variable
names to new values. The Pascal equality operator, ``=``,
tests for equality in Dylan and also appears in some
language constructs such as ``let``. (Two Dylan objects
are equal, generally, if they belong to the same class and have equal
substructure.)

The C equality operator, ``==``, acts as the
:term:`identity` operator in Dylan. Two variables are
:term:`identical` if and only if they are bound to the
exact same object. For example, the following three expressions mean
roughly the same thing::

    (a == b)   // in Dylan
    (&a == &b) // in C or C++
    (@a = @b) // in Pascal

The following piece of source code demonstrates all three
operators in actual use.

.. code-block:: dylan

    let car1 = make(<car>);
    let car2 = make(<car>);
    let car3 = car2;

    car2 = car3;// #t
    car1 = car2;// ??? (see below)
    car2 == car3;// #t
    car1 == car2;// #f

    car2 := car1;// rebind
    car1 == car2;// #t

    let x = 2;
    let y = 2;

    x = y;// #t
    x == y;// #t (only one 2!)

Two of the examples merit further explanation. First, we don't
know whether ``car1 = car2``, because we don't know if
make creates each car with the same serial number, driver and other
information as previous cars. If and only if none of those values
differ, then ``car1`` equals ``car2``.
Second, ``x == y`` because every variable bound to a
given number refers to the exact same instance of that number, at least
from the programmer's perspective. (The compiler will normally do
something more useful and efficient when generating the actual machine
code.)  Strings behave in a fashion different from numbers -- 
instances of strings are stored separately, and two equal strings are
not necessarily the same string.

Parallel Values
===============

It's possible to bind more than one variable at a time in Dylan.
For example, a single ``let`` statement could bind
``x`` to 2, ``y`` to 3 and ``z`` to 4.

.. code-block:: dylan

    let (x, y, z) = values (2, 3, 4);

In Perl, the equivalent statement would assign a vector of
values to a vector of variables. In Dylan, no actual vectors or lists
are used. All three values are assigned directly, using some
implementation-dependent mechanism.

.. _type-declarations:

Type Declarations
=================

Dylan variables may have explicit types. This allows the
compiler to generate better code and to catch type-mismatch errors at
compile time. To take advantage of this feature, use the ``::`` operator:

.. code-block:: dylan

    let x :: <integer> = 2;
    let vehicle :: <vehicle> = make(<car>);
    let y :: <number> = 3; // any numeric class
    let z :: <integer> = vehicle; // error!

As seen in the example, a variable may be bound to values of its
declared type or to values of subclasses of its declared type. Type
mismatch errors should be caught at compile time. In general, the
compiler may infer the types of variables at when generating machine
code. If a local variable never gets rebound to anything other than an
integer, for example, the compiler can rely on this fact to optimize
the resulting code.

Module Variables and Constants
==============================

Dylan supports :term:`module-level` variables,
which serve roughly the same purpose as C's global variables. Although
the ``let`` function may only be used within :term:`methods`
(Dylan-speak for regular functions), the forms ``define variable`` and
``define constant`` may be used at the top level.

.. code-block:: dylan

    define variable *x* :: <integer> = 3;
    define variable *y* = 4;
    define constant $hi = "Hi!";

Note that there's not much point in declaring types for
constants. Any remotely decent compiler will be able to figure that
information out on its own.
