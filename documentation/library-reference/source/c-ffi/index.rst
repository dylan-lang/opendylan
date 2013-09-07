*****************
The C-FFI Library
*****************

.. current-library:: c-ffi
.. current-module:: c-ffi

.. contents::

Introduction
============

The C-FFI (*C foreign function interface*) library provides a means of
interfacing a Dylan application with code written in the C language. The
C-FFI library is available to applications as the module C-FFI in the
library C-FFI.

The C-FFI library consists of macros, classes, and functions that you
can use to write a complete description of the Dylan interface to a C
library. Compiling this description generates a set of Dylan classes and
functions through which Dylan code can manipulate the C library’s data
and call its functions. Interface descriptions can also allow C code to
call into Dylan; compiling such a description generates entry points
compatible with C’s calling conventions.

Overview
========

This section is an overview of the C-FFI library, introducing the most
commonly used constructs as the basis for examples.

The C-FFI library provides a set of macros that can be used to describe
a C interface in a form that the Open Dylan compiler can
understand; we call these macros the *C interface definition language*.

The C interface definition language provides macros that correspond to
each of C’s type, function, variable, and constant defining forms. These
macros define Dylan classes that designate and encapsulate instances of
C types, Dylan methods through which to manipulate C variables and call
out to C functions, and functions with C-compatible entry points through
which to call in to Dylan from C.

In addition to the interface definition language, the C-FFI library
provides run-time methods and functions for allocating, manipulating and
destroying instances of C data structures. For example, using these
facilities you can allocate C structs and arrays, and access and set
their elements.

C types in Dylan
----------------

When you use the interface definition language to describe a C type to
the Dylan compiler, the compiler generates a new Dylan class. This class
is said to *designate* the C type, which means that it carries with it
the essential properties of the C type such as its size and alignment.

You can use this *designator class* in subsequent interface definition
forms to specify which elements involve the designated C type. A
designator class also carries with it the information on how to
interpret the untyped C data as a tagged Dylan object.

The C-FFI library contains predefined designator classes for C’s
fundamental types like ``int`` and ``double``. The names of these
predefined Dylan classes are formed from the C name of the fundamental
type being designated. The designator class name for a particular C type
formed using Dylan’s standard class-naming convention; it is prefixed
with "C-", hyphenated if it contains more than one word, and enclosed in
angle brackets. For example, the C-FFI library provides the class
``<C-int>`` to designate the C type ``int`` ; it designates ``double``
by the class ``<C-double>``, and ``unsigned long`` by the class
``<C-unsigned-long>``.

.. note:: Since Dylan variable names are compared without sensitivity to
   case, the capitalization of the "C" in the names above, and in other
   Dylan names appearing in this document, is not binding and can safely be
   ignored.

The C-FFI library also provides predefined classes designating pointers
to C’s fundamental numeric types. To do so, it adds a ``*`` to the
fundamental C type designator. For example ``<C-double*>`` designates the
C type ``double*``.

The following is an example of defining and using designator classes.
Suppose we have the following C struct:

.. code-block:: c

    typedef struct {
      unsigned short x_coord;
      unsigned short y_coord;
    } Point;

We describe C structs to Dylan using the macro :macro:`define C-struct`:

.. code-block:: dylan

    define C-struct <Point>
      slot x-coord :: <C-unsigned-short>;
      slot y-coord :: <C-unsigned-short>;
    end C-struct;

This form defines a new designator class ``<Point>`` for a structure type
corresponding to the C type ``Point``. We designate the types of the
slots of ``<Point>`` using the Dylan classes designating the C types used
in the definition of ``Point``. In this case, both slots are of the C
type ``unsigned short`` which is designated by the predefined class
``<C-unsigned-short>``. The information about the C type ``unsigned
short`` carried by this designator class allows the compiler to compute
the size, alignment, and layout of the struct. The compiler records the
struct’s size and alignment and associates them with ``<Point>``. The
designator class ``<Point>`` can then be used in the definition of other
types, functions, and variables. For example, we could describe

.. code-block:: c

    typedef struct {
      Point start;
      Point end;
    } LineSegment;

like this:

.. code-block:: dylan

    define C-struct <LineSegment>
      slot start :: <Point>;
      slot end :: <Point>;
    end C-struct;

As well as acting as a static information carrier for use in other FFI
definitions, a designator class can also be instantiable, in which case
Dylan uses an instance of the designator class to represent an object of
the C type it designates when that object is passed from the "C world"
to the "Dylan world".

*Note:* Only classes that designate C pointer types can be instantiated
in this way. Instances of C’s fundamental numeric value types like ``int``,
``char``, and ``double`` are just converted to an equivalent Dylan object
with the same value. The ``<Point>`` class is not an instantiable class in
Dylan because there is nothing in Dylan that corresponds to a C struct.
However, the C-FFI does provide a Dylan representation of a pointer to a
C struct.

To illustrate, here is an example interaction involving a C struct
containing some pointer-typed slots and some slots whose types are
fundamental numeric types:

.. code-block:: dylan

    define C-struct <Example>
      slot count :: <C-int>;
      slot statistic :: <C-double>;
      slot data :: <C-char*>;
      slot next :: <Example*>;
      pointer-type-name :: <Example*>;
    end C-struct;

This example defines the two designator types ``<Example>`` and
``<Example*>`` ; the slots *count* and *statistic* have fundamental
numeric types while *data* and *next* have pointer types. The getter and
setter methods for the slots are defined for instances of ``<Example*>``.

Suppose there is a function *current-example* that returns an
initialized ``<Example*>`` struct. The following transactions illustrate
what you get when you read the slots of the structure it returns:

::

    ? define variable example = current-example();
    // Defined example
    ? example.count;
    4
    ? instance?(example.count, <integer>);
    #t
    ? example.statistic;
    10.5
    ? instance?(example.statistic, <float>);
    #t

The interactions above show that if we access structure slots that were
defined as being of one of C’s fundamental numeric types, we get a Dylan
number of the equivalent value. The same thing happens if an imported C
function returns a fundamental numeric type: a Dylan number with the
same value appears in Dylan. Similarly, when setting slots in structs
expecting numbers or passing objects out to C functions expecting
numeric arguments, you should provide a Dylan number, and the C-FFI will
convert it automatically to its C equivalent.

::

    ? example.data;
    {<C-char> pointer #xff5e00}
    ? instance?(example.data, <C-char*>);
    #t
    ? example.next;
    {<Example> pointer #xff5f00}
    ? instance?(example.next, <Example*>);
    #t

The interactions above show that accessing structure slots with a
pointer type results in an instance of the Dylan class that designates
that type. Again, the same thing happens if an imported C function
returns a pointer type: an instance of the corresponding designator
class is created. Similarly, when setting slots in structs expecting
pointers or passing objects out to C functions expecting pointer
arguments, you should provide an instance of the Dylan designator class
for that pointer type, and the C-FFI will convert it automatically to
the raw C pointer value.

Later sections describe all the macros available for defining C types
and the functions available for manipulating them.

C functions in Dylan
--------------------

When you use the interface definition language to describe a C function
to the Dylan compiler, the compiler generates a new Dylan function. This
*wrapper function* accepts Dylan arguments and returns Dylan results. It
converts each of its arguments from a Dylan object to a corresponding C
value before calling the C function it wraps. The C-FFI converts any
results that the C function returns into Dylan objects before returning
them to the caller.

In order for Dylan to be able to call into C correctly, C functions must
be described to Dylan in the same detail a C header file would provide a
calling C program. Specifically, for every function we must provide the
C name and the type of its arguments and results. As with struct
definitions, these types are indicated by naming the designator classes
corresponding to the C types involved in the C-FFI description of the C
function.

The following is an example of defining and using wrapper functions.
Suppose we have the following ``extern`` C function declaration:

.. code-block:: c

    extern double cos (double angle);

We describe C functions to Dylan using the C-FFI macro :macro:`define
C-function`:

.. code-block:: dylan

    define C-function C-cos
      parameter angle :: <C-double>;
      result cos :: <C-double>;
      c-name: "cos"
    end C-function;

The name appearing immediately after the :macro:`define C-function` is
the name we want to give to the Dylan variable to which our wrapper
function will be bound. We call it *C-cos*. We also give the actual C
name of the function we want to wrap as the value of the keyword
*c-name:*.

Once we have compiled the definition — and assuming the compiled version
of the C library implementing *cos* has been linked in with the Dylan
application — we can call the wrapper function just like any other Dylan
function:

::

    ? C-cos(0.0);
    1.0

As we noted above, when values are passed back and forth between Dylan
and C, the C-FFI performs automatic conversions. In this case, the type
of the parameter and the result are both fundamental numeric types which
means that the C-FFI will accept and return Dylan floats, converting to
and from raw C floats as necessary.

As well as making C functions available to Dylan code, the C-FFI allows
us to make Dylan functions available to call from C code. We do this by
defining a *C-callable* wrapper function. A C-callable wrapper is a
Dylan function that a C program can call. The C-callable wrapper has a C
calling convention. When a C program calls a C-callable wrapper, the
C-FFI performs the necessary data conversions and then invokes a Dylan
function.

You can pass C-callable wrappers into C code for use as callbacks. You
can also give them names visible in C, so that C clients of Dylan code
can call into Dylan directly by invoking a named function.

The argument and result conversions performed by C-callable wrappers are
just like those done within Dylan wrapper functions. The macro that
defines C-callable wrappers is called :macro:`define C-callable-wrapper`
and we describe it in detail later. For now, consider the following
simple example. Suppose we have a C ``extern`` function declaration
*AddDouble*:

.. code-block:: c

    extern double AddDouble (double x, double y);

This function is intended to return the sum of two ``double`` values.
Instead of implementing the function in C, we can implement it in Dylan
using Dylan’s generic function ``+``. All we need to do is define a
C-callable wrapper for ``+``, as follows:

.. code-block:: dylan

    define C-callable-wrapper AddDoubleObject of \+
      parameter x :: <C-double>;
      parameter y :: <C-double>;
      c-name: "AddDouble";
    end C-callable-wrapper;

We can now call ``AddDouble`` in C. Our wrapper will be invoked, the C
arguments will be converted and passed to Dylan’s + generic function,
and then the result of the computation will be converted and passed back
to C:

.. code-block:: c

    {
      extern double AddDouble (double x, double y);
      double result;

      result = AddDouble(1.0, 2.0);
    }

The C-FFI binds the Dylan variable *AddDoubleObject* to a Dylan object
representing the function pointer of the C-callable wrapper. This
reference allows the C-callable wrapper to be passed to a C function
expecting a callback argument.

C variables in Dylan
--------------------

When you use the interface definition language to describe a C variable
to the Dylan compiler, the compiler generates new Dylan getter and
setter functions for reading and setting the variable’s value from
Dylan. If the variable is constant, it defines a getter function only.

The getter function converts the C value to a Dylan value before
returning it according to the variable’s declared type. Similarly, the
setter function converts its argument, as Dylan value, into a C value
before setting the C variable. These conversions happen according to the
same rules that apply to other C-Dylan world transition points, such as
argument passing or structure slot access.

In order for Dylan to be able to access a C variable correctly, we must
describe the variable to Dylan in the same detail that a C header file
would give to a C program that uses it. Specifically, we must provide
the C name and the type of the variable. As with struct and function
definitions, we indicate C types by naming the appropriate Dylan
designator classes.

Here is an example of defining and using C variables. Suppose we have
the following ``extern`` C variable declaration:

.. code-block:: c

    extern double mean;

We describe C variables to Dylan using the C-FFI macro :macro:`define
C-variable`:

.. code-block:: dylan

    define C-variable C-mean :: <C-double>
      c-name: "mean";
    end C-variable;

The name immediately after the :macro:`define C-variable` is the name of
the Dylan variable to which the getter for our C variable will be bound.
In this case it is *C-mean*.

We give the C name of the variable as the value of the keyword *c-name:*.
Once we have compiled the definition — and assuming the compiled
version of the C library defining *mean* has been linked in with the
Dylan application — we can call the getter function just like any other
Dylan function:

::

    ? C-mean();
    1.5

By default, the C-FFI also defines a setter function for the variable.
The setter name uses Dylan’s convention of appending "-setter" to the
getter name.

::

    ? C-mean() := 0.0;
    0.0
    ? C-mean();
    0.0

As described above, when values are passed back and forth between Dylan
and C, the C-FFI performs automatic conversions. In this case, the type
of the variable is a fundamental numeric type which means that the C-FFI
accepts and returns Dylan floats, converting to and from raw C floats as
necessary.

.. note:: We could achieve the same result by using the :macro:`define
   C-address` macro, which defines a constant that is a pointer to the
   storage allocated for the C variable.

Notes on Linking
================

When using C-FFI, you will typically need to link in an existing library
or framework.

:doc:`LID files <../lid>` provide many options for controlling
the compilation and linking of the project depending on what exactly
is required.

Linking against a Library
-------------------------

This can be done in a :doc:`LID file <../lid>` using the :ref:`C-Libraries <lid-c-libraries>`
keyword.  This supports both static and shared libraries. It also supports
specifying a search path.  For example::

    C-Libraries: -lGL

Linking against a Mac OS X Framework
------------------------------------

Just as with a regular shared library, the :ref:`C-Libraries <lid-c-libraries>`
keyword in a :doc:`LID file <../lid>`.  For example::

    C-Libraries: -framework OpenGL

Using pkg-config
----------------

Libraries that use "pkg-config" are slightly more complicated to work with in
that they require using the :ref:`Jam-Includes <lid-jam-includes>` keyword and
an additional file within the project.  The GTK+ bindings provide multiple
examples of this.

In the LID file, you would include the additional Jam file::

    Jam-Includes: gtk-dylan.jam

And you would provide the additional Jam file::

    {
      local _dll = [ FDLLName $(image) ] ;
      LINKLIBS on $(_dll) += `pkg-config --libs gtk+-3.0` ;
      CCFLAGS += `pkg-config --cflags gtk+-3.0` ;
    }

Tracing FFI Calls
=================

When working with the C-FFI, it is very useful to be able to trace what
is happening, what is getting called, what the arguments are, and what
the return value is. To aid in this, C-FFI enables the programmer to
enable tracing.

To do this, you will need to exclude the default implementation of
tracing when importing the ``c-ffi`` module and define your own
implementation.

In your ``library.dylan``, you would change your module declaration:

.. code-block:: dylan

    use c-ffi;

to:

.. code-block:: dylan

    use c-ffi, exclude: {
      $trace-ffi-calls,
      log-entry,
      log-exit };
    use format-out;

Note that we've used the ``format-out`` module from the ``io``
library in addition to the exclusion.

After doing that, you can define your own implementation of
tracing such that your implementation is in the same lexical
scope as the ``C-function`` definitions that you want to trace:

.. code-block:: dylan

    define constant $trace-ffi-calls = #t;

    define inline-only function log-entry(c-function-name, #rest args) => ();
      format-out("entering %s %=", c-function-name, args);
    end;
    define inline-only function log-exit(c-function-name, #rest results) => ();
      format-out(" => %=\n", results);
    end;

    define C-function ...

When this is run, you will see output like::

    entering nn_socket #[1, 16] => #[0]
    entering nn_socket #[1, 16] => #[1]
    entering nn_bind #[0, "inproc://a"] => #[1]
    entering nn_connect #[1, "inproc://a"] => #[1]
    entering nn_send #[1, #x007D0AAC, 3, 0] => #[3]
    entering nn_recv #[0, #x007D0AE4, 3, 0] => #[3]
    entering nn_close #[0] => #[0]
    entering nn_close #[1] => #[0]

Terminology
===========

For the rest of this chapter, we adopt the following terminology,
hopefully not too inconsistent with common C terminology:

-  *Base type* Basic units of data storage (C’s variously sized
   integers, characters, and floating point numbers) and aggregate
   records (structs and unions).
-  *Derived type*. A type based on some other type (C’s pointer, array,
   and function types).
-  *Fundamental numeric type*. One of C’s integer or floating point types.
   This does not include pointer types, structure types, or union types.

Basic options in C-FFI macros
=============================

The defining macros of the C-FFI share a consistent core set of options
which are worth describing here:

-  A *c-name* argument. Every defining form allows you to specify the
   corresponding C entity through the keyword *c-name:*. It is optional
   in some forms but required in others. You can define types that have
   no named opposite number in C, and the c-name option is always
   optional in type definitions. On the other hand, you must always name
   an imported C function or variable so that Dylan knows the correct
   name from the compiled C library to link with.

   In general, any C entity you can declare in C using ``extern`` can only be
   found by the C-FFI if you pass a *c-name* argument to the corresponding
   C-FFI definition.

-  A *pointer-type-name* argument. All the type-defining forms allow you
   to name the type for a pointer to the type being defined. This is
   normally specified throughout the *pointer-type-name:* keyword
   option.

Designator classes
==================

As `Overview`_ explained, the C-FFI defines some Dylan classes to designate
C types and to describe how they are passed to and from Dylan. These
*designator classes* carry with them static information about the C type
they designate.

The C-FFI library provides an initial set of designator classes
corresponding to C’s fundamental types, as well as macros for generating
designator classes corresponding to C’s pointer types and for extending
the translation between C data and Dylan objects.

Designator classes that correspond to fundamental numeric types are not
instantiable. When you pass a numeric value to Dylan from C, the C-FFI
simply generates a Dylan number with the same value. Similarly, a Dylan
number passed to C is converted to a C number of the appropriate type
and value.

Each of the fundamental designator classes indicate a conversion to or
from a unique Dylan class. The conversions that take place are described
in detail in the documentation for each designator class.

The main reasons for this design are increased efficiency, simplified
implementation, and added convenience when working with numeric values.
The designator classes for the numeric types could have been made
instantiable and placed beneath the appropriate number protocol classes
in Dylan, but these extra representations in such a fundamental area
could cause problems for Dylan compilers. In addition, to make these
instantiable designator classes convenient to work with, the C-FFI would
also have to define methods on the standard arithmetic and comparison
operators. It is simpler to represent these fundamental types with
existing Dylan objects.

However, the designator classes that correspond to pointer types *are*
instantiable. When you pass a pointer from C to Dylan, the C-FFI
constructs an instance of the appropriate designator class that contains
the raw address. A wrapped pointer like this can be passed out to some C
code that is expecting a compatible pointer — the C-FFI extracts the raw
address before handing it to C code. The documentation for the abstract
class :class:`<C-pointer>` describes the compatibility rules for pointer
types.

This feature of pointer designator classes allows Dylan code to be typed
to a specific kind of pointer. For example, you can define methods that
specialize on different kinds of pointer on the same generic function.

Designator type properties
--------------------------

To understand how designator classes work, it is useful to know about
their properties. A few of these properties are accessible
programmatically, but others are implicit and only really exist in the
compiler. Some of the properties may be empty.

A *referenced type* is the designator type to which a pointer refers. A
designator’s *referenced-type* only has a value for subtypes of
:class:`<C-statically-typed-pointer>`. Programs can access the
referenced type through the function *referenced-type*.

A designator class’s *pointer-type* only has a value for each of
those types that has a pointer designator type that refers to it. Most
of the constructs that define a new designator type also define a
pointer-type for that designator. Many of the macros that define
designators accept a *pointer-type-name:* keyword to bind the
*pointer-type* of the defined designator to a given variable. The
pointer-type is not programmatically available because it may not have
been defined. You can assure that there is a pointer-type for a
particular designator by using the macro :macro:`define c-pointer-type`.

A designator class’s *import type* and *export type* are instantiable
Dylan types that describe the Dylan instantiation of a designator class
when it is used in a position that *imports* values from C, or *exports*
values to C.

Nearly all of the C-FFI’s designators have import and export types that
are equivalent. Some, such as :class:`<C-string>`, have different import
and export types because it is possible to pass a pointer to a Dylan
object to C directly without creating a C pointer object, or copying the
underlying data, but when importing a string from C it is not practical
to copy the contents and create a Dylan string. By default, the import
and export types for any subtype of :class:`<C-pointer>` are the class
itself. You can override this by defining a new subclass with the macro
:macro:`define C-mapped-subtype`.

You can define a designator’s *import-function* and *export-function* by
using the macro :macro:`define c-mapped-subtype`. These functions are
merely the procedural specifications for translating the C data to Dylan
and back. The *import* and *export* functions are inherited when you
define a subclass for a designator.

Designator class basics
-----------------------

.. class:: <C-value>
   :sealed:
   :abstract:

   :description:

     The abstract superclass of all designator classes. It is a subclass
     of :drm:`<object>`. It has neither an *export-type* nor an
     *import-type*, so you cannot use it when designating a transition
     between C and Dylan.

.. class:: <C-void>
   :sealed:
   :abstract:

   :description:

     The abstract superclass of all designator classes. It is a subclass
     of :class:`<C-value>`. It has neither an *export-type* nor an
     *import-type*, so you cannot use it when designating a transition
     between C and Dylan.

     This class is only useful in that it is the *referenced-type* for
     :class:`<C-void*>`.

.. function:: size-of

   Takes a designator class and returns the size of the C type that the
   class designates.

   :signature: size-of *designator-class* => *size*

   :parameter designator-class: A subclass of :class:`<C-value>`.
   :value size: An instance of :drm:`<integer>`.

   :description:

     Takes a designator class and returns the size of the C type that
     the class designates.

     The ``size-of`` function can be applied to any designator class.
     However, if it is applied to :class:`<C-void>`, :class:`<C-value>`,
     or :class:`<C-struct>`, it returns zero. It corresponds to C’s
     ``sizeof`` operator and returns an integer, *size*, in the same
     units as ``sizeof`` does on the target platform. It can be useful
     when allocating a C object whose declared size is not accurate and
     has to be adjusted manually.

.. function:: alignment-of

   Takes a designator class and returns the alignment of the C type that
   the class designates.

   :signature: alignment-of *designator-class* => *alignment*

   :parameter designator-class: A subclass of :class:`<C-value>`.
   :value alignment: An instance of :drm:`<integer>`.

   :description:

     Takes a designator class and returns the alignment of the C type
     that the class designates. The ``alignment-of`` function can be
     applied to any designator class. It returns the alignment as an
     integer, in the same units as :func:`size-of` does.

Fundamental numeric type designator classes
-------------------------------------------

This section describes the pre-defined designator classes for
fundamental C numeric types. On page `Designator
classes`_ we saw that none of these designator types
are instantiable: a number on one side of the interface is converted to
a number on the other side with the same value.

There are some additional details to note about integer representations.
Because Dylan’s integer representations do not match C’s exactly, for
each of the C integer types there are three designator classes that can
be used to translate Dylan representations to that C integer. The
categories are *plain*, *unsafe*, and *raw* integers.

*Plain* integer designators — of which the class ``<C-unsigned-short>`` is
an example — translate C integer values to instances of :drm:`<integer>`. If
the integer being translated is too big for the destination, the C-FFI
signals an error. There are two ways this can happen.

-  On export, the C-FFI signals an error if the Dylan value has more
   significant bits than the C integer.

This can happen if, for example, the designator is ``<C-unsigned-short>``,
and the Dylan value is negative, or if *unsigned* *short* on that
platform is 16 bits wide, but the Dylan integer has more than 16
significant bits. The check will be omitted if the compiler can
determine that no Dylan value outside the safe range can reach there.
This can be done using a limited integer type.

-  On import into Dylan, the C-FFI signals an error if it cannot
   represent the C value using a Dylan :drm:`<integer>`.

This can happen with any C integer type that is more than 30 bits wide.
The size of a Dylan :drm:`<integer>` depends on the particular platform, but
it is guaranteed to be at least 30 bits in length.

The C-FFI never signals an error for the *unsafe* designator classes —
of which the class ``<C-unsafe-unsigned-short>`` is an example — but if
the destination is too small for the value, the most significant bits of
the value are chopped off to fit into the destination. Because there is
no checking, using the unsafe designator classes brings a very small
performance improvement, but nonetheless you should not use them unless
you are certain you will not lose any bits.

*Raw* designator classes — of which the class ``<C-raw-unsigned-int>`` is
an example — represent the integer on the Dylan side as a
``<machine-word>``. An instance of ``<machine-word>`` is guaranteed to have
enough bits to represent any C ``long`` value, or any C ``void*`` value.
Note that a ``<machine-word>`` value may still have more significant bits
than some C integer types, and so the C-FFI may still signal an overflow
error if the ``<machine-word>`` value, interpreted as indicated by the
designator, has more significant bits than may be held in the indicated
C type.

`The integer designator classes and their mappings.`_ shows all raw, plain,
and unsafe integer designator types exported from the C-FFI module.

.. table:: The integer designator classes and their mappings.
   :name: The integer designator classes and their mappings.

   +-------------------------------+--------------------+--------------------+
   | Designator name               | C type             | Dylan type(s)      |
   +===============================+====================+====================+
   | ``<C-int>``                   | ``int``            | :drm:`<integer>`   |
   +-------------------------------+--------------------+--------------------+
   | ``<C-raw-int>``               | ``int``            | ``<machine-word>`` |
   +-------------------------------+--------------------+--------------------+
   | ``<C-unsafe-int>``            | ``int``            | :drm:`<integer>`   |
   +-------------------------------+--------------------+--------------------+
   | ``<C-raw-signed-int>``        | ``signed int``     | ``<machine-word>`` |
   +-------------------------------+--------------------+--------------------+
   | ``<C-unsafe-signed int>``     | ``signed int``     | :drm:`<integer>`   |
   +-------------------------------+--------------------+--------------------+
   | ``<C-signed-int>``            | ``signed int``     | :drm:`<integer>`   |
   +-------------------------------+--------------------+--------------------+
   | ``<C-raw-unsigned-int>``      | ``unsigned int``   | ``<machine-word>`` |
   +-------------------------------+--------------------+--------------------+
   | ``<C-unsafe-unsigned-int>``   | ``unsigned int``   | :drm:`<integer>`   |
   +-------------------------------+--------------------+--------------------+
   | ``<C-unsigned-int>``          | ``unsigned int``   | :drm:`<integer>`   |
   +-------------------------------+--------------------+--------------------+
   | ``<C-unsigned-long>``         | ``unsigned long``  | :drm:`<integer>`   |
   +-------------------------------+--------------------+--------------------+
   | ``<C-signed-long>``           | ``signed long``    | :drm:`<integer>`   |
   +-------------------------------+--------------------+--------------------+
   | ``<C-unsafe-unsigned-long>``  | ``unsigned long``  | :drm:`<integer>`   |
   +-------------------------------+--------------------+--------------------+
   | ``<C-unsafe-signed-long>``    | ``signed long``    | :drm:`<integer>`   |
   +-------------------------------+--------------------+--------------------+
   | ``<C-raw-unsigned-long>``     | ``unsigned long``  | ``<machine-word>`` |
   +-------------------------------+--------------------+--------------------+
   | ``<C-raw-signed-long>``       | ``signed long``    | ``<machine-word>`` |
   +-------------------------------+--------------------+--------------------+
   | ``<C-unsigned-short>``        | ``unsigned short`` | :drm:`<integer>`   |
   +-------------------------------+--------------------+--------------------+
   | ``<C-signed-short>``          | ``signed short``   | :drm:`<integer>`   |
   +-------------------------------+--------------------+--------------------+
   | ``<C-unsafe-unsigned-short>`` | ``unsigned short`` | :drm:`<integer>`   |
   +-------------------------------+--------------------+--------------------+
   | ``<C-unsafe-signed-short>``   | ``signed short``   | :drm:`<integer>`   |
   +-------------------------------+--------------------+--------------------+
   | ``<C-raw-unsigned-short>``    | ``unsigned short`` | ``<machine-word>`` |
   +-------------------------------+--------------------+--------------------+
   | ``<C-raw-signed-short>``      | ``signed short``   | ``<machine-word>`` |
   +-------------------------------+--------------------+--------------------+
   | ``<C-unsigned-char>``         | ``unsigned char``  | :drm:`<integer>`   |
   +-------------------------------+--------------------+--------------------+
   | ``<C-signed-char>``           | ``signed char``    | :drm:`<integer>`   |
   +-------------------------------+--------------------+--------------------+
   | ``<C-unsafe-unsigned-char>``  | ``unsigned char``  | :drm:`<integer>`   |
   +-------------------------------+--------------------+--------------------+
   | ``<C-unsafe-signed-char>``    | ``signed char``    | :drm:`<integer>`   |
   +-------------------------------+--------------------+--------------------+
   | ``<C-raw-unsigned-char>``     | ``unsigned char``  | ``<machine-word>`` |
   +-------------------------------+--------------------+--------------------+
   | ``<C-raw-signed-char>``       | ``signed char``    | ``<machine-word>`` |
   +-------------------------------+--------------------+--------------------+
   | ``<C-char>``                  | ``char``           | :drm:`<integer>`   |
   +-------------------------------+--------------------+--------------------+
   | ``<C-unsafe-char>``           | ``char``           | :drm:`<integer>`   |
   +-------------------------------+--------------------+--------------------+
   | ``<C-raw-char>``              | ``char``           | ``<machine-word>`` |
   +-------------------------------+--------------------+--------------------+

For each of the fundamental integer designator types, *<C-* *xxx* *>*,
there is also a type designating pointers to that type called *<C-*
*xxx* *\*>*. In addition, the C-FFI defines methods for
:gf:`pointer-value` and :gf:`pointer-value-setter`, with appropriate
translation behavior for each of the types designating pointers to the
fundamental integer designator types.

.. class:: <C-number>
   :sealed:
   :abstract:

   :superclasses: :class:`<C-value>`

   :description:

     The abstract superclass of all classes that designate a fundamental
     numeric C type.

.. class:: <C-float>
   :sealed:
   :abstract:

   :description:

     The class of C floating point values.

.. class:: <C-double>
   :sealed:
   :abstract:

   :description:

     The class of C double-precision values.

Pointer designator classes and related functions
------------------------------------------------

This section describes the pre-defined classes that designate C pointer
types. Subclasses of the abstract classes documented here are
instantiable, and C pointers are represented in Dylan by instances of
these classes.

.. note:: Pointer designator classes are defined for all the designator
   classes in `The integer designator classes and their
   mappings.`_, but are not listed here. To form the name
   of the pointer designator class for a particular designator class,
   append a ``*`` to the part of the name enclosed in angle brackets. Thus
   for ``<C-int>`` the pointer designator class is ``<C-int*>``.

.. class:: <C-pointer>
   :primary:
   :open:
   :abstract:

   :superclasses: :class:`<C-value>`

   :description:

     The abstract superclass of all classes that designate a C pointer
     type. Instances of concrete subclasses of :class:`<C-pointer>`
     encapsulate a raw C address. The make methods on subclasses of
     :class:`<C-pointer>` accept the keyword argument ``address:``,
     which must be a Dylan :drm:`<integer>` or ``<machine-word>``
     representation of the C address.

.. function:: pointer-address

   Recovers the address from an instance of :class:`<C-pointer>` and returns it as
   a Dylan ``<machine-word>``.

   :signature: pointer-address *C-pointer* => *address*

   :parameter c-pointer: An instance of :class:`<C-pointer>`.
   :value address: An instance of ``<machine-word>``.

   :description:

     Recovers the address from an instance of :class:`<C-pointer>` and
     returns it as a Dylan ``<machine-word>``.

.. function:: pointer-cast

   Converts a pointer from one pointer type to another.

   :signature: pointer-cast *pointer-designator-class* *C-pointer* => *new-C-pointer*

   :parameter pointer-designator-class: A subclass of :class:`<C-pointer>`.
   :parameter c-pointer: An instance of :class:`<C-pointer>`.
   :value new-c-pointer: An instance of :class:`<C-pointer>`.

   :description:

     Converts a pointer from one pointer type to another. The new
     pointer will have the same address as the old pointer.

.. function:: null-pointer

   Returns a null pointer whose type is given by the
   pointer-designator-class.

   :signature: null-pointer *pointer-designator-class* => *null-pointer*

   :parameter pointer-designator-class: A subclass of :class:`<C-pointer>`.
   :parameter c-pointer: An instance of :class:`<C-pointer>`.
   :value new-c-pointer:

   :description:

     Returns a null pointer whose type is given by
     *pointer-designator-class*. Note that different calls to
     ``null-pointer`` may return the same object.

.. function:: null-pointer?

   Returns true if a pointer is null

   :signature: null-pointer? *C-pointer* => *boolean*

   :parameter c-pointer: An instance of :class:`<C-pointer>`.
   :value boolean: An instance of :drm:`<boolean>`.

   :description:

     Returns ``#t`` if a pointer is null and ``#f`` otherwise.

.. class:: <C-void\*>
   :open:
   :concrete:

   :superclasses: :class:`<C-pointer>`

   :description:

     The class designating C’s ``void*`` pointer type. No
     :gf:`pointer-value` methods are defined on this class.

.. class:: <C-statically-typed-pointer>
   :open:
   :abstract:

   :superclasses: :class:`<C-pointer>`

   :description:

     The abstract superclass of all classes designating a C pointer type
     for a non-*void* base.

.. macro:: define C-pointer-type
   :defining:

   Defines a constant bound to a pointer class designating pointers to a
   designator class name.

   :macrocall:
     .. code-block:: dylan

       define C-pointer-type *pointer-class-name* => *designator-class-name*

   :parameter pointer-class-name: A Dylan variable name.
   :value designator-class: A Dylan name.

   :description:

     Defines a constant bound to a pointer class designating pointers to
     *designator-class-name*. Note that the pointer type may already
     exist. The class defined will be open, abstract and instantiable.
     Objects returned by ``make(*pointer-class-name*)`` will be
     instances of a sealed concrete subclass of *pointer-class-name*.

.. function:: referenced-type

   Returns the class designating the contents type of the designated C
   pointer type.

   :signature: referenced-type *pointer-designator-class* => *designator-class*

   :parameter pointer-designator-class: A subclass of :class:`<C-pointer>`.
   :value designator-class: A subclass of :class:`<C-value>`.

   :description:

     Returns the class designating the contents type of the C pointer
     type designated by pointer-designator-class. The same designator
     class is returned whenever *referenced-type* is called with the
     same argument.

.. function:: c-type-cast

   Converts a value to a value of a specified type, according to the
   semantics of a C type cast.

   :signature: c-type-cast *type* *value* => *value*

   :parameter type: See Description.
   :parameter value: An instance of :drm:`<object>`.
   :value value: An instance of :drm:`<object>`.

   :description:

     Returns the value of the second argument, converted to the type
     specified by the first argument, in accordance with the semantics of a C
     type cast. This is convenient to use when translating C code to Dylan.
     It may also be helpful for converting a value to the form required by a
     C-function wrapper argument.

     The first argument can be either a C type designator or one of the Dylan
     classes :drm:`<boolean>`, :drm:`<character>`, ``<machine-word>``, or any subclass
     of :drm:`<number>`. For a C type designator, the value is converted to the
     Dylan class which it maps to. *<C-* [*un* ]*signed-short>* and *<C-*
     [*un* ]*signed-char>* truncate the value as well as ensuring that it is
     an :drm:`<integer>`.

   :example:

     For example, with a function declared in C as

     .. code-block:: c

       Foo(long x);

     and called as

     .. code-block:: c

       Foo((long) p);

     if the Dylan declaration is

     .. code-block:: dylan

       define C-function Foo
         parameter x :: <C-both-long>;
         c-name: "Foo";
       end;

     then the equivalent call will be:

     .. code-block:: dylan

       Foo(c-type-cast(<C-both-long>, p));

     which will ensure that the C semantics are preserved without needing to
     analyze exactly what the type cast is doing.

     The functions :gf:`pointer-value` and :gf:`pointer-value-setter`
     perform the primitive Dylan-to-C and C-to-Dylan conversions as
     documented with the designator class of the pointer’s contents type
     (see `The integer designator classes and their mappings.`_). The
     C-FFI signals an error if it cannot convert the object you attempt
     to store in the pointer to a compatible type.

     These two functions are part of a protocol for extending the C type
     conversions. You can define new methods for :gf:`pointer-value` and
     :gf:`pointer-value-setter` for types defined by :macro:`define
     C-subtype` that are subtypes of :class:`<C-pointer>`.

.. generic-function:: pointer-value
   :open:

   Dereferences a c-typed pointer using its encapsulated raw C address.

   :signature: pointer-value *C-typed-pointer* #key *index* => *object*

   :parameter c-typed-pointer: An instance of :class:`<C-statically-typed-pointer>`.
   :value object: An instance of :drm:`<object>`.

   :description:

     Dereferences *c-typed-pointer* using its encapsulated raw C
     address, and returns a Dylan object representing the object at that
     address. If you supply index, the pointer is treated as a pointer
     to an array, and the function returns the appropriate element
     indexed by the correct unit size.

     It is an error if *C-typed-pointer* does not point to a valid
     address or is a null pointer.

   See also

   - :gf:`pointer-value-setter`.

.. generic-function:: pointer-value-setter
   :open:

   Allows you to set pointer values.

   :signature: pointer-value-setter *new-value* *C-typed-pointer* #key *index* => *new-value*

   :parameter new-value: An instance of :drm:`<object>`.
   :parameter c-typed-pointer: An instance of :class:`<C-statically-typed-pointer>`.
   :parameter #key index: An instance of :drm:`<integer>`.
   :value new-value: An instance of :drm:`<object>`.

   :description:

     Allows you to set pointer values. If you supply index, the pointer is
     treated as a pointer to an array, and the function returns the
     appropriate element indexed by the correct unit size.

     It is an error if *C-typed-pointer* does not point to a valid address or
     is a null pointer.

.. generic-function:: pointer-value-address
   :open:

   Returns a pointer of the same type as a C-typed pointer that points
   to the object offset into the C-typed pointer.

   :signature: pointer-value-address *C-typed-pointer* #key *index* => *object*

   :parameter c-typed-pointer: An instance of :class:`<C-statically-typed-pointer>`.
   :parameter #key index: An instance of :drm:`<integer>`.
   :value object: An instance of :drm:`<object>`.

   :description:

     Returns a pointer of the same type as *C-typed-pointer* that points
     to the *index* *th* object offset into *C-typed-pointer*. The
     following expression is guaranteed to be true:

   :example:

     .. code-block:: dylan

       pointer-value(*C-typed-pointer*, index: i)
        = pointer-value (pointer-value-address(*C-typed-pointer*, index: i))

.. method:: element
   :specializer: <C-statically-typed-pointer>

   Dereferences a c-typed pointer using its encapsulated raw C address.

   :signature: element *C-typed-pointer* *index* => *object*

   :parameter c-typed-pointer: An instance of :class:`<C-statically-typed-pointer>`.
   :value object: An instance of :drm:`<object>`.

   :description:

     Dereferences a c-typed pointer using its encapsulated raw C
     address. Synonymous with a call to :gf:`pointer-value` that
     includes the optional index. Thus it does the same thing as:

     .. code-block:: dylan

        pointer-value(*C-statically-typed-pointer*, index: *index*)

.. method:: element-setter
   :specializer: <C-statically-typed-pointer>

   Allows you to set pointer values.

   :signature: element-setter *new* *C-typed-pointer* *index* => *object*

   :parameter c-typed-pointer: An instance of :class:`<C-statically-typed-pointer>`.
   :parameter index: An instance of :drm:`<integer>`.
   :value object: An instance of :drm:`<object>`.

   :description:

     Synonymous with a call to :gf:`pointer-value-setter` that includes
     the optional index. Thus it does the same thing as:

     .. code-block:: dylan

        pointer-value-setter(*new*, *C-statically-typed-pointer*, index:
                             *index*)

.. method:: =
   :specializer: <C-pointer>

   Returns ``#t`` if two pointers are equal.

   :signature: = *C-pointer-1* *C-pointer-2* => *boolean*

   :parameter c-pointer-1: An instance of :class:`<C-pointer>`.
   :parameter c-pointer-2: An instance of :class:`<C-pointer>`.
   :value boolean: An instance of :drm:`<boolean>`.

   :description:

     Returns ``#t`` if two pointers are equal. This is equivalent to:

     .. code-block:: dylan

        (pointer-address(*C-pointer-1*) = pointer-address(*C-pointer-2*))

     Note that operations corresponding to C pointer arithmetic are not
     defined on :class:`<C-pointer>`. If pointer arithmetic operations are
     required, use :gf:`pointer-value` with an ``index:`` argument.

   See also

   - :gf:`pointer-value`.

.. method:: <
   :specializer: <C-pointer>

   Returns ``#t`` if the second argument is less than the first.

   :signature: < *C-pointer-1* *C-pointer-2* => *boolean*

   :parameter c-pointer-1: An instance of :class:`<C-pointer>`.
   :parameter c-pointer-2: An instance of :class:`<C-pointer>`.
   :value boolean: An instance of :drm:`<boolean>`.

   :description:

     Returns ``#t`` if the second argument is less than the first. This
     allows pointer comparison operations to be performed on instances
     of :class:`<C-pointer>`.

     Note that operations corresponding to C pointer arithmetic are not
     defined on :class:`<C-pointer>`. If pointer arithmetic operations
     are required, use :gf:`pointer-value` with an ``index:`` argument.

   See also

   - :gf:`pointer-value`.

The following functions comprise the conceptual foundation on which the
pointer accessing protocol is based. In the signatures of these
functions, *byte-index* is in terms of address units (typically bytes)
and *scaled-index* is scaled by the size of the units involved. In the
setters, *new* is the new value to which the value in the pointed-at
location will be set. These functions can be used to deference any
general instance of :class:`<C-pointer>`.

C-char-at
^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-char-at *C-pointer* #key *byte-index* *scaled-index* => *machine-word*

C-char-at-setter
^^^^^^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-char-at-setter *new* *C-pointer* #key *byte-index* *scaled-index*
      => *machine-word*

C-signed-char-at
^^^^^^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-signed-char-at *C-pointer* #key *byte-index* *scaled-index* =>
      *machine-word*

C-signed-char-at-setter
^^^^^^^^^^^^^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-signed-char-at-setter *new* *C-pointer* #key *byte-index*
      *scaled-index* => *machine-word*

C-unsigned-char-at
^^^^^^^^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-unsigned-char-at *C-pointer* #key *byte-index* *scaled-index* =>
      *machine-word*

C-unsigned-char-at-setter
^^^^^^^^^^^^^^^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-unsigned-char-at-setter *new* *C-pointer* #key *byte-index*
      *scaled-index* => *machine-word*

C-unsigned-short-at
^^^^^^^^^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-unsigned-short-at *C-pointer* #key *byte-index* *scaled-index*
      => *machine-word*

C-unsigned-short-at-setter
^^^^^^^^^^^^^^^^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-unsigned-short-at-setter *new* *C-pointer* #key *byte-index*
      *scaled-index* => *machine-word*

C-signed-short-at
^^^^^^^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-signed-short-at *C-pointer* #key *byte-index* *scaled-index* =>
      *machine-word*

C-signed-short-at-setter
^^^^^^^^^^^^^^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-signed-short-at-setter *new* *C-pointer* #key *byte-index*
      *scaled-index* => *machine-word*

C-short-at
^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-short-at *C-pointer* #key *byte-index* *scaled-index* =>
      *machine-word*

C-short-at-setter
^^^^^^^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-short-at-setter *new* *C-pointer* #key *byte-index* *scaled-index* =>
      *machine-word*

C-unsigned-long-at
^^^^^^^^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-unsigned-long-at *C-pointer* #key *byte-index* *scaled-index* =>
      *machine-word*

C-unsigned-long-at-setter
^^^^^^^^^^^^^^^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-unsigned-long-at-setter *new* *C-pointer* #key *byte-index*
      *scaled-index* => *machine-word*

C-signed-long-at
^^^^^^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-signed-long-at *C-pointer* #key *byte-index* *scaled-index* =>
      *machine-word*

C-signed-long-at-setter
^^^^^^^^^^^^^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-signed-long-at-setter *new* *C-pointer* #key *byte-index*
      *scaled-index* => *machine-word*

C-long-at
^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-long-at *C-pointer* #key *byte-index* *scaled-index* => *machine-word*

C-long-at-setter
^^^^^^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-long-at-setter *new* *C-pointer* #key *byte-index* *scaled-index* =>
      *machine-word*

C-unsigned-int-at
^^^^^^^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-unsigned-int-at *C-pointer* #key *byte-index* *scaled-index* =>
      *machine-word*

C-unsigned-int-at-setter
^^^^^^^^^^^^^^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-unsigned-int-at-setter *new* *C-pointer* #key *byte-index*
      *scaled-index* => *machine-word*

C-signed-int-at
^^^^^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-signed-int-at *C-pointer* #key *byte-index* *scaled-index* =>
      *machine-word*

C-signed-int-at-setter
^^^^^^^^^^^^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-signed-int-at-setter *new* *C-pointer* #key *byte-index*
      *scaled-index* => *machine-word*

C-int-at
^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-int-at *C-pointer* #key *byte-index* *scaled-index* => *machine-word*

C-int-at-setter
^^^^^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-int-at-setter *new* *C-pointer* #key *byte-index* *scaled-index* =>
      *machine-word*

C-double-at
^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-double-at *C-pointer* #key *byte-index* *scaled-index* => *float*

C-double-at-setter
^^^^^^^^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-double-at-setter *new-double-float* *C-pointer* #key *byte-index*
      *scaled-index* => *float*

C-float-at
^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-float-at *C-pointer* #key *byte-index* *scaled-index* => *float*

C-float-at-setter
^^^^^^^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-float-at-setter *new-single-float* *C-pointer* #key *byte-index*
      *scaled-index* => *float*

C-pointer-at
^^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-pointer-at *C-pointer* #key *byte-index* *scaled-index* => *C-pointer*

C-pointer-at-setter
^^^^^^^^^^^^^^^^^^^

Function

Signature

.. code-block:: dylan

    C-pointer-at-setter *new* *C-pointer* #key *byte-index* *scaled-index*
      => *C-pointer*

Structure types
---------------

.. class:: <C-struct>
   :open:
   :abstract:

   :description:

     The abstract superclass of all classes designating a C struct type.
     It is a subclass of :class:`<C-value>`. It is a subclass of
     :class:`<C-value>`. You can describe new struct types using the
     :macro:`define C-struct` macro.

     Classes designating C structs are not instantiable. Where a slot,
     array element, function parameter or function result is typed as a
     struct value, pointers to that struct type are accepted and
     returned.

Union types
-----------

.. class:: <C-union>
   :open:
   :abstract:

   :description:

     The abstract superclass of all classes designating a C union type.
     It is a subclass of :class:`<C-value>`. You can describe new union
     types with the macro :macro:`define C-union`. Classes designating C
     unions are not instantiable. Where a slot, array element, function
     parameter or function result is typed as a union value, pointers to
     that union type are accepted and returned.

Notes on C type macros
----------------------

The C-FFI’s C interface description language does not model all of the
ways of defining new types in C, but all C types should be expressible
in it. As a simplification, we do not support anonymous base types in
the C interface description language. If a structure or union field has
an in-line type definition in C, that definition must be extracted and
given a name in order for it to be used. For example, the following C
struct

.. code-block:: c

    struct something {
      char *name;
      long flags;
      union {
        long int_val;
        char *string_val;
      } val;
    }

can be described with these definitions:

.. code-block:: dylan

    define C-union <anonymous-union-1>
      slot int-val :: <C-long>;
      slot string-val :: <C-string>;
    end C-union;

    define C-struct <anonymous-struct-1>
      slot name :: <C-string>;
      slot flags :: <C-long>;
      slot val :: <anonymous-union-1>;
    end C-struct;

The slots of these ex-inline types must be accessed through a chain of
accesses, for example ``o.val.string-val``.

Defining types
==============

This section covers the definition macros that create Dylan designators
for C types, structs and unions.

Defining specialized versions of designator classes
---------------------------------------------------

.. macro:: define C-subtype
   :defining:

   Defines a specialized designator class for a C type based on an
   existing designator class for that type.

   :macrocall:
     .. code-block:: dylan

       define [*modifiers* *] C-subtype name (superclasses)
         [*slot-spec* ; ...] [;]
         [*type-options* ] [;]
       end [C-subtype] [*name* ]

   :parameter modifiers: The same as the modifiers allowed in :drm:`define class <class>`.
   :parameter name: A Dylan variable name.
   :parameter superclasses: A list of Dylan names.
   :parameter slot-spec: Same syntax as a slot definition in ``define class``.
   :parameter type-options: A property list.

   :description:

     Defines a specialized designator class for a C type based on an
     existing designator class for that type. It does this by defining a
     subclass of the original designator class, and is a simple wrapper
     around :drm:`define class <class>` from which it takes its syntax. The
     superclasses, slot-specs, and *modifiers* are passed on to ``define
     class`` unchanged. In effect, it expands to:

     .. code-block:: dylan

       define class *name* (*superclasses*)
         *slot-spec* ; ...
       end class;

     In terms of C, ``define C-subtype`` can be thought of as
     implementing a strongly typed version of ``typedef`` because a new
     designator class is generated that Dylan’s type system can
     distinguish from the designator class on which it was based. As
     well as inheriting from an existing designator class, other Dylan
     classes can be mixed in too.

     The optional *type-options* must be a property list. The *c-name:*
     keyword is recognized, allowing the original C name of the type
     designated by the class to be documented. The *pointer-type-name:*
     keyword option can be used to name the designator for pointers to
     *name*.

   :example:

     Some example C declarations:

     .. code-block:: c

       typedef void *Handle;

       typedef Handle WindowHandle;
       typedef Handle StreamHandle;

       extern WindowHandle CurrentWindow (void);

       extern StreamHandle CurrentStream (void);

     Example FFI definitions:

     .. code-block:: dylan

       define C-subtype <Handle> (<C-void*>) end;

       define C-subtype <WindowHandle> (<Handle>) end;
       define C-subtype <StreamHandle> (<Handle>) end;

       define C-function CurrentWindow
         result value :: <WindowHandle>;
         c-name: "CurrentWindow";
       end C-function;

       define C-function CurrentStream
         result value :: <StreamHandle>;
         c-name: "CurrentStream";
       end C-function;

     Example transactions:

     .. code-block:: dylan

       ? <C-void*> == <WindowHandle> | <WindowHandle> == <StreamHandle>;
       #f

       ? define variable *cw* = CurrentWindow();
       // Defined *cw*

       ? *cw*
       {<WindowHandle> #xff5400}

       ? define variable *cs* = CurrentStream();
       // Defined *cs*

       ? *cs*
       {<StreamHandle> #xff6400}

       ? instance?(*cs*, <WindowHandle>) | instance?(*cw*, <StreamHandle>);
       #f

     The following example uses the ability to specify extra superclasses to
     place a type beneath an abstract class.

     Example C declarations:

     .. code-block:: c

       struct _Matrix {
         int rank;
         int *dimensions;
         int *values;
       };
       typedef struct _Matrix *Matrix;

       extern Matrix MatrixAdd (Matrix m, Matrix n);

     Example FFI definitions:

     .. code-block:: dylan

       define C-struct <_Matrix-struct>
         slot rank :: <C-int>;
         slot dimensions :: <C-int*>;
         slot values :: <C-int*>;
         pointer-type-name: <_Matrix-struct*>;
       end C-struct;

       define C-subtype <Matrix> (<_Matrix-struct*>, <number>) end;

       define C-function MatrixAdd
         parameter m :: <Matrix>;
         parameter n :: <Matrix>;
         result value :: <Matrix>;
         c-name: "MatrixAdd";
       end C-function;

       define method \+ (m1 :: <Matrix>, m2 :: <Matrix>) =>
           (r :: <Matrix>)
         MatrixAdd(m1, m2)
       end method;

Defining specialized designator classes
---------------------------------------

.. macro:: define C-mapped-subtype
   :defining:

   Allows you to define a name to which to bind a pointer designator.

   :macrocall:
     .. code-block:: dylan

       define *modifiers* C-mapped-subtype *type-name* (*superclasses*)
         [map *high-level-type* [, import-function: *import-fun* ]
         [, export-function: *export-fun* ];]
         [import-map *high-level-type*,
         import-function: *import-function* ;]
         [export-map *high-level-type*,
         export-function: *export-function* ;]
         [type-options]
       end

   :parameter modifiers: The same as the modifiers allowed in :drm:`define-class <class>`.
   :parameter type-name: A Dylan variable name.
   :parameter superclasses: A list of Dylan names.
   :parameter high-level-type: An instance of :drm:`<function>`.
   :parameter import-fun: An instance of :drm:`<function>`.
   :parameter export-fun: An instance of :drm:`<function>`.
   :parameter type-options: A property list.

   :description:

     Allows you to define a name to which to bind a pointer designator.

     The *modifiers* may be *sealed* or *open*. (The default is
     *sealed*.) Their effect on the class defined is the same as the
     similar modifiers on an ordinary class.

     The possible combinations are, a map clause, an import-map clause,
     an export-map clause, or both an import-map and an export-map
     clause. Any other combinations are illegal.

     The *import-map* clause specifies that a type conversion takes
     place when *type-name* is used as a designator for values imported
     from C into Dylan. The conversion is accomplished by calling the
     *import-function* on the imported value. This call is automatically
     inserted into function wrappers, structure member getters,
     pointer-value dereference functions and so on by the C-FFI. The
     *high-level-type* is used as the Dylan type specifier for the
     appropriate parameter or result in any wrapper function or
     *c-struct* accessor which uses the defined class. The *export-map*
     clause specifies a similar type conversion for exported values. The
     *high-level-type* must in either case name an instantiable Dylan
     type.

     .. code-block:: dylan

         map <type-c>;

     is equivalent to:

     .. code-block:: dylan

         import-map <type-c>;
         export-map <type-c>;

     The import and export functions are monadic functions whose single
     argument is the appropriate low-level value for export functions and the
     appropriate Dylan type for import functions. Any mapped subtype which
     specifies an import-map must specify an *import-function*. Any mapped
     subtype which specifies an export-map must specify an *export-function*.

     Map boolean example:

     bool-header.h:

     .. code-block:: c

         typedef int bool;

         bool bool_function (bool b);
         void bool_pointer_function (bool *b);

         //eof

     .. code-block:: dylan

         Module: my-module

         define C-mapped-subtype <bool> (<C-int>)
           map <boolean>,
           export-function:
             method (v :: <boolean>) => (result :: <integer>)
               as(<integer>, if(v) 1 else 0 end if) end,
           import-function:
             method (v :: <integer>) => (result :: <boolean>)
               ~zero?(v) end;
         end;

         //end module

     Mapped string example: an alternate version of C-string which
     automatically converts instances of :drm:`<byte-string>` to instances
     of ``<C-example-string>`` on export.

     string-header.h

     .. code-block:: c

         typedef char *string;

         string string-filter(string s);
         void string-modifier(string *s);

         //eof

     .. code-block:: dylan

         module: my-module

         define C-mapped-subtype <C-example-string> (<C-char*>, <string>)
           export-map type-union(<byte-string>,
                                 <C-example-string>),
           export-function: c-string-exporter;
         end;

         define method c-string-exporter
             (s :: <byte-string>) => (result :: <C-char*>)
           as(<C-example-string>, s)
         end;

         define method c-string-exporter
             (s :: <C-example-string>) => (result :: <C-example-string>)
           s
         end;

         //end module

     It is possible to define an ordinary subtype of a mapped supertype.
     The mapping characteristic of the subtype is inherited from the
     supertype. It is also possible to define a mapped subtype of a
     mapped supertype. When the subtype and supertype both specify an
     export function, the export functions of the subtype and the
     supertype are composed with the subtype’s export function applied
     to the result of the supertype’s export function. Import functions
     of a mapped subtype and supertype are similarly composed. Mapping
     characteristics are inherited from the supertype where the subtype
     does not define them. (You can think of this as composition with
     identity when either the supertype or subtype fails to specify an
     import or export function.) This shadowing is only useful when
     import and export maps are defined separately. Here is an example
     of a mapped subtypes which adds an import map to the mapped version
     of ``<C-example-string>`` defined above.

     .. code-block:: dylan

       define C-mapped-subtype <other-string>
           (<C-example-string>)
         import-map <byte-string>,
         import-function: method (v :: <byte-string>) =>
               (result :: <C-example-string>)
             as(<C-example-string>, v)
           end method;
       end;

     The import signature is :drm:`<byte-string>`. The export signature is
     inherited from ``<C-example-string>`` ``type-union(<byte-string>,
     <C-example-string>)``. For a example involving composition of
     mapped types consider the following (hypothetical) definitions of
     ``<C-raw-int>``, ``<C-mapped-int>`` and ``<bool>``. The
     ``<C-raw-int>`` class is a primitive which returns and accepts
     instances of ``<machine-word>``. The ``<C-mapped-int>`` class is a
     mapped subtype which converts the instances of ``<machine-word>``
     to instances of :drm:`<integer>`. The ``<bool>`` class is a mapped
     subtype of ``<C-mapped-int>`` which converts to and from
     :drm:`<boolean>`.

     .. code-block:: dylan

         define C-mapped-subtype <C-mapped-int> (<C-raw-int>)
           map <boolean>,
           export-function:
             method (v :: <integer>) => (result :: <machine-word>)
               as(<machine-word>, v) end,
           import-function:
             method (v :: <machine-word>) => (result :: <integer>)
               as(<integer>, v) end;
         end;

         define C-mapped-subtype <bool> (<C-mapped-int>)
           map <boolean>,
           export-function:
             method (v :: <boolean>) => (result :: <integer>)
               if(v) 1 else 0 end if) end,
           import-function:
             method (v :: <integer>) => (result :: <boolean>)
               ~zero?(v) end;
         end;

Describing structure types
--------------------------

.. macro:: define C-struct
   :defining:

   Describes C’s aggregate structures.

   :macrocall:
     .. code-block:: dylan

       define C-struct *name*
         [*slot-spec* ; ...] [;]
         [*type-options* ] [;]
       end [C-struct] [*name* ]

   :parameter name: A Dylan variable name.
   :parameter slot-spec:
   :parameter type-options: A property list.

   :description:

     Describes C’s aggregate structures. The name is defined to be a
     designator class encapsulating the value of a structure, not a
     pointer to the structure. This is significant because many of the
     protocols associated with structures work only on pointers to
     structures — pointers to structures being the most common form and
     the form closest to Dylan’s object model. The new designator class
     is defined to be a subclass of :class:`<C-struct>`.

     Once defined, a structure-designating class is most likely to be
     used as the basis for a pointer type definition in terms of which
     most further transactions will take place. Structure-designating
     classes are abstract and cannot have direct instances. Accessor
     methods defined for the slots of the structure are specialized on
     the structure designator’s pointer-type. However, the class itself
     may be needed to specify an in-line structure in another structure,
     union, or array, or a value-passed structure argument or result in
     a C function.

     A slot-spec has the following syntax:

     .. code-block:: dylan

       [*slot-adjective* ] slot *getter-name* :: *c-type* #key *setter*
         *address-getter* *c-name length* *width*

     The *slot-adjective* can be *constant*,  *array* or *bitfield*. The
     *array* slot adjective indicates that the slot is repeated and the
     *dimensions* option is used to indicate how many repetitions are
     defined, and how it is accessed. The *bitfield* slot adjective
     indicates that the slot is really a bitfield. If *bitfield* is
     given then the *width* option must also be given. The *c-type*
     given for a *bitfield* slot must be an integer designator. The
     *c-type* for a *bitfield* slot indicates how the value is
     interpreted in Dylan by the slot accessor. A slot may not be
     specified as both an *array* and a *bitfield*. If *constant*
     is specified, then no setter is generated. The *constant*
     adjective can be supplied for *array* and *bitfield* slots.

     The getter-name keyword specifies the name of the Dylan function to
     which the getter method for the structure slot will be added. The
     specializer of the getter method’s single argument will be a
     designator indicating a pointer to the struct’s name.

     The c-type specifies the field’s C type, and must be a designator
     class. Unlike Dylan slot specifications, the type declaration here
     is not optional.

     The optional setter keyword specifies the generic function to which
     the setter method for the structure slot will be added. It defaults
     to getter-name*-setter*. No setter method is defined if the
     *setter* option is ``#f``. If the *constant* keyword is supplied, no
     *setter* option should be supplied.

     The optional *address-getter* specifies the name of a function that
     can be used to return a pointer to the data in the member. It must
     return a ``<C-pointer>`` object that points to a C type. No
     *address-getter* is defined by default.

     You can use the *dimensions* keyword only if you used the *array*
     slot adjective. This *dimensions* value can be either a list of
     integers or a single integer. The accessor for an array slot is
     defined to take an extra integer parameter for each dimension
     given.

     You can use the *width* keyword option only if you used the
     *bitfield* adjective.

     The optional c-name keyword allows you to document the original C
     name of the slot.

     The type-options clause is a property list allowing you to specify
     properties of the type as a whole. It accepts the optional keyword
     c-name:, allowing you to document the original C name of the struct
     to be documented. The optional keyword *pointer-type-name:* is also
     accepted. When given, the name is bound to the struct pointer type
     on which the accessors are defined.

     The type option *pack:* *n* indicates that the struct has the
     packing semantics of Microsoft’s ``#pragma pack(*n*)``.

   :example:

     Example C declaration:

     .. code-block:: dylan

       struct Point {
         unsigned short x;
         unsigned short y;
       };

       Point *OnePoint(); /* Returns a pointer to a Point */
       Point *PointArray(); /* Returns a Point array */

     Example FFI definition:

     .. code-block:: dylan

       define C-struct <Point>
         slot x :: <C-unsigned-short>;
         slot y :: <C-unsigned-short>;
         pointer-type-name: <Point*>;
       end C-struct;

       define C-function one-point
         result point :: <Point*>;
         c-name: "OnePoint";
       end C-function;

       define C-function point-array
         result array :: <Point*>;
         c-name: "PointArray";
       end C-function;

     Example transactions::

       ? define variable p = one-point();
       // Defined p.

       ? values(p.x, p.y);
       100
       50

       ? define variable array = point-array();
       // Defined array.

       ? array[5].object-class; // implicit conversion to
       // the pointer type
       {<Point> pointer #xff5e00}

       ? begin array[5].x := 10; array[5].y := 20 end;
       20

       ? values(array[5].x, array[5].y)
       10
       20

Describing union types
----------------------

.. macro:: define C-union
   :defining:

   Describes C union types to the *c-ffi*.

   :macrocall:
     .. code-block:: dylan

       define C-union *name*
         [*slot-spec* ; ...] [;]
         [*type-options* ] [;]
       end [C-union] [*name* ]

   :parameter name: A Dylan variable name.
   :parameter slot-spec:
   :parameter type-options: A property list.

   :description:

     Describes C union types to the C-FFI. The syntax for the macro and
     its use are similar to :macro:`define c-struct` except that bitfield
     slots are not allowed. The designator created by the macro is a
     subclass of :class:`<c-union>`.

     Each of the slots in a union is laid out in memory on top of one another
     just as in C’s ``union`` construct.

   :example:

     Example C declaration:

     .. code-block:: c

       union Num {
         int int_value;
         double double_value;
       };

       Num *OneNum(); /* Returns a pointer to a Num */

       Num *NumArray(); /* Returns a Num array */

     Example FFI definition:

     .. code-block:: dylan

       define C-union <Num>
         slot int-value :: <C-int>;
         slot double-value :: <C-double>;
         pointer-type-name: <Num*>;
       end C-union;

       define C-function one-num
         result num :: <Num*>;
         c-name: "OneNum";
       end C-function;

       define C-function num-array
         result array :: <Num*>;
         c-name: "NumArray";
       end C-function;

     Example transactions::

       ? define variable n = one-num();
       // Defined n.

       ? values(p.int-value, p.double-value);
       154541
       92832.e23 // or something

       ? define variable array = num-array();
       // Defined array.

       ? array[5].object-class; // implicit conversion to
       // the pointer type
       {<Num> pointer #xff5e00}

       ? array[5].int-value := 0;
       0

       ? array[5].double-value;
       11232e-12 // or a different something

Functions
=========

This section describes the C FFI macros that allow C functions to be
made available to Dylan and Dylan functions available to C.

Function types
--------------

This section describes classes that designate C function types and how
to construct them.

.. class:: <C-function-pointer>
   :open:
   :abstract:

   :description:

     The superclass of all classes that designate a C function type. It
     is a subclass of :class:`<C-pointer>`. The Dylan variable bound by
     :macro:`define c-callable` is of this type.

Describing C functions to Dylan
-------------------------------

.. macro:: define C-function
   :defining:

   Describes a C function to the *c-ffi*.

   :macrocall:
     .. code-block:: dylan

       define C-function *name*
         [*parameter-spec*; ...]
         [*result-spec*;]
         [*function-option*, ...;]
       end [C-function] [*name*]

   :parameter name: A Dylan variable name.
   :parameter parameter-spec:
   :parameter result-spec:
   :parameter function-option: A property list.

   :description:

     Describes a C function to the C-FFI. In order for a C function to
     be called correctly by Dylan, the same information about the
     function must be given as is needed by C callers, typically
     provided by ``extern`` declarations for the function in a C header
     file: the function’s name and the types of its parameters and
     results.

     The result of processing a ``define C-function`` definition is a
     Dylan function which is bound to name. This function takes Dylan
     objects as arguments, converting them to their C representations
     according to the types declared for the parameters of the C
     function before calling the C function with them. If the C function
     returns results, these results are converted to Dylan
     representations according to the declared types of those results
     before being returned to the Dylan caller of the function. By
     default the function created is a raw method, not a generic
     function. A generic function method can defined by using the
     *generic-function-method:* option.

     Either the *c-name:* function option must be supplied, or the
     *indirect:* option must be supplied with a value other than ``#f``,
     but not both.

     A parameter-spec has the following syntax::

       [*adjectives*] parameter name :: *c-type* #key *c-name*

     If no parameters are specified, the C function is taken to have no
     arguments.

     The adjectives can be either *output*, *input*, or both. The
     calling discipline is specified by the *input* and *output*
     adjectives.

     By itself, *input* indicates that the argument is passed into the
     function by value. This option is the default and is used primarily
     to document the code. There is a parameter to the generated Dylan
     function corresponding to each *input* parameter of the C function.

     The *output* adjective specifies that the argument value to the C
     function is used to identify a location into which an extra result
     of the C function will be stored. There is no parameter in the
     generated Dylan function corresponding to an *output* parameter of
     the C function. The C-FFI generates a location for the extra return
     value itself and passes it to the C function. When the C function
     returns, the value in the location is accessed and returned as an
     extra result from the Dylan function. The C-FFI allocates space for
     the output parameter’s referenced type, passes a pointer to the
     allocated space, and returns :gf:`pointer-value` of that pointer. A
     struct or union type may not be used as an output parameter.

     Example of *output* parameter definition:

     .. code-block:: dylan

       define C-function mix-it-up
         output parameter out1 :: <some-struct*>;
         output parameter out2 :: <C-int*>;
         result value :: <C-int>;
         c-name: "mix_it_up";
       end C-function mix-it-up;

     Example transaction::

       ? mix-it-up();
       1
       {<some-struct> pointer #xfefe770}
       42

     If both *input* and *output* are supplied, they specify that the
     argument value to the C function is used to identify a location
     from which a value is accessed and into which an extra result value
     is placed by the C function. There is a parameter to the generated
     Dylan function corresponding to each *input* *output* parameter of
     the C function that is specialized as the union of the export type
     of the referenced type of the type given for the parameter in
     ``define c-function``, and ``#f``. When the C function returns, the
     value in the location is accessed and returned as an extra result
     from the Dylan function. If an *input* *output* parameter is passed
     as ``#f`` from Dylan then a ``NULL`` pointer is passed to the C
     function, and the extra value returned by the Dylan function will
     be ``#f``.

     Example of *input* *output* parameter definition:

     .. code-block:: dylan

       define C-function mix-it-up
         input output parameter inout :: <C-int*>;
         result value :: <C-int>;
         c-name: "mix_it_up";
       end C-function mix-it-up;

     Example transaction::

       ? mix-it-up(7);
       1
       14

     Note that neither *output* nor *input* *output* affects the
     declared type of an argument: it must have the same type it has in
     C and so, because it represents a location, must be a pointer type.

     A result-spec has the following syntax::

       result [name :: c-type]

     If no *result* is specified, the Dylan function does not return a
     value for the C result, and the C function is expected to have a
     return type of *void*.

     Each *function-option* is a keyword–value pair. The
     *generic-function-method:* option may be either ``#t`` or ``#f``,
     indicating whether to add a method to the generic function name or
     to bind a bare constant method directly to name. The default value
     for *generic-function-method:* is ``#f``. The option *C-modifiers:*
     can be used to specify platform dependent modifiers for the C
     function being called. For example, on Windows, use *C-modifiers:*
     ``"__stdcall"`` if the C function to be called is defined to be a
     ``__stdcall`` function.

     The *c-name:* option is used to specify the name of the C function
     as it is defined in the object or shared library file. The *c-name*
     must be a constant string.

     The *indirect:* ``#t`` option defines a function that accepts a C
     function pointer as its first argument and calls the function given
     with the signature described by the parameters and result given. In
     this case the Dylan function defined accepts one more argument than
     if *c-name* was given. The type specified for the first parameter
     of the Dylan function is :class:`<c-function-pointer>`. One of
     *c-name* or *indirect:* ``#t`` must be supplied, but not both.

     Example C declarations:

     .. code-block:: c

       /* Compute the length of a string */
       int strlen(char *string);

       /* Set the given locations to values,
          returning an error code */
       int fill_locations(int *loc1, int *loc2);

       /* Read at most as far as indicated in max_then_read,
          updating it to contain how much was actually read */
       void read_stuff(int *max_then_read);

     Example FFI definitions:

     .. code-block:: dylan

       define C-function strlen
         parameter string :: <C-char*>;
         result value :: <C-int>;
         c-name: "strlen";
       end C-function;

       define C-function fill-locations
         output parameter loc1 :: <C-int*>;
         output parameter loc2 :: <C-int*>;
         result return-code :: <C-int>;
         c_name: "fill_locations";
       end C-function;

       define C-function read-stuff
         input output parameter :: <C-int*>;
         c-name: "read_stuff";
       end C-function;

     Example transactions:

     ::

       ? strlen($my-c-string);
       44
       ? fill-locations();
       0
       101 // extra output value
       102 // extra output value
       ? read-stuff(100);
       50 // extra output value

     In effect, a ``define C-function`` such as:

     .. code-block:: dylan

       define C-function foo
         parameter string :: <C-char*>;
         parameter count :: <C-int>;
         result value :: <C-int>;
         c-name: "foo";
       end C-function;

     expands into something like:

     .. code-block:: dylan

       define constant foo =
         method (string, count)
           let c-string = %as-c-representation(<C-char*>,
                                               string);
           let c-count = %as-c-representation(<C-int>, count);
           let c-result = %call-c-function("foo", c-string,
                                           c-count);
           %as-dylan-representation(<C-int>, c-result);
         end;

     with the declared type.

Describing Dylan functions for use by C
---------------------------------------

.. macro:: define C-callable-wrapper
   :defining:

   Makes a Dylan function callable from C by describing a C contract for
   the function.

   :macrocall:
     .. code-block:: dylan

       define C-callable-wrapper [*dylan-rep-name* ]
        of *dylan-function*
         [*parameter-spec* ; ...] [;]
         [*result-spec* ] [;]
         [*function-options* ][;]
       end [C-callable-wrapper]

   :parameter dylan-rep-name: A Dylan variable name.
   :parameter dylan-function: An instance of :drm:`<function>`.
   :parameter parameter-spec:
   :parameter result-spec:
   :parameter function-options: A property list.

   :description:

     Makes a Dylan function callable from C by describing a C contract
     for the function. In order to generate a correct C-callable
     function wrapper, the same information about the function must be
     given as would be needed by C callers, typically provided by
     ``extern`` declarations for the function in a C header file: the
     types of its parameters and results.

     The result of processing a ``define C-callable-wrapper`` definition
     is a function with a C entry point with the contract described.
     This function takes C values as arguments, converting them to Dylan
     representations according to the types declared for the parameters
     of the C function before calling the Dylan function with them. If
     the C function was described as returning results, the results of
     the call to the Dylan function are converted to C representations
     according to the declared types of those results before being
     returned to the C caller of the function.

     The *dylan-function* is a Dylan function that accepts the correct
     number of parameters, and is called by the C callable wrapper.

     The function-options are a property list. This list may contain a
     string value for the c-name keyword. If a c-name is specified, that
     name is made visible to C as the name of the generated *C-callable
     wrapper* function. Given a compatible ``extern`` declaration, this
     allows C code to call Dylan code simply by invoking a named
     function. The *export:* option takes the values ``#t`` or ``#f``
     and indicates whether the c-name for the generated
     *C-callable-wrapper* function is to be exported from the library’s
     *.dll*. ``#t`` means it is exported, ``#f`` means it is not. The
     default is #f. The *c-modifiers:* option is the same as in the
     *c-function* macro, except that the modifiers apply to the C
     function wrapper which is generated. See :macro:`define C-function`.

     If dylan-rep-name is specified, it is bound to an instance of a
     function-pointer designator class identifying the generated
     C-callable wrapper function. You can pass this pointer to C code
     for use as, for example, a callback.

     A parameter-spec has the following syntax::

         [*adjectives* ] parameter name :: *c-type* #key *c-name*

     If no parameters are specified, the C function is taken to have no
     arguments.

     An adjective can be *input*, *output*, or both. The calling
     discipline is specified by the *input* and *output* adjectives.

     If a parameter is *output*, the corresponding parameter is not
     passed to the Dylan function, but the Dylan function is expected to
     return an extra value that is placed in the location pointed to by
     the parameter. When the pointer is NULL, the extra value from the
     Dylan function is ignored. The type designated for the parameter
     must be a pointer type.

     If a parameter is both *input* and *output*, the parameter must be
     a pointer type, and the value accepted by the Dylan function is the
     result. The functions pointer-value and pointer-value-setter
     perform the primitive Dylan-to-C and C-to-Dylan conversions as
     documented with the designator class of the pointer’s contents type
     (see Table 1.1). The C-FFI signals an error if it cannot convert
     the object you attempt to store in the pointer to a compatible
     type on that pointer. The Dylan function is expected to return
     an extra value which is placed into the location specified by the
     pointer passed to the C function. If the pointer passed to the C
     function is ``NULL``, then the value passed to the Dylan function
     will be ``#f``, and the extra value returned will be ignored.

     There is currently no way to define a C-callable function that
     accepts a variable number of arguments.

     A result-spec has the following syntax::

         result name :: *c-type*

     If no *result* is specified, the C function defined does not return
     a value. It is defined as what in C terminology is known as a
     *void* function.

   :example:

     Example C declarations:

     .. code-block:: c

       /* Compute the length of a string */
       int strlen(char *string);

       /* Set the given locations to values, returning an
          error code */
       int fill_locations(int *loc1, int* loc2);

       /* Read at most as far as indicated in max_then_read,
          updating it to contain how much was actually read */
       void read_stuff(int *max_then_read);

     Example FFI definitions:

     .. code-block:: dylan

       define method dylan-strlen (string) => (length) ... end;

       define C-callable-wrapper of dylan-strlen
         parameter string :: <C-char*>;
         result value :: <C-int>;
         c-name: "strlen";
       end C-function;

       define method dylan-fill-locations ()
        => (return-code :: <integer>,
            val1 :: <integer>,
            val2 :: <integer>)
         ...
       end;

       define C-callable-wrapper of dylan-fill-locations
         output parameter loc1 :: <C-int*>;
         output parameter loc2 :: <C-int*>;
         result return-code :: <C-int>;
         c-name: "fill_locations";
       end C-function;

       define method dylan-read-stuff (max :: <integer>) =>
         (read :: <integer) ...
       end;

       define C-callable-wrapper of dylan-read-stuff
         input output parameter max-then-read :: <C-int*>;
         c-name: "read_stuff";
       end C-function;

     Example C calls:

     .. code-block:: c

       {
         int length, *loc1, *loc2, max_then_read;
         length = strlen("ABC");
         fill_locations(loc1, loc2);

         max_then_read = 100

         read_stuff(&max_then_read);
       }

     In effect, a ``define C-callable-wrapper`` such as:

     .. code-block:: dylan

       define C-callable-wrapper of foo
         parameter string :: <C-char*>;
         parameter count :: <C-int>;
         result value :: <C-int>;
         c-name: "foo";
       end C-function;

     expands into something like:

     .. code-block:: dylan

       %c-callable-function "foo" (c-string, c-count)
         let dylan-string
           = %as-dylan-representation(<C-char*>, c-string);
         let dylan-count
           = %as-dylan-representation(<C-int>, c-count);
         let dylan-result
           = foo(dylan-string, dylan-count);
         %as-c-representation(<C-int>, dylan-result);
       end;

     where the *%* functions perform the primitive conversions between
     Dylan and C representations, checking that their arguments are
     compatible with the declared type.

     Callback example:

     .. code-block:: dylan

       define C-function c-sort
         parameter strings :: <C-string*>;
         parameter compare :: <C-function-pointer>;
         result sorted-strings :: <C-string*>;
         c-name: "sort";
       end C-function;

       define C-callable-wrapper callback-for-< of \<
         parameter string1 :: <C-string>;
         parameter string2 :: <C-string>;
         result int :: <C-int>;
       end C-callable-wrapper;

       ? callback-for-<
       {function pointer #xff6e00}

       ? c-sort(some-c-strings, callback-for-<);
       {<C-string> array}

Variables
---------

This section covers describing and accessing C variables.

.. macro:: define C-variable
   :defining:

   Describes C variables to the *c-ffi*.

   :macrocall:
     .. code-block:: dylan

       define C-variable *getter-name* :: *c-type*
         #key *setter* *c-name* import: *boolean*
       end [C-variable]

   :parameter getter-name: A Dylan variable name.
   :parameter c-type: A Dylan name.
   :parameter setter: ``#f`` or a Dylan variable name.
   :parameter c-name: A string constant.
   :parameter import: ``#f`` or ``#t``.

   :description:

     Describes C variables to the C-FFI. It defines a getter and setter
     function for accessing the variable’s value. The c-name keyword
     argument is required and gives the C name of the variable to be
     accessed. The *setter* keyword allows you to specify the name of
     the setter function, or if a setter function is to be defined at
     all. If *setter* is ``#f``, no setter function will be defined.

     The *import:* option indicates if the C variable must be imported
     from another *.dll* or not. ``#t`` indicates it is in another
     *.dll* and must be imported, ``#f`` means that it is not to be
     imported. Whether the variable has to be imported from another
     *.dll* or not is determined by which Dylan project the C source
     files are part of. If they are in the same project as the
     *C-variable* definition then the value of "import:" should be
     ``#f`` as the definition and variable will be linked into the same
     *.dll*. If the definition is in a different project from the C
     source files then they will be in separate *.dll* s and *import:*
     needs to be ``#t``. The default value is ``#f``.

     For integer, float, or pointer-typed C variables the representation
     is clear and unambiguous. For C struct or union typed variables the
     translation is not so simple. A C union or struct has no direct
     representation in Dylan. You may only have a reference to the C
     object in Dylan through a :class:`<c-pointer>` object. For this
     reason, ``define c-variable`` is not permitted for variables with C
     aggregate types. Use :macro:`define C-address` for those variables.

   :example:

     ::

       ? define C-variable process-count :: <C-int>,
         c-name: "process_count" end;

       ? process-count();
       57

       ? process-count() := 0;
       0

       ? process-count();
       0

       ? define C-variable machine-name-1 :: <C-char*>,
         c-name: "MachineName";
       end;

       ? machine-name-1();
       #{<C-char*> #xaaabc00}

     In C and other static languages what is known as a variable is a
     named allocation of memory. To access a global C variable from
     Dylan it is occasionally necessary to get a handle to the location
     where that variable is kept. The :macro:`define C-address` macro
     can be used for this purpose.

.. macro:: define C-address
   :defining:

   Defines a Dylan constant binding that is a :class:`<C-pointer>` to
   the location of a C global variable.

   :macrocall:
     .. code-block:: dylan

       define C-address *name* :: *pointer-designator-type*
         #key *c-name* import: *boolean*
       end [C-address] [*name* ]

   :parameter name: A Dylan variable name.
   :parameter pointer-designator-type:
   :parameter c-name: A string constant.
   :parameter import: ``#f`` or ``#t``.

   :description:

     Defines a Dylan constant binding, *name*, that is a
     :class:`<C-pointer>` which points to the location of the C global
     variable *c-name*.

     *Pointer-designator-type* must be the type of the constant to be
     defined, and a subtype of ``<C-pointer>``.

     The *import:* option indicates if the C address must be imported
     from another *.dll* or not. ``#t`` indicates it is in another
     *.dll* and must be imported, ``#f`` means that it is not to be
     imported. Whether the variable has to be imported from another
     *.dll* or not is determined by which Dylan project the C source
     files are part of. If they are in the same project as the
     *C-address* definition then the value of "import:" should be ``#f``
     as the definition and variable will be linked into the same *.dll*.
     If the definition is in a different project from the C source files
     then they will be in separate *.dll* s and *import:* needs to be
     ``#t``. The default value is ``#f``.

Allocating and deallocating C storage
=====================================

C objects can be allocated by calling *make* on an associated wrapper
class or by allocating them on the stack using the macro
:macro:`with-stack-structure`.

The C component of a *make* -allocated object is not deallocated by
default when the Dylan designator object is reclaimed by the garbage
collector, so we provide a manual means of freeing this storage with the
function *destroy*.

.. method:: make
   :specializer: subclass(<C-pointer>)

   Allocates a C object on the heap.

   :signature: make *subclass(<c-pointer>)* #key *allocator* *element-count* *extra-bytes* *address* => *C-pointer*

   :parameter subclass: A subclass of :class:`<C-pointer>`.
   :parameter #key allocator: An instance of :drm:`<function>`.
   :parameter #key element-count: An instance of :drm:`<integer>`.
   :parameter #key extra-bytes: An instance of :drm:`<integer>`.
   :parameter #key address: An instance of :drm:`<integer>` or ``<machine-word>``.
   :value c-pointer: An instance of type :class:`<c-pointer>` pointing to the object.

   :description:

     Allocates a C object on the heap, using whatever standard C
     allocation function is in use on the target platform (typically
     ``malloc``) to allocate the storage. This method is applicable to
     subclasses of :class:`<C-pointer>` and returns an instance of its
     argument class.

     If the address option is provided, no new storage is allocated, but
     instead, a new pointer with the given machine word address is
     returned.

     The *allocator* argument should be a Dylan function that can serve
     as an allocator. It must accept a single integer argument — the
     number of bytes to be allocated — and return a Dylan
     ``<machine-word>`` that represents the address of the memory it
     allocated.

     The amount of storage allocated by default is the result of::

         size-of(*pointer-wrapper-class*.referenced-type)

     If a positive integer is passed as an extra-bytes option, that
     number of extra bytes is also allocated.

     If a positive integer is passed as a element-count option, space
     for element-count copies of the referenced type is allocated,
     taking into account the extra-bytes option for each of them. The
     element-count argument can be used for allocating arrays of sizes
     that are not known statically. The keyword element-count is used
     for this option rather than size in order to avoid conflict with
     the size collection keyword. The logical size of a collection
     represented by a pointer wrapper and the number of array elements
     that implement it may differ; a null-terminated string is an
     example of such a case.

     This ``make`` method calls ``initialize`` on the wrapper object it
     generates before returning it.

     ::

         ? define variable *space-for-one-int* = make(<C-int*>);

         ? *space-for-one-int*[0];
         97386437634 // Could have been anything unless the
           // default allocator guarantees to zero new memory.

         ? *space-for-one-int*[0] := 0;
         0

         ? *space-for-one-int*[0];
         0

         ? define variable *space-for-ten-ints*
         = make(<C-int*>, element-count: 10);

         ? define C-struct <Z-properties>
           slot type :: <C-int>;
           array slot properties :: <C-int>,
         end C-struct <Z-properties>;

         ? define variable *props* =
           make(<Z-properties>,
             extra-bytes: 10 * size-of(<C-int>));

.. generic-function:: destroy
   :open:

   Frees the allocated heap memory at a specified address.

   :signature: destroy *C-pointer* #key *de-allocator* => ()

   :parameter c-pointer: An instance of `<C-pointer>`.
   :parameter #key de-allocator: An instance of :drm:`<function>`.

   :description:

     Frees the allocated heap memory at the address encapsulated in
     *C-pointer*.

     The *deallocator* argument should be a Dylan function that can
     serve as a deallocation facility. It must accept an address as a
     ``<machine-word>`` and free the storage allocated at that address.

     You should only use ``destroy`` on pointers allocated using
     ``make`` where no address was given. If *allocator* was passed to
     ``make``, the matching deallocator should be passed to ``destroy``.

     There is a default method for destroy on
     :class:`<C-statically-typed-pointer>`.

.. macro:: with-stack-structure
   :statement:

   Allocates an object within the scope of the body of the code.

   :macrocall:
     .. code-block:: dylan

        with-stack-structure (*name* :: *wrapper-type*
            #key *element-count* *extra-bytes*)
          *body*
        end [with-stack-structure]

   :parameter name: A Dylan variable name.
   :parameter wrapper-type: A Dylan name.
   :parameter #key element-count: An instance of :drm:`<integer>`.
   :parameter #key extra-bytes: An instance of :drm:`<integer>`.

   :description:

     Allocates an object *name* within the scope of a *body*. The
     *element-count* and *extra-bytes* options behave as in ``make``.
     The memory that was allocated is freed after *body* exits.

     This macro gives the object *dynamic extent*.

   :example:

     .. code-block:: dylan

        define C-struct <PointStruct>
          slot x-coord :: <C-unsigned-short>;
          slot y-coord :: <C-unsigned-short>;
          pointer-type-name: <PointStruct*>
        end C-struct;

        define constant <Point> = <PointStruct*>;

        define C-function PlotPoint
          parameter point :: <Point>;
          c-name: "PlotPoint";
        end C-function;

        define method plot (x, y)
          with-stack-structure (point :: <Point>)
            point.x-coord := 20;
            point.y-coord := 30;
            PlotPoint(point);
          end;
        end;

Utility designator classes
==========================

The following designator classes are defined for convenience purposes
using :macro:`define c-mapped-subtype`.

.. class:: <C-boolean>
   :open:
   :abstract:

   :description:

     A mapped subclass of ``<C-int>`` that provides an analogue to
     Dylan’s :drm:`<boolean>` class. The Dylan type for both import and
     export is :drm:`<boolean>`, and the C type is ``int``. The C integer
     ``0`` is mapped to ``#f`` in Dylan, and all other values are mapped
     to ``#t``.

.. class:: <C-string>
   :open:
   :abstract:

   :description:

     A mapped subclass of ``<C-char*>`` and :drm:`<string>`. On export the
     Dylan types ``<C-string>``, or :drm:`<byte-string>` may be passed to
     C. On import all values are mapped to ``<C-string>``. A
     :drm:`<byte-string>` may be passed to C directly and no copying takes
     place. The value in C will be a pointer to the data of the
     byte-string. The implementation of :drm:`<byte-string>` is such that,
     unless there are ``NULL`` characters embedded in the string,
     ``strlen`` in C and ``size`` in Dylan will return the same value.

     A :drm:`<byte-string>` may only be safely passed to a C function if
     its value is never stored and used after the call returns.

.. class:: <C-character>
   :open:
   :abstract:

   :description:

     The Dylan type for import and export is :drm:`<character>`. It is a
     designator that allows instances of :drm:`<character>` to be passed to
     and from C.

.. macro:: with-c-string
   :statement:

   Passes a C pointer to the contents of a :drm:`<byte-string>`.

   :macrocall:
     .. code-block:: dylan

        with-c-string (*variable* = *string-valued-expression*)
          *body*
        end

   :parameter variable: A Dylan variable name.
   :parameter string-valued-expression: An instance of :drm:`<string>`.

   :description:

     Use this macro when you need to pass C a pointer to the contents of
     a :drm:`<byte-string>`, but for some reason it cannot be passed
     directly. Inside the *body*, *variable* is bound to a
     :class:`<C-string>` object that refers to the contents of the
     string returned by *string-valued-expression*.

     .. note:: The ``<c-string>`` object is only live during the period that
        *body* is executing. If the program holds onto the pointer after that,
        the data it refers to cannot be guaranteed to be correct, because the
        garbage collector can no longer keep track of it.

.. function:: clear-memory!

   Stores zeros in the specified bytes of memory.

   :signature: clear-memory! *pointer*, *size* => ()

   :parameter pointer: An instance of type :class:`<C-pointer>` that
     points to the memory location at which to start writing zeros.
   :parameter size: An instance of type :drm:`<integer>`. The number of
     bytes to clear.

   :description:

     Stores zeros into *size* bytes of memory beginning at *pointer*.
     The space is assumed to be a whole number of words and
     word-aligned.

.. function:: copy-bytes!

   Copies an arbitrary number of bytes at an arbitrary alignment.

   :signature: copy-bytes! *destination-pointer*, *source-pointer*, *size* => ()

   :parameter destination-pointer* An instance of type :class:`<C-pointer>`.
   :parameter source-pointer* An instance of type :class:`<C-pointer>`.
   :parameter size* An instance of :drm:`<integer>`.

   :description:

     Copies an arbitrary number of bytes at arbitrary alignment instead
     of copying whole words.

   See also :func:`copy-into!`.

.. function:: copy-into!

   Copies the specified number of words.

   :signature: copy-into! *destination-pointer*, *source-pointer*, *size*) => ()

   :parameter destination-pointer* An instance of type :class:`<C-pointer>`.
   :parameter source-pointer* An instance of type :class:`<C-pointer>`.
   :parameter size* An instance of :drm:`<integer>`.

   :description:

     Copies *size* bytes from *source-pointer* to *destination-pointer*.

     Although the size is specified in bytes, it will be assumed to be a
     multiple of the word size. The function may also assume that both
     pointers are word-aligned and that the two storage areas do not
     overlap.

   See also :func:`copy-bytes!`.

.. function:: equal-memory?

   Returns ``#t`` if the size of the two designated memory spaces have
   the same contents.

   :signature: equal-memory? *ptr1*, *ptr2*, *size* => <boolean>

   :parameter ptr1: An instance of type :class:`<C-pointer>`.
   :parameter ptr2: An instance of type :class:`<C-pointer>`.
   :parameter size: An instance of :drm:`<integer>`.

   :description:

     Returns ``#t`` if the *size* bytes of memory starting at pointer
     *ptr1* have the same contents as the memory starting at *ptr2*,
     else ``#f``. The space is assumed to be a whole number of words and
     word-aligned.

.. class:: <C-Dylan-object>
   :open:
   :abstract:

   :superclasses: :class:`<C-void*>`

   :description:

     A mapped subclass of :class:`<C-void*>`. Objects of this type
     correspond to specific Dylan objects. The Dylan type for import and
     export is ``<C-Dylan-Object>``. The C type is ``void*``.

     To pass a reference to an arbitrary Dylan object to C, the Dylan
     object first must be registered using
     :func:`register-C-Dylan-object`. Then a ``<C-Dylan-object>``
     *handle* to the object can be created using the function
     :func:`export-C-Dylan-object`. The handle can then be passed
     directly to any C transition point designated as
     :class:`<C-Dylan-object>`. Any object received by Dylan from a
     transition point designated as ``<C-Dylan-object>`` may be passed
     to :func:`import-C-Dylan-object` to get the Dylan object for which
     it was a handle.

.. function:: register-C-Dylan-object

   Allows objects to be passed to a C function as instances of
   :class:`<C-Dylan-object>`.

   :signature: register-C-Dylan-object *object*

   :parameter object: An instance of :drm:`<object>`.

   :description:

     Allows objects to be passed to a C function as instances of
     :class:`<C-Dylan-object>`.

     The ``register-C-Dylan-object`` function arranges for the garbage
     collector to leave the storage used by *object* unclaimed, and
     assures that the handle passed to C is not accidentally corrupted
     (from C’s point of view) by the memory manager.

   See also :func:`unregister-C-Dylan-object`.

.. function:: unregister-C-Dylan-object

   Deallocates an object.

   :signature: unregister-C-Dylan-object *object*

   :parameter object: An instance of :drm:`<object>`.

   :description:

     Deallocates an object. When the handle is no longer needed from C,
     you call ``unregister-C-Dylan-object`` to allow the object to be
     normally reclaimed by the memory manager. Calls to
     :func:`register-C-Dylan-object` and ``unregister-C-Dylan-object``
     on the same object nest or interleave without interference. That
     is, if :func:`register-C-Dylan-object` is called exactly twice on
     an object then ``unregister-C-Dylan-object`` must be called exactly
     twice before the memory manager can reclaim the space for the
     object as it normally would.

.. function:: export-C-Dylan-object

   Fetches the :class:`<C-Dylan-object>` handle for a Dylan object.

   :signature: export-C-Dylan-object *object* => *c-dylan-object*

   :parameter object: An instance of :class:`<C-Dylan-object>`.
   :parameter object: An instance of :drm:`<object>`.

   :description:

     Fetches the :class:`<C-Dylan-object>` handle for a Dylan object.

.. function:: import-C-Dylan-object

   Fetches the Dylan object for a :class:`<C-Dylan-object>` handle.

   :signature: import-c-dylan-object *c-dylan-object* => *object*

   :parameter object: An instance of :class:`<C-Dylan-object>`.
   :value object: An instance of :drm:`<object>`.

   :description:

     Fetches the Dylan object for a :class:`<C-Dylan-object>` handle.
