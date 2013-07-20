*******
Objects
*******

The features of Dylan's object system don't map directly onto the
features found in C++. Dylan handles access control using
:term:`modules`, not ``private`` declarations within
individual classes. Standard Dylan has no destructors, but instead relies
upon the garbage collector to recover memory and on block/cleanup
to recover lexically scoped resources. Dylan objects don't even have real
member functions.

Dylan's object system is at least as powerful as that of C++. Multiple
inheritance works smoothly, constructors are rarely needed and there's
no such thing as object slicing. Alternate constructs replace the
missing C++ features. Quick and dirty classes can be turned into clean
classes with little editing of existing code.

Before starting, temporarily set aside any low-level expertise in
C++. Dylan differs enough that such knowledge can actually interfere
with the initial learning process.

Built-In Classes
================

Dylan has a large variety of built-in classes. Several of these
represent primitive data types, such as :drm:`<integer>`
and :drm:`<character>`. A few represent
actual language-level entities, such as :drm:`<class>`
and :drm:`<function>`. Most of the others
implement collection classes, similar to those found in C++'s
Standard Template Library. A few of the most important classes are
shown here:

.. figure: missing.png

   Several Standard Dylan Classes

The built-in collection classes include a number of common data
structures. Arrays, tables, vectors, ranges and deques should be
provided by all Dylan implementations. The language specification
also standardizes strings and byte-strings.

Not all the built-in classes may be subclassed. This allows the
compiler to heavily optimize code dealing with basic numeric types and
certain common collections. The programmer may also mark classes as
:term:`sealed`, restricting how and where they may be subclassed. See
`Sealing <http://opendylan.org/books/drm/Sealing>`_ for details.

Slots
=====

Objects have :term:`slots`, which resemble the data
members in C++ or fields in Java. Like
variables, slots are bound to values; they don't actually contain
their data. A simple Dylan class shows how slots are declared:

.. code-block:: dylan

    define class <vehicle> (<object>)
      slot serial-number;
      slot owner;
    end;

The above code would be quick and convenient to write while building a
prototype, but it could be improved. The slots have no declared types
so they default to :drm:`<object>`, and they don't specify default values
so they default to ``#f``.  The following snippet fixes both problems:

.. code-block:: dylan

    define class <vehicle> (<object>)
      slot serial-number :: <integer>,
        required-init-keyword: sn:;
      slot owner :: <string>,
        init-keyword: owner:,  // optional
        init-value: "Northern Motors";
    end class <vehicle>;

The type declarations work just like type declarations anywhere
else in Dylan; they limit a binding to objects of a given class or of
one of its subclasses, and they let the compiler optimize. The new
keywords describe how the slots get their initial values. (The keyword
``init-function`` may also be used; it must be followed
by a function with no arguments and the appropriate return type.)

To create a vehicle object using the new class declaration, a
programmer could write one of the following:

.. code-block:: dylan

    make(<vehicle>, sn: 1000000)
    make(<vehicle>, sn: 2000000, owner: "Sal")

In the first example, ``make`` returns a vehicle
with the specified serial number and the default owner. In the second
example, ``make`` sets both slots using the keyword
arguments.

Only one of ``required-init-keyword``, ``init-value`` and
``init-function`` may be specified. However, ``init-keyword``
may be paired with either of the latter two if desired. More
than one slot may be initialized by a given keyword.

Dylan also provides for the equivalent of C++ ``static``
members, plus several other useful allocation schemes. See
the DRM for the full details.

Getters and Setters
===================

An object's slots are accessed using two functions: a getter and
a setter. By default, the getter function has the same name as the
slot, and the setter function appends "``-setter``".
These functions may be invoked as follows:

.. code-block:: dylan

    owner(sample-vehicle);  // returns owner
    owner-setter("Faisal", sample-vehicle);

Dylan also provides some convenient "syntactic sugar"
for these two functions. They may also be written as:

.. code-block:: dylan

    sample-vehicle.owner;  // returns owner
    sample-vehicle.owner := "Faisal";

.. _generic-functions-objects:

Generic functions and Objects
=============================

Generic functions, introduced in :doc:`Methods and Generic functions
<methods-generic-functions>`, provide the equivalent of C++ member
functions. In the simplest case, just declare a generic function which
dispatches on the first parameter.

.. code-block:: dylan

    define generic tax (v :: <vehicle>) => (tax-in-dollars :: <float>);

    define method tax (v :: <vehicle>) => (tax-in-dollars :: <float>)
      100.00
    end;

    //=== Two new subclasses of vehicle

    define class <car> (<vehicle>)
    end;

    define class <truck> (<vehicle>)
      slot capacity, required-init-keyword: tons:;
    end;

    //=== Two new "tax" methods

    define method tax (c :: <car> ) => (tax-in-dollars :: <float>)
      50.00
    end method;

    define method tax (t :: <truck>) => (tax-in-dollars :: <float>)
      // standard vehicle tax plus $10/ton
      next-method() + t.capacity * 10.00
    end method;

The function ``tax`` could be invoked as
``tax(v)`` or ``v.tax``, because it
only has one argument. Generic functions with two or more arguments
must be invoked in the usual Dylan fashion; no syntactic sugar exists
to make them look like C++ member functions.

The version of tax for ``<truck>`` objects
calls a special function named ``next-method``. This
function invokes the next most specific method of a generic function;
in this case, the method for ``<vehicle>``
objects.  Parameters to the current method get passed along
automatically.

Technically, ``next-method`` is a special parameter to a method, and
may be passed explicitly using ``#next``.

.. code-block:: dylan

    define method tax
        (t :: <truck>, #next next-method) => (tax-in-dollars :: <float>)
      // standard vehicle tax plus $10/ton
      next-method() + t.capacity * 10.00
    end method;

Dylan's separation of classes and generic functions provides some
interesting design ideas. Classes no longer need to "contain"
their member functions; it's possible to write a new generic
function without touching the class definition. For example, a module
handling traffic simulations and one handling municipal taxes could
each have many generic functions involving vehicles, but both could
use the same vehicle class.

Slots in Dylan may also be replaced by programmer-defined accessor
functions, all without modifying existing clients of the class. The
DRM describes numerous ways to accomplish the change; several should
be apparent from the preceding discussion. This flexibility frees
programmers from creating functions like ``GetOwnerName`` and
``SetOwnerName``, not to mention the corresponding private member
variables and constructor code.

For even more creative uses of generic functions and the Dylan object
model, see the chapter on :doc:`Multiple Dispatch <multiple-dispatch>`.

Initializers
============

The ``make`` function handles much of the
drudgery of object construction. It processes keywords and initializes
slots. Programmers may, however, customize this process by adding
methods to the generic function ``initialize``. For
example, if vehicle serial numbers must be at least seven digits:

.. code-block:: dylan

    define method initialize (v :: <vehicle>, #key)
      next-method();
      if (v.serial-number < 1000000)
        error("Bad serial number!");
      end if;
    end method;

``initialize`` methods get called after regular
slot initialization. They typically perform error checking or calculate
derived slot values. Initialize methods must specify ``#key`` in their
parameter lists.

It's possible to access the values of slot keywords from
``initialize`` methods, and even to specify additional
keywords in the class declaration. See the DRM for further details.

Abstract Classes and Overriding Make
====================================

Abstract classes define the interface, not the implementation,
of an object. There are no direct instances of an abstract class.
Concrete classes actually implement their interfaces. Every abstract
class will typically have one or more concrete subclasses. For example,
if plain vanilla vehicles shouldn't exist, ``<vehicle>`` could
be defined as follows:

.. code-block:: dylan

    define abstract class <vehicle> (<object>)
      // ...as before
    end;

The above modification prevents the creation of direct instances
of ``<vehicle>``. At the moment, calling
``make`` on this class would result in an error.
However, a programmer could add a method to make which allowed the
intelligent creation of vehicles based on some criteria, thus making
``<vehicle>`` an :term:`instantiable abstract class`:

.. code-block:: dylan

    define method make
        (class == <vehicle>, #rest keys, #key big?)
     => (vehicle :: <vehicle>)
      if (big?)
        make(<truck>, keys, tons: 2)
      else
        make(<car>, keys)
      end
    end method make;

A number of new features appear in the parameter list. The expression
"``class == <vehicle>``" specifies a :term:`singleton` dispatch,
meaning this method will be called only if ``class`` is exactly
``<vehicle>``, not a subclass such as ``<car>``.  Singleton dispatch
is discussed in the chapter on :doc:`Multiple Dispatch
<multiple-dispatch>`.  The use of ``#rest`` and ``#key`` in the same
parameter list means all keyword arguments will be stored in the
``keys`` parameter but if ``big?`` is passed it will be bound to the
variable by the same name.  The new make method could be invoked in
any of the following fashions:

.. code-block:: dylan

    let x = 1000000;
    make(<vehicle>, sn: x, big?: #f); //=> car
    make(<vehicle>, sn: x, big?: #t); //=> truck
    make(<vehicle>, sn: x);           //=> car

Methods added to ``make`` don't actually need to create new objects. Dylan
officially allows them to return existing objects. This can be used to
manage lightweight shared objects, such as the "flyweights" or "singletons"
described by Gamma, et al., in
`Design Patterns <http://st-www.cs.uiuc.edu/users/patterns/DPBook/DPBook.html>`_.
