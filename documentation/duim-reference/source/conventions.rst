**************************
Conventions in this Manual
**************************

This chapter describes the conventions used in this manual and in the
DUIM software itself.

Audience, goals, and purpose
============================

This manual is intended for programmers using DUIM, and forms a complete
reference for the Application Programmer’s Interface (API) for DUIM. You
should also see *Building Applications using DUIM* for a description of
how to start building applications using DUIM. At some points, the API
also includes lower-level layers, which DUIM programmers are free to
specialize.

The DUIM library is a set of interfaces that allow you to create
graphical user interfaces (GUIs) for your application using Dylan code.

In this document, we may refer to two different audiences. A *user* is a
person who uses an application program that was written using DUIM. A
DUIM *programmer* is a person who writes application programs using
DUIM. Generally, this manual assumes that you, the reader, are the
programmer.

Example code fragments
======================

Throughout this manual, example code fragments are provided at suitable
points in the documentation. These provide short illustrations of how to
use the interfaces being described. If you wish, you can run these
examples interactively by typing them into the Dylan Playground.

A number of additional, longer examples are provided as part of the
Harlequin Dylan installation, and are installed on your hard disk
automatically. You can look at these examples and load them into the
environment by clicking on the examples button in the main window of the
Harlequin Dylan environment.

Longer examples are also provided and discussed fully in the *Building
Applications using DUIM*, which you should refer to for an introduction
to building DUIM applications.

Module structure
================

The functionality of DUIM is provided via a number of modules. Each
chapter of this manual indicates what module its API is exported from.

The *duim* module is the main API module, which contains the variables
for the API-level functions available.

The *duim-geometry* module provides basic support for coordinate
geometry. This allows the position of elements in a window object to be
determined correctly.

The *duim-extended-geometry* module provides more extensive support for
co-ordinate geometry that is only required for more specialist uses.

The *duim-dcs* module provides color support to the DUIM library.

The *duim-sheets* module provides basic support for sheets. Sheets are
the basic unit of window applications, and can be nested hierarchically
to build up a complete user interface.

The *duim-graphics* module provides support for graphics drawing

The *duim-layouts* module provides support for a layout protocol that
makes it easy to create and layout groups of related elements in a given
interface. This module can handle layout problems such as the spacing
and justification of a group of elements automatically.

The *duim-gadgets* module provides all the gadgets available for use in
the DUIM library. Gadgets are the sheet objects that make up any user
interface, and the DUIM library supplies all the gadgets you will need
in your applications.

The *duim-frames* module provides support for frames. A DUIM frame is a
combination of a set of nested sheets, together with an event loop that
describes the behavior of the elements in those sheets. DUIM frames can
be used to specify whether a given user interface is displayed in an
application as a dialog box, or a more straightforward window, or as a
task wizard, and so on.

The Dylan Playground should be used when you just want to experiment
with DUIM code fragments without creating modules of your own. For real
application code, of course, you should define your own modules and
libraries and use the appropriate library code required by your
application.

Spread point arguments to functions
===================================

Many functions that take point arguments come in two forms: *structured*
and *spread*. Functions that take structured point arguments take the
argument as a single *point* object. Functions that take spread point
arguments take a pair of arguments that correspond to the *x* and *y*
coordinates of the point.

Functions that take structured point arguments, or return structured
point values have an asterisk in their name, for example, *draw-line\**
.

Immutability of objects
=======================

Most DUIM objects are *immutable*, that is, at the API level none of
their components can be modified once the object is created. Examples of
immutable objects include all of the members of the *<region>* classes,
pens, brushes, colors, and text styles. Since immutable objects by
definition never change, functions in the DUIM API can safely capture
immutable objects without first copying them. This also allows DUIM to
cache immutable objects. Any *make* methods that return immutable
objects are free to either create and return a new object, or return an
already existing object.

A few DUIM objects are *mutable*. Some components of mutable objects
can be modified once the object has been created, usually via setter
functions.

In DUIM, object immutability is maintained at the class level.
Throughout this specification, the immutability or mutability of a class
will be explicitly specified.

Some immutable classes also allow *interning*. A class is said to be
interning if it guarantees that two instances that are equivalent will
always be ``==``. For example, the class ``<text-style>`` is interned, so
calling ``make-text-style`` twice with the same arguments would return
identical values.

In some rare cases, DUIM *will* modify objects that are members of
immutable classes. Such objects are referred to as being *volatile*.
Extreme care must be take with volatile objects. For example, objects of
class *<bounding-box>* are often volatile.

Behavior of interfaces
----------------------

Any interfaces that take or return mutable objects can be classified in
a few different ways.

Most functions *do not capture* their mutable input objects, that is,
these functions will either not store the objects at all, or will copy
any mutable objects before storing them, or perhaps store only some of
the components of the objects. Later modifications to those objects will
not affect the internal state of DUIM.

Some functions *may capture* their mutable input objects. That is, it is
not specified whether the mutable inputs to these functions will or will
not be captured. For such functions, you should assume that these
objects will be captured and must not modify these objects capriciously.
Furthermore, the behavior is undefined if these objects are later
modified.

Some functions that return mutable objects are guaranteed to create
*fresh outputs*. These objects can be modified without affecting the
internal state of DUIM.

Functions that return mutable objects that are not fresh objects fall
into two categories:

-  Those that return *read-only state*
-  Those that return *read/write state*

If a function returns read-only state, programmers must not modify that
object; doing so might corrupt the state of DUIM. If a function returns
read/write state, the modification of that object is part of the DUIM
interface, and you are free to modify the object in ways that make
sense.

Specialized arguments to generic functions
==========================================

Unless otherwise stated, this manual uses the following convention for
specifying which arguments to generic functions are specialized:

-  If the generic function is a ``-setter`` function, the second argument
   is the one that is intended to be specialized.
-  If the generic function is a "mapping" function (such as ``do-sheets``),
   the second argument (the object that specifies what is being
   mapped over) is the one that is specialized. The first argument (the
   functional argument) is not intended to be specialized.
-  Otherwise, the first argument is the one that is intended to be
   specialized.

Macros that expand into calls to advertised functions
=====================================================

Many macros that take a "body" argument expand into a call to an
advertised function that takes a functional argument. This functional
argument will execute the supplied body. For a macro named
``with-environment``, the function is generally named
``do-with-environment``. For example, ``with-drawing-options`` might
be defined as follows:

.. code-block:: dylan

    define macro with-drawing-options
      { with-drawing-options
        (?medium:name, #rest ?keys:\*) ?body:body end }
          => { begin
                 let with-drawing-options-body =
                   method (?medium) ?body end;
                 do-with-drawing-options(?medium,
                   with-drawing-options-body, ?keys)
               end }
    end macro;

    define method do-with-drawing-options
        (medium :: <medium>, function, #rest options)
      apply(merge-drawing-options-into-medium, medium, options);
      function(medium)
    end;

Terminology pertaining to error conditions
==========================================

When this documentation specifies that it "is an error" for some
situation to occur, this means that:

-  No valid DUIM program should cause this situation to occur.
-  If this situation does occur, the effects and results are undefined.
-  DUIM often tries to detect such an error, but it might not.

When this manual specifies that some argument "must be a *type* " or
uses the phrase "the *type* argument", this means that it is an
error if the argument is not of the specified *type*. DUIM tries to
detect such type errors, but it might not always be successful.

When this documentation says that "an error is signalled" in some
situation, this means that:

-  If the situation occurs, DUIM will signal an error using ``error`` or
   ``cerror``.
-  Valid DUIM programs may rely on the fact that an error will be
   signalled.

When this manual states that "a condition is signalled" in a given
situation, this is the same as saying that "an error is signalled", with
the exception that the condition will be signalled using ``signal``
instead of ``error``.
