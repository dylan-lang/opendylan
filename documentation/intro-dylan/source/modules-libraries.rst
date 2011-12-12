*******************
Modules & Libraries
*******************

Modules and libraries provide the structure of a Dylan program. Modules
represent namespaces and control access to objects and functions.
Libraries contain modules, and act as units of compilation in a
finished Dylan program.

Simple Modules
==============

Modules import the symbols of other modules and export their
own. The dependencies between modules must form a directed, acyclic
graph. Two modules may not use each other, and no circular
dependencies may exist.

Modules only export variables. Since the names of classes and
generic functions are actually stored in variables, this represents
no hardship. A sample module containing the vehicle classes from
earlier chapters might resemble:

.. code-block:: dylan

    define module Vehicles
      use Dylan;
      export
        <vehicle>,
          serial-number,
          owner, owner-setter,
          tax,
        <car>,
        <truck>,
          capacity;
    end module;

Like all normal modules, this one uses the ``Dylan`` module, which
contains all of the standard built-in functions and classes. In turn,
the ``Vehicles`` module exports all three of the vehicle classes, the
generic function ``tax``, several getter functions and a single
setter function.

To control access to a slot, export some combination of its
getter and setter functions. To make a slot public, export both. To
make it read-only, export just the getter function. To make it
private, export neither. In the above example, the slot
``serial-number`` is read-only, while the slot
``owner`` is public.

Note that when some module adds a method to a generic function,
the change affects all modules using that function. The new method
actually gets added *into* the variable representing
the generic function. Since the variable has been previously exported,
all clients can access the new value.

Import Options
==============

Dylan allows very precise control over how symbols are imported
from other modules. For example, individual symbols may be imported
by name. They may be renamed, either one at a time, or by adding a
prefix to all a module's symbols at once. Some or all of them may be
re-exported immediately. See the DRM for specific examples.

Dylan's import system has a number of advantages. Name conflicts
occur rarely. Programmers don't need to define or maintain function
prototypes. There's no explicit need for header files. Modules may
also provide different interfaces to the same objects -- one module
exports a complete interface, which another module imports, redefines
and re-exports.

Libraries
=========

Libraries contain modules. For example, the ``Dylan``
library contains the ``Dylan`` module
described earlier, the ``Extensions`` module, and
possibly several other implementation-dependent modules. Note that
a library and a module may share a given name. Modules with the
same name may also appear in more than one library.

By default, a Dylan environment provides a library called
``Dylan-User`` for the convenience of the programmer.
This is typically used for short, single library programs which
depend only on modules found in the Dylan library.

Additionally, every library contains an implicit module, also
known as ``Dylan-User``, which imports all of the
modules found in the ``Dylan`` library. This may be
used for single module programs. Many Dylan environments, however,
use it to bootstrap new library definitions. The vehicle library,
for example, might be defined as follows in a ``Dylan-User``
module:

.. code-block:: dylan

    define library Vehicles
      use Dylan;            // This is the library!
      export                // These are modules.
        Vehicles,           // (Defined above.)
        Traffic-Simulation,
        Crash-Testing,
        Inspection;         // (Hypothetical.)
    end library Vehicles;

This library could in turn be imported by another library:

.. code-block:: dylan

    define library Vehicle-Application
      use Dylan;
      use My-GUI-Classes;
      use Vehicles;
    end;

Libraries import other libraries and export modules, whereas
modules import other modules and export variables. In general, a
module may import any module found in its own library or exported
from a library imported by its own library. The following module, for
example, could belong to the ``Vehicle-Application`` library.

.. code-block:: dylan

    define module Sample-Module
      // module name         source library
      use Dylan;          // Dylan
      use Extensions;     // Dylan
      use Menus;          // My-GUI-Classes
      use Vehicles;       // Vehicles
      use Inspection;     // Vehicles
    end module;

Sealing
=======

Classes and generic functions may be :term:`sealed`
using a number of Dylan forms. This prevents code in other libraries
from subclassing objects or adding methods to generic functions, and
lets the compiler optimize more effectively. Both classes and generic
functions are sealed by default.

To allow code in other libraries to subclass a given class,
declare it as ``open``:

.. code-block:: dylan

    define open class <sample> (<object>) end;

To allow other libraries to add methods to a generic function,
use a similar syntax:

.. code-block:: dylan

    define open generic sample-function( o :: <object> ) => ();

A third form, ``define inert domain``, partially
seals a generic function, disallowing only some additions from outside
a library.

For more information on sealing, see the chapter
"Controlling Dynamism" in the DRM.
