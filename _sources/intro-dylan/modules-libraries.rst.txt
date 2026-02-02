*******************
Modules & Libraries
*******************

Modules and libraries provide the structure of a Dylan program. Modules
represent namespaces and control access to objects such as classes, functions,
variables, and constants.  Libraries are the unit of compilation in a Dylan
program. A library contains any number of modules, which may or may not be
exported for other libraries to use.

Simple Modules
==============

Modules import names (or bindings) from other modules and export names
for use by other modules. The names that may be imported/exported are
the module-level (also called "global") variables such as those created
by :drm:`define variable`, :drm:`define class`, :drm:`define generic`, etc.

The dependencies between modules must form a directed, acyclic
graph. Two modules may not use each other, and no circular dependencies
may exist. A sample module containing the vehicle classes from earlier
chapters might look like this:

.. code-block:: dylan

    define module vehicles
      use dylan;
      export
        <vehicle>,
        serial-number,
        owner,
        owner-setter,
        tax,
        <car>,
        <truck>,
        capacity;
    end module;

Like all normal modules, this one uses the ``dylan`` module, which
contains all of the standard built-in functions and classes. In turn,
the ``vehicles`` module exports all three of the vehicle classes, the
generic function ``tax``, several getter functions, and a single
:drm:`setter <slots>` function.

To make a :drm:`slot <slots>` public export its getter and setter functions. To
make the slot read-only, export just the getter function. To make it private,
export neither. In the above example, the slot ``serial-number`` is read-only,
while the slot ``owner`` is read/write.

Note that when a module adds a method to an imported generic function,
the change affects all modules using that function. :drm:`define method`
adds the new method to the existing generic function object, which may
be referenced by any module importing its binding. The module that
originally defined the generic function may prevent this behavior by
:drm:`sealing` it over specific argument types.

Import Options
==============

Dylan allows very precise control over how bindings are imported from
other modules. For example, individual bindings may be imported by
name. They may be renamed, either one at a time, or by adding a prefix
to all of a module's names at once. Some or all of them may be
re-exported immediately. See the DRM :drm:`define module` for specific
examples.

Dylan's module system has a number of advantages. Name conflicts
occur rarely. Programmers don't need to define or maintain function
prototypes. There's no need for header files. Modules may
also provide different interfaces to the same objects -- one module
exports a complete interface, which another module imports, redefines
and re-exports. For example, it is common to use one or more unexported modules
for implementation and export a separate public API module.

Libraries
=========

Libraries contain modules. For example, the ``dylan``
library contains the ``dylan`` module
described earlier, the ``dylan-extensions`` module, and
several other implementation-dependent modules. Note that
a library and a module may share the same name. Modules with the
same name may also appear in more than one library.

Every library contains an implicit module called ``dylan-user`` which imports
all the names from the ``dylan`` module. This module is only used to define
your library and module definitions. The ``vehicle`` library, for example, might be
defined as follows in the ``dylan-user`` module:

.. code-block:: dylan

    Module: dylan-user

    define library vehicles
      use dylan;            // This is the library!
      export                // These are modules.
        vehicles,           // (Defined above.)
        traffic-simulation,
        crash-testing,
        inspection;         // (Hypothetical.)
    end library vehicles;

This library could in turn be imported by another library:

.. code-block:: dylan

    Module: dylan-user

    define library vehicle-application
      use dylan;
      use my-gui-classes;
      use vehicles;
    end;

Libraries use other libraries and export modules, whereas
modules use other modules and export bindings. In general, a
module may use any module found in its own library or exported
from a library imported by its own library. The following module, for
example, could belong to the ``vehicle-application`` library.

.. code-block:: dylan

    Module: dylan-user

    define module sample-module
      // module name        source library
      use dylan;            // dylan
      use dylan-extensions; // dylan
      use menus;            // my-gui-classes
      use vehicles;         // vehicles
      use inspection;       // vehicles
    end module;

Sealing
=======

Classes and generic functions may be :drm:`sealed <sealing>`, preventing code
in other libraries from subclassing objects or adding methods to generic
functions. This allows the compiler to optimize more effectively. Both classes and
generic functions are sealed by default.

To allow code in other libraries to subclass a given class,
declare it as ``open``:

.. code-block:: dylan

    define open class <sample> (<object>) end;

To allow other libraries to add methods to a generic function,
use a similar syntax:

.. code-block:: dylan

    define open generic sample-function (o :: <object>) => ();

A third form, :drm:`define sealed domain`, partially
seals a generic function, disallowing only some additions from outside
a library.

For more information on sealing, see :drm:`"Sealing" in the DRM <sealing>`.
