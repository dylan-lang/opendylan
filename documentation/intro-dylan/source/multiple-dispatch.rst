*****************
Multiple Dispatch
*****************

:term:`Multiple dispatch` is one of the most powerful
and elegant features of Dylan. As explained in the section on
:ref:`generic functions and objects <generic-functions-objects>`,
Dylan methods are declared separately from the classes upon which they
act.  :term:`Polymorphism`, the specialization of methods
for use with particular classes, can be implemented by declaring several
methods with different parameters and attaching them to one generic
function:

.. code-block:: dylan

    define generic inspect-vehicle(v :: <vehicle>, i :: <inspector>) => ();

    define method inspect-vehicle(v :: <vehicle>, i :: <inspector>) => ();
      look-for-rust(v);
    end;

    define method inspect-vehicle(car :: <car>, i :: <inspector>) => ();
      next-method();  // perform vehicle inspection
      check-seat-belts(car);
    end;

    define method inspect-vehicle(truck :: <truck>, i :: <inspector>) => ();
      next-method();  // perform vehicle inspection
      check-cargo-attachments(truck);
    end;

However, different types of vehicle inspectors may have different
policies. A state inspector, in addition to the usual procedures, will
also typically check a car's insurance policy. To implement this, add
another method to the generic function ``inspect-vehicle``:

.. code-block:: dylan

    define method inspect-vehicle(car :: <car>, i :: <state-inspector>) => ();
      next-method();  // perform car inspection
      check-insurance(car);
    end;

    let inspector = make(<state-inspector>);
    let car = make(<car>);
    inspect-vehicle(car, inspector);

Calling the generic function ``inspect-vehicle``
with these arguments performs three separate tasks:
``look-for-rust``, ``check-seat-belts`` and
``check-insurance``. The most specific method on
``inspect-vehicle`` -- the one for the classes
``<car>`` and ``<state-inspector>`` -- is invoked first
and calls ``next-method`` to invoke the less-specific methods in turn.

For an exact definition of "specific", see the DRM.

Dispatching on Specific Objects
===============================

Dylan also allows functions to dispatch on specific objects. For
example, state inspectors might pass the governor's car without
actually looking at it. Dylan expresses this situation using
:term:`singletons`, objects which are treated as
though they were in a class of their own. For example:

.. code-block:: dylan

    define constant $governors-car = make(<car>);

    define method inspect-vehicle(car == $governors-car,
      i :: <state-inspector>) => ();
      wave-through(car);
    end;

(In this example, none of the usual inspection methods will be
invoked since the above code neglects to call ``next-method``.)
