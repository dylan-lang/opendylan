*******************************
Getters & Setters are functions
*******************************

Many object-oriented languages force you to manually define ``get_foo`` and
``set_foo`` functions for each class slot ``foo`` in order to control access to
the slots and provide for future extensibility.  For example, ``set_foo``
could be modified to keep a table of values of ``foo``.

Dylan provides for this extensibility without having to write the
accessor functions yourself. Consider a simple Dylan class:

.. code-block:: dylan

    define class <vehicle> (<object>)
      slot driver, required-init-keyword: driver:;
    end;

This creates two functions, ``driver`` and ``driver-setter``.  You can prevent
``driver-setter`` from being defined by adding the ``constant`` modifier to the slot declaration:

.. code-block:: dylan

   constant slot driver, required-init-keyword: driver:;

Under normal circumstances, both the getter and setter function will be inlined
and optimized into direct accesses.

If you later discover that you can't allow direct access to the driver slot,
you can rename the slot and create two explicit functions named ``driver`` and
``driver-setter`` to replace it, thus changing the internal behavior without
changing the API for your class. The following example maintains a hash table
mapping drivers to their cars:

.. code-block:: dylan

    define constant *driver-to-car-map* = make(<table>);

    define class <vehicle> (<object>)
      // The leading '%' is a convention for naming internal slots.
      slot %driver, required-init-keyword: driver:;
    end;

    define method initialize
        (vehicle :: <vehicle>, #key, #all-keys)
      *driver-to-car-map*[vehicle.%driver] := vehicle;
    end;

    // The name "driver" would be exported as the public API.
    define constant driver = %driver;

    define method driver-setter
        (driver, vehicle :: <vehicle>) => (value :: <object>)
      *driver-to-car-map*[driver] := vehicle;
      vehicle.%driver := driver;
    end;

Note that Dylan provides some handy syntactic sugar to make slot accessor
functions look like members of a C structure:

.. code-block:: dylan

    let car = make(<vehicle>, driver: "Frank");

    // These two expressions are semantically identical and return the same value.
    car.driver;
    driver(car);

    // These three statements perform the same assignment.
    car.driver := "Sally";
    driver(car) := "Sally";
    driver-setter("Sally", car);

