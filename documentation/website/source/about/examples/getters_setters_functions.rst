*******************************
Getters & Setters are functions
*******************************

Many object-oriented languages encourage you to define a get_foo and
set_foo function for each member variable foo. These allow you to
control access to the variables and provide for future extensibility.
For example, set_foo could be modified to keep a table of values of foo.

Dylan provides for this extensibility without having to write the
accessor functions up front. Consider a simple Dylan class:

.. code-block:: dylan

    define class <vehicle> (<object>)
      slot driver, required-init-keyword: driver:;
    end;

This creates two functions, driver and driver-setter.  (You can prevent
driver-setter from being defined by using ``constant slot driver ...`` instead.)
Under normal circumstances, these two functions will be inlined and optimized
into direct accesses.

Dylan provides some handy syntactic sugar to make these functions look
like members of a C structure:

.. code-block:: dylan

    let car = make(<vehicle>, driver: "Frank");

    // These two expressions return the same value.
    car.driver;
    driver(car);

    // These three statements perform the same assignment.
    car.driver := "Sally";
    driver(car) := "Sally";
    driver-setter("Sally", car);

If you later discover that you can't allow direct access to the driver
slot, you can rename the slot and create two explicit functions named
driver and driver-setter to replace it. The following example
maintains a hash table mapping drivers to their cars:

.. code-block:: dylan

    define constant *driver-to-car-map* = make(<table>);

    define class <vehicle> (<object>)
      slot %driver, required-init-keyword: driver:;
    end;

    define method initialize
        (vehicle :: <vehicle>, #key, #all-keys)
      *driver-to-car-map*[vehicle.%driver] := vehicle;
    end;

    // The name "driver" would be exported as the public API.
    define constant driver = %driver;

    define method driver-setter
        (driver, vehicle :: <driver>) => (value :: <object>)
      *driver-to-car-map*[driver] := vehicle;
      vehicle.%driver := driver;
    end;

