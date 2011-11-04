***********************
Classes & Instantiation
***********************

Dylan makes it easy to define classes. You don't normally need to
write constructors or accessor functions, which saves a lot of boring
typing. You can initialize slots (a.k.a. "fields", "member variables",
etc.) by providing a default value or expression in the slot spec and
the caller may override the value using keyword arguments.  A small
percentage of classes still need "constructors", which can be defined
by overriding ``initialize``.

.. code-block:: dylan

    define class <car> (<object>)
      slot serial-number :: <integer> = unique-serial-number();
      slot model-name :: <string>,
	required-init-keyword: model:;
      slot has-sunroof? :: <boolean>,
	init-keyword: sunroof?:,
	init-value: #f;
    end class <car>;

    define variable *unique-serial-number* = 0;

    define function unique-serial-number() => (usn :: <integer>)
      let serial = *unique-serial-number*;
      *unique-serial-number* := *unique-serial-number* + 1;
      serial;
    end function;

    define constant $blue-car = make(<car>, model: "Viper");
    define constant $black-car = make(<car>, model: "Town Car", sunroof?: #t);
    define constant $red-car = make(<car>, model: "F40", sunroof?: #f);

