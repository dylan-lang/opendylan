*************
Hello, World!
*************

The canonical "Hello World" program in Dylan.

File library.dylan:

.. code-block:: dylan

  Module: dylan-user

  define library hello
    use io, import: { format-out };
  end;

  define module hello
    use format-out;
  end;

File hello.dylan:

.. code-block:: dylan

  Module: hello

  format-out("Hello!\n");


**How it works:**

1. Every Dylan program defines a library and at least one module in
   that library, and the simple Hello World program is no exception.

#. Every source file must have a "Module" header to say which module
   its definitions should be in.   The library is defined in the pre-existing
   "dylan-user" module.

#. The top level expressions in each source file are executed in order
   when the library is loaded, so there is no need for a "main" method
   here.
