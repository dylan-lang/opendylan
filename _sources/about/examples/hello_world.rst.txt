*************
Hello, World!
*************

The canonical "Hello World" program in Dylan.

**File library.dylan:**

.. code-block:: dylan

   Module: dylan-user

   define library hello
     use dylan;
     use io, import: { format-out };
   end;

   define module hello
     use dylan;
     use format-out;
   end;

**File hello.dylan:**

.. code-block:: dylan

   Module: hello

   format-out("Hello!\n");


.. hint:: The `deft new application
          <https://package.opendylan.org/deft/index.html#deft-new-application>`_
          command (called ``dylan new application`` in Open Dylan 2024.1 and
          older releases) will create these files for you, along with a test
          suite and build files.

How it works
============

* Every Dylan program defines a library and at least one module in that
  library, and the simple Hello World program is no exception.

* Every source file has a ``Module:`` header to say which module its
  definitions should be in. The library and module themselves are defined in
  the pre-existing ``dylan-user`` module.

* The top level expressions in each source file are executed in order when the
  library is loaded, so there's no need for a ``main`` function.
