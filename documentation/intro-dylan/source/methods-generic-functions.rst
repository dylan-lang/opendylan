***************************
Methods & Generic Functions
***************************

Dylan :term:`methods` correspond roughly to the
functions found in C and Pascal. They take zero or more named parameters,
but also return zero or more named return values. A minimal Dylan method
might look like the following:

.. code-block:: dylan

    define method hello-world()
      puts("Hello, world!");
    end;

This method has no parameters and an unspecified return value. It
could return any number of values of any type. In order to make the
above code more clear, the function could be rewritten as follows:

.. code-block:: dylan

    define method hello-world() => ();
      puts("Hello, world!");
    end method;

There have been two changes. The function now officially returns
no value whatsoever. Also note that ``end`` has been
replaced by ``end method`` which could in turn be
rewritten as ``end method hello-world``. In general,
Dylan permits all the obvious combinations of keywords and labels to
follow an end statement.

Parameters & Parameter Lists
============================

Dylan methods declare parameters in fashion similar to that of
conventional languages, except for the fact that parameters may
optionally be untyped. Both of the following methods are legal:

.. code-block:: dylan

    define method foo(x :: <integer>, y) end;
    define method bar(m, s :: <string>) end;

Both ``foo`` and ``bar`` have
one typed and one untyped parameter, but neither has a well-defined
return value (or actually does anything). As in C, each typed parameter
must have its own type declaration; there's no syntax for saying
"the last three parameters were all integers".

Functions with variable numbers of parameters include the
``#rest`` keyword at the end of their parameter lists.
Thus, the declaration for C's ``printf`` function
would appear something like the following in Dylan:

.. code-block:: dylan

    define method printf(format-string :: <string>, #rest arguments) => ();
      // Print the format string, extracting one at a time from "arguments".
      // Note that Dylan actually allows us to verify the types of variables,
      // preventing those nasty printf errors, such as using %d instead of %ld.
      // ...
    end method printf;

Note that Dylan makes no provision for passing variables by
reference in the Pascal sense, or for passing pointers to variables.
parameter names are simply bound to whatever values are passed, and may
be rebound like regular variables. This means that there's no way to
write a ``swap`` function in Dylan (except by using
macros). However, the following function works just fine, because it
modifies the :term:`internal state` of another
object:

.. code-block:: dylan

    define method sell(car :: <car>, new-owner :: <string>) => ();
      if (credit-check(new-owner))
        car.owner := new-owner;
      else
        error("Bad credit!");
      end;
    end;

If this sounds unclear, reread the chapter on :doc:`variables and expressions
<expressions-variables>`.

Return Values
=============

Because Dylan methods can't have normal "output"
parameters in their parameter lists, they're allowed considerably
more flexibility when it comes to return values. Methods may return
more than one value. As with parameters, these values may be typed or
untyped. Interestingly enough, all return values *must*
be named.

A Dylan method -- or any other control construct -- returns
the value of the last expression in its body.

.. code-block:: dylan

    define method foo() => sample :: <string>;
      "Sample string.";  // return string
    end;

    define method bar() => my-untyped-value;
     if (weekend-day?(today()))
        "Let's party!";  // return string
     else
        make(<excuse>);  // return object
      end if;
    end method;

    define method moby( )
      =>sample :: <string>, my-untyped-value;
      values( foo(), bar() );  // return both!
    end;

    define method baz( ) => ( );
      let (x,y) = moby( );  // assign both
    end;

Bare Methods
============

Nameless methods may be declared inline. Such :term:`bare
methods` are typically used as parameters to other methods.
For example, the following code fragment squares each element of a list
using the built in ``map`` function and a bare
method:

.. code-block:: dylan

    define method square-list(in :: <list>)
     => out :: <list>
      map(method(x) x * x end, in);
    end;

The ``map`` function takes each element of
the list ``in`` and applies the anonymous method. It
then builds a new list using the resulting values and returns it.
The method ``square-list`` might be invoked as
follows:

.. todo:: Must distinguish return values from code.

.. code-block:: dylan

    square-list( #(1,2,3,4) );
    => #(1,4,9,16)

Local Methods
=============

Local methods resemble bare methods but have names. They are
declared within other methods, often as private utility routines. Local
methods are typically used in a fashion similar to Pascal's local
functions.

.. code-block:: dylan

    define method sum-squares(in :: <list>) => sum-of-element-squares :: <integer>;
      local method square( x )
              x * x;
            end,
            method sum(list :: <list>)
              reduce1(\+, list);
            end;
      sum(map(square, in));
    end;

Local methods can actually outlive the invocation of the
function which created them. parameters of the parent function remain
bound in a local method, allowing some interesting techniques:

.. code-block:: dylan

    define method build-put(string :: <string>) => (res :: <function>);
      local method string-putter()
              puts(string);
            end;
      string-putter;  // return local method
    end;

    define method print-hello() => ();
      let f = build-put("Hello!");
      f();  // print "Hello!"
    end;

Local functions which contain bound variables in the above
fashion are known as :term:`closures`.

.. _generic-functions:

Generic functions
=================

A :term:`generic function` represents zero or more
similar methods. Every method created by means of ``define
method`` is automatically :term:`contained`
within the generic function of the same name. For example, a 
programmer could define three methods named ``display``,
each of which acted on a different data type:

.. code-block:: dylan

    define method display(i :: <integer>)
      do-display-integer(i);
    end;

    define method display(s :: <string>)
      do-display-string(s);
    end;

    define method display(f :: <float>)
      do-display-float(f);
    end;

When a program calls ``display``, Dylan examines
all three methods. Depending on the number and type of arguments to
``display``, Dylan invokes one of the above methods.
If no methods match the actual parameters, an error occurs.

In C++, this process occurs only at compile time. (It's called
operator overloading.) In Dylan, calls to ``display``
may be resolved either at compile time or while the program is actually
executing. This makes it possible to define methods like:

.. code-block:: dylan

    define method display(c :: <collection>)
      for (item in c)
        display(item);  // runtime dispatch
      end;
    end;

This method extracts objects of unknown type from a collection,
and attempts to invoke the generic function ``display``
on each of them. Since there's no way for the compiler
to know what type of objects the collection actually contains, it
must generate code to identify and invoke the proper method at
runtime. If no applicable method can be found, the Dylan runtime
environment throws an exception.

Generic functions may also be declared explicitly, allowing the
programmer to exercise control over what sort of methods get added.
For example, the following declaration limits all ``display``
methods to single parameter and no return value:

.. code-block:: dylan

    define generic display(thing :: &object;) => ()

Generic functions are explained in greater detail in the chapter on
:doc:`multiple dispatch <multiple-dispatch>`.

Keyword Arguments
=================

Functions may accept :term:`keyword arguments`,
extra parameters which are identified by a label rather than by their
position in the argument list. Keyword arguments are often used in a
fashion similar to :term:`default parameter values`
in C++. For example, the following hypothetical method might print
records to an output device:

.. code-block:: dylan

    define method print-records(records :: <collection>,
      #key init-codes = "", lines-per-page = 66) => ();

      send-init-codes(init-codes);
      // ...print the records
    end method;

This method could be invoked in one of several ways. The first
specifies no keyword arguments, and the latter two specify some
combination of them. Note that order of keyword arguments doesn't
matter.

.. code-block:: dylan

    print-records(recs);
    print-records(recs, lines-per-page: 65);
    print-records(recs, lines-per-page: 120, init-codes: "***42\n");

Programmers have quite a bit of flexibility in specifying
keyword arguments. They may optionally omit the default value for a
keyword (in which case ``#f`` is used). Default value
specifiers may actually be function calls themselves, and may rely on
regular parameters already being in scope. Variable names may be
different from keyword names, a handy tool for preventing name
conflicts.

For more information on keyword arguments, especially their use
with :ref:`generic functions <generic-functions>` see the DRM.
