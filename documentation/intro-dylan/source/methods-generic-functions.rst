***************************
Methods & Generic Functions
***************************

Dylan :term:`methods` correspond roughly to the
functions found in C++. They take zero or more named parameters,
but also return zero or more named return values. A minimal Dylan method
might look like the following:

.. code-block:: dylan

    define method hello-world ()
      format-out("Hello, world!");
    end;

This method has no parameters and an unspecified return value. It
could return any number of values of any type. In order to make the
above code more clear, the function could be rewritten as follows:

.. code-block:: dylan

    define method hello-world () => ()
      format-out("Hello, world!");
    end method;

There have been two changes. The function now officially returns
no values whatsoever. Also note that ``end`` has been
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

    define method foo (x :: <integer>, y) end;
    define method bar (m, s :: <string>) end;

Both ``foo`` and ``bar`` have
one typed and one untyped parameter, but neither has a well-defined
return value (or actually does anything). As in C, each typed parameter
must have its own type declaration; there's no syntax for saying
"the last three parameters are all integers".

Functions with variable numbers of parameters include the
``#rest`` keyword in their parameter lists.
Thus, the declaration for C's ``printf`` function
would appear something like the following in Dylan:

.. code-block:: dylan

    define method printf (format-string :: <string>, #rest arguments) => ()
      // Print the format string, extracting one at a time from "arguments".
      // Note that Dylan actually allows us to verify the types of variables,
      // preventing those nasty printf errors, such as using %d instead of %ld.
      // ...
    end method printf;

Note that Dylan makes no provision for passing variables by
reference in the Pascal sense, or for passing pointers to variables.
parameter names are simply bound to whatever values are passed, and may
be rebound like regular variables. This means that there's no way to
write a ``swap`` function in Dylan.  (It may be done using
macros). However, the following function works just fine, because it
modifies the :term:`internal state` of another
object:

.. code-block:: dylan

    define method sell (car :: <car>, new-owner :: <string>) => ()
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

Because Dylan methods can't have "output" parameters, they're allowed
considerably more flexibility when it comes to return values. Methods
may return more than one value. As with parameters, these values may
be typed or untyped. All return values *must* be named.

A Dylan method -- or any other control construct -- returns
the value of the last expression in its body.

.. code-block:: dylan

    define method foo () => (sample :: <string>)
      "Sample string."    // return string
    end;

    define method bar () => (my-untyped-value)
      if (weekend-day?(today()))
        "Let's party!"  // return string
      else
        make(<excuse>)  // return object
      end if
    end method;

    define method moby () => (sample :: <string>, my-untyped-value)
      values(foo(), bar())    // return both!
    end;

    define method baz () => ()
      let (x,y) = moby();  // assign both
    end;

Bare Methods
============

Nameless methods may be declared inline. Such :term:`bare methods` are
typically used as parameters to other methods.  For example, the
following code fragment squares each element of a list using the built
in ``map`` function and a bare method:

.. code-block:: dylan

    define method square-list (numbers :: <list>) => (out :: <list>)
      map(method(x) x * x end, numbers);
    end;

The ``map`` function takes each element of
the list ``numbers`` and applies the anonymous method. It
then builds a new list using the resulting values and returns it.
The method ``square-list`` might be invoked as
follows:

.. todo:: Must distinguish return values from code.

.. code-block:: dylan

    square-list(#(1, 2, 3, 4));
    => #(1, 4, 9, 16)

Local Methods
=============

Local methods resemble bare methods but have names. They are
declared within other methods, often as private utility routines.

.. code-block:: dylan

    define method sum-squares (in :: <list>) => (sum-of-element-squares :: <integer>)
      local method square (x)
              x * x
            end,
            method sum (list :: <list>)
              reduce1(\+, list)
            end;
      sum(map(square, in))
    end;

Local methods can outlive the invocation of the
function which created them. parameters of the parent function remain
bound in a local method, allowing some interesting techniques:

.. code-block:: dylan

    define method build-put (string :: <string>) => (res :: <function>)
      local method string-putter()
              format-out(string);
            end;
      string-putter   // return local method
    end;

    define method print-hello () => ()
      let f = build-put("Hello!");
      f()  // print "Hello!"
    end;

Local functions which contain references to local variables that are
outside of the local function's own scope are known as
:term:`closures`.  In the above example, ``string-putter`` "closes
over" (or captures the binding of) the variable named ``string``.

.. _generic-functions:

Generic Functions
=================

A :term:`generic function` represents zero or more
similar methods. Every method created by means of ``define
method`` is automatically :term:`contained`
within the generic function of the same name. For example, a 
programmer could define three methods named ``display``,
each of which acted on a different data type:

.. code-block:: dylan

    define method display (i :: <integer>)
      do-display-integer(i);
    end;

    define method display (s :: <string>)
      do-display-string(s);
    end;

    define method display (f :: <float>)
      do-display-float(f);
    end;

When a program calls ``display``, Dylan examines
all three methods. Depending on the type of the argument to
``display``, Dylan invokes one of the above methods.
If no methods match the actual parameters, an error occurs.

In C++, this process occurs only at compile time. (It's called
operator overloading.) In Dylan, calls to ``display``
may be resolved either at compile time or while the program is actually
executing. This makes it possible to define methods like:

.. code-block:: dylan

    define method display (c :: <collection>)
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
methods to single parameter and no return values:

.. code-block:: dylan

    define generic display (thing :: <object>) => ()

Generic functions are explained in greater detail in the chapter on
:doc:`multiple dispatch <multiple-dispatch>`.

Keyword Arguments
=================

Functions may accept :term:`keyword arguments`,
extra parameters which are identified by a label rather than by their
position in the argument list. Keyword arguments are often used in a
fashion similar to :term:`default parameter values`
in C++, and they are always optional.

The following hypothetical method might print records to an output device:

.. code-block:: dylan

    define method print-records
        (records :: <collection>, #key init-codes = "", lines-per-page = 66)
     => ()
      send-init-codes(init-codes)
      // ...print the records
    end method;

The arguments following ``#key`` are keyword arguments. You could call this
method in several ways:

.. code-block:: dylan

    print-records(recs);
    print-records(recs, lines-per-page: 65);
    print-records(recs, lines-per-page: 120, init-codes: "***42\n");

The first line calls the method without using any of the keyword arguments. The
second line uses one of the keyword arguments and the third uses both. Note
that the order of the keyword arguments does not matter.

With all three calls, the ``init-codes`` and ``lines-per-page`` variables are
available in the body of the method, even though keyword arguments are omitted
in two of the calls. When a keyword argument is omitted, it is given the default
value specified in the method definition. Therefore, in the first call, the 
``lines-per-page`` variable has the value ``66``, and in the first and second
calls, the ``init-codes`` variable has the value ``""``.

Programmers have quite a bit of flexibility in specifying keyword arguments.

* The default value specifier (e.g. the ``= 66`` above) may be omitted, in
  which case ``#f`` is used.
* The type of the keyword argument may be specified or omitted, just as with
  regular arguments.
* The keyword name can be different from the variable name used in the body of
  the method—a handy tool for preventing name conflicts.
* The default value specifier can be a complex expression, and it can even use
  earlier parameters.
* They keyword arguments allowed or required by each method can be specified by
  the generic function. For more on this, see `Parameter Lists and Generic
  Functions`_ below.

The following method uses some of these features:

.. code-block:: dylan

    define method subseq
        (seq :: <sequence>, #key start :: <integer> = 0, end: _end :: <integer> = seq.size)
      assert(start <= _end, "start is after end");
      ...
    end;

Firstly, the ``start:`` and ``end:`` keyword arguments are both specialized as
``<integer>``. The caller can only supply integers for these parameters.
Secondly, the ``start:`` keyword argument is associated with the ``start``
variable in the body of the method as usual, but because the Dylan language
does not allow a variable named ``end``, that keyword argument is instead
associated with the ``_end`` variable. Finally, if the ``end:`` keyword argument
were omitted, the value of the ``_end`` variable would be the size of the
``seq`` argument/variable.

Rest Arguments
==============

An argument list can also include ``#rest``, which is used with a variable name:

.. code-block:: dylan

   define method format (format-string, #rest format-parameters)
     ...
   end method

Any extra arguments are passed to the body of the method as a
:drm:`<sequence>` in the specified variable. For example, if the above method
were called like so:

.. code-block:: dylan

   format("Today will be %s with a high of %d", "cloudy", 52);
   
The ``format-parameters`` variable in the body of the method would have the
value ``#[ "cloudy", 52 ]``.

Parameter Lists and Generic Functions
=====================================

A generic function restricts the parameter lists of its methods, but methods
can expand on the generic function's parameter list if the generic function
allows it. This section describes how that works. It is a little more advanced
than rest of this introduction, so you may want to skip this section for now
and refer back to it later.

We described the ``#key`` and ``#rest`` parameter list tokens above. The
``#key`` token may also be used by itself, e.g., ``define method foo (arg,
#key)``. And there is a third parameter list token, ``#all-keys``, that
indicates that a method permits other keyword arguments than those listed.
These features are only useful when working with a generic function and its
family of methods. When used together, these tokens must appear in the order
``#rest``, ``#key``, ``#all-keys``.

The table below shows the different kinds of parameter lists that a generic
function can have, and what effect each has on the parameter lists of the
methods that it contains.

   +-----------------------------------+-------------------------------------------------------+
   | Generic function's parameter list | Methods' parameter lists                              |
   |                                   +-----------+---------------+---------------+-----------+
   |                                   | ``#key``  | ``#key a, b`` | ``#all-keys`` | ``#rest`` |
   +===================================+===========+===============+===============+===========+
   | ``(x)``                           | Forbidden | Forbidden     | Forbidden     | Forbidden |
   +-----------------------------------+-----------+---------------+---------------+-----------+
   | ``(x, #key)``                     | Required  | Allowed       | Allowed       | Allowed   |
   +-----------------------------------+-----------+---------------+---------------+-----------+
   | ``(x, #key a, b)``                | Required  | Required      | Allowed       | Allowed   |
   +-----------------------------------+-----------+---------------+---------------+-----------+
   | ``(x, #key, #all-keys)``          | Required  | Allowed       | Automatic     | Allowed   |
   +-----------------------------------+-----------+---------------+---------------+-----------+
   | ``(x, #key a, b, #all-keys)``     | Required  | Required      | Automatic     | Allowed   |
   +-----------------------------------+-----------+---------------+---------------+-----------+
   | ``(x, #rest r)``                  | Forbidden | Forbidden     | Forbidden     | Required  |
   +-----------------------------------+-----------+---------------+---------------+-----------+
   
   Required:
      Each method must have this element in its parameter list.
   Allowed:
      Each method may have this element in its parameter list, but is not
      required to.
   Forbidden:
      No method may have this element in its parameter list.
   Automatic:
      Each method effectively has ``#all-keys`` in its parameter list, even if
      it is not present.

This table shows the different kinds of parameter lists that a method can have,
what the ``r`` variable contains for each, and which keywords are permitted by
each. It is a run-time error to call a method with a keyword argument that it
does not permit.

   ======================================  =================  =========================  ======================
   Method's parameter list                 Contents of ``r``  Permits ``a:`` and ``b:``  Permits other keywords
   ======================================  =================  =========================  ======================
   ``(x)``                                 —                  No                         No            
   ``(x, #key)``                           —                  If next method permits     If next method permits
   ``(x, #key a, b)``                      —                  Yes                        If next method permits
   ``(x, #key, #all-keys)``                —                  Yes                        Yes           
   ``(x, #key a, b, #all-keys)``           —                  Yes                        Yes           
   ``(x, #rest r)``                        Extra arguments    No                         No            
   ``(x, #rest r, #key)``                  Keywords/values    If next method permits     If next method permits
   ``(x, #rest r, #key a, b)``             Keywords/values    Yes                        If next method permits
   ``(x, #rest r, #key, #all-keys)``       Keywords/values    Yes                        Yes           
   ``(x, #rest r, #key a, b, #all-keys)``  Keywords/values    Yes                        Yes           
   ======================================  =================  =========================  ======================

   Extra arguments:
      The local variable ``r`` is set to a :drm:`<sequence>` containing all the
      arguments passed to the method beyond the required arguments (i.e., the
      sequence will not contain ``x``).
   Keywords/values:
      The local variable ``r`` is set to a :drm:`<sequence>` containing all the
      keywords and values passed to the method. The first element of the
      sequence is one of the keywords, the second is the corresponding value,
      the third is another keyword, the fourth is its corresponding value, etc.
   If next method permits:
      The method only permits a keyword if some other applicable method permits
      it. In other words, it permits all the keywords in the :drm:`next-method`
      chain, effectively inheriting them. This rule is handy when you want to
      allow for future keywords that make sense within a particular family of
      related classes but you do not want to be overly permissive.

To illustrate the "next method" rule, say we have the following definitions:

.. code-block:: dylan
   
   define class <shape> (<object>) ... end;
   define generic draw (s :: <shape>, #key);
   
   define class <polygon> (<shape>) ... end;
   define class <triangle> (<polygon>) ... end;

   define class <ellipse> (<shape>) ... end;
   define class <circle> (<ellipse>) ... end;

   define method draw (s :: <polygon>, #key sides) ... end;
   define method draw (s :: <triangle>, #key) ... end;
   
   define method draw (s :: <ellipse>, #key) ... end;
   define method draw (s :: <circle>, #key radius) ... end;

The ``draw`` methods for ``<polygon>`` and ``<triangle>`` permit the ``sides:``
keyword. The method for ``<triangle>`` permits ``sides:`` because the method for
``<polygon>`` objects also applies to ``<triangle>`` objects and that method
permits ``sides:``.

However, the ``draw`` method for ``<circle>`` only permits the ``radius:``
keyword, because the ``draw`` method for ``<polygon>`` does not apply to
``<circle>`` objects — the two classes branch off separately from ``<shape>``.

Finally, the method for ``<ellipse>`` does not permit the ``radius:`` keyword
because, while a circle is a kind of ellipse, an ellipse is *not* a kind of
circle. ``<circle>`` does not inherit from ``<ellipse>`` and the ``draw`` method
for ``<circle>`` objects does not apply to ``<ellipse>`` objects.

For more information on keyword arguments, especially their use
with :ref:`generic functions <generic-functions>`, see the DRM.
