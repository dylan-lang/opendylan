*********************
Iteration Cheat Sheet
*********************


Simple iteration with ``while`` and ``until``
=============================================

.. code-block:: dylan

    while (condition?)
      // do something while the test condition? is true
    end;

.. code-block:: dylan

    until (condition?)
      // do something until the test condition? is true
    end;

Read more in the DRM: :drm:`while` and :drm:`until`.

The ``for`` loop
================

The :drm:`for` loop can be used in many different ways. We demonstrate some of the
most commonly used features here:

Iterating over a collection
---------------------------

.. code-block:: dylan

    for (element in collection)
      // do something with element
    end;

Iterating over a range
----------------------

.. code-block:: dylan

    for (count from 0 below num)
      // do work
      // count ranges from 0 to the integer below num
    end;

    for (column from 1 to 3)
      // do work
      // count ranges from 1 to 3, inclusive.
    end;

    // Changing the stepping and going in reverse
    for (index from stop - 1 to start by -1)
      // index will start at 'stop - 1' and end at the
      // value of 'start', decrementing by 1 with each
      // iteration of the loop.
    end;

Iterating over a table
----------------------

The easiest way to iterate over a table is to use an extension to
the standard :drm:`for` loop that Open Dylan supports:

.. code-block:: dylan

    for (value keyed-by key in table)
      // do work
    end;

If you want to directly access the keys of the table, you can use
:drm:`key-sequence`:

.. code-block:: dylan

    for (key in table.key-sequence)
      // do work
    end;

Breaking out of a loop
======================

Breaking out of a loop is just like any other non-local exit in Dylan.
Combine any loop with a :drm:`block` expression:

.. code-block:: dylan

   let result = block (exit-block)
                  while (~done())
                    if (got-error?())
                      exit-block(1);
                    end;
                  end;
                  2
                end;

In the example, if the loop ends naturally because ``done()`` returns true,
then the result is ``2`` because the :drm:`while` exits naturally and ``2``
is the last expression in the block. If ``got-error?`` returns true, the
result is ``1`` because that was the value passed to ``exit-block``.

Collection Functions
====================

When working with a collection, some additional operations are available
that remove the need for explicit iteration over the collection.

In all of these, the function passed in can be any of:

* An existing function.
* An escaped operator name (``\+`` for example).
* A locally defined method.
* The result of a method that returns a function such as :drm:`curry`
  :drm:`rcurry` or other `functional operations <https://opendylan.org/books/drm/Functional_Operations>`_.

``do``
------

:drm:`do` iterates over one or more collections, performing side effects:

.. code-block:: dylan

    do(method (x)
         format-out("%s\n", x)
       end,
       #[1, 2, 3])

``map``, ``map-as``, ``map-into``
---------------------------------

:drm:`map` iterates over one or more collections, applying a function and
returns the results in a new collection.  :drm:`map-as` and :drm:`map-into`
allow control over the way that the results are returned.

.. code-block:: dylan

    let type-bindings = map(generate-type-binding, all-var-specs);

.. code-block:: dylan

    let strings = map(curry(as, <string>), names);

.. code-block:: dylan

    let c-direct-superclasses = map-as(<list>, convert, direct-superclasses(c));

Read more in the DRM: :drm:`map`, :drm:`map-as`, :drm:`map-into`.

``reduce``, ``reduce1``
-----------------------

:drm:`reduce` combines the elements of a collection and a seed value into
a single value by repeatedly applying a binary function.

:drm:`reduce1` is similar to :drm:`reduce`, except that the first value of
the collection is used as the seed value.

.. code-block:: dylan

    reduce(\*, 1, dimensions(x))

.. code-block:: dylan

    reduce1(\+, #(1, 2, 3, 4, 5))

``reduce`` is often combined with ``map`` operations:

.. code-block:: dylan

    reduce(\+, 0, map(size, qqs))

Iteration with Tail Recursion
=============================

The ``iterate`` macro in the ``common-dylan`` library is another powerful way to do
iteration. It relies on the fact that Dylan implementations are required to optimize
tail recursion.

.. code-block:: dylan

   let x = 7;
   let factorial = iterate loop (n = x, total = 1)
                     if (n < 2)
                       total
                     else
                       loop(n - 1, n * total)   // tail call = iteration
                     end
                   end;

Here ``iterate`` creates a local method, ``loop``, with two parameters ``n`` and
``total`` which calls itself recursively until ``n < 2`` is true and then it returns the
value of ``total``. It then calls the method with the parameter default values, ``x`` and
``1``.

You could of course do the same thing with a local method yourself but the ``iterate``
macro makes it more concise.
