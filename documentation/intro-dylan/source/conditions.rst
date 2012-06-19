**********
Conditions
**********

Dylan offers sophisticated exception handling, allowing programs
to recover smoothly from error conditions. Like C++, Dylan represents
errors with objects. Dylan also supports advisory warnings and
potentially correctable errors.

When something unusual happens, a program can :term:`signal` a
:term:`condition`. :term:`Handlers` specify how to react to various
sorts of conditions.

Signaling
=========

Unlike the exceptions of C++ or Java, signaling a condition does *not* itself
cause the current function or block to exit. Instead, calling the :drm:`signal`
function is just like calling any other function. The ``signal`` function just
locates an appropriate handler and calls it normally.

One consequence of this is that a handler can signal another condition in a very
straightforward manner. For example, imagine a program that searches for a
person by name, and if it cannot find one, it searches for a pet by the same
name, and if it cannot find the pet either, it breaks into the debugger. Given
an unknown name, you might see the following backtrace:

1. ``break({condition <key-not-found-error>: "Toby"})``
2. ``handle-no-pet-found({condition <key-not-found-error>: "Toby"})``
3. ``signal({condition <key-not-found-error>: "Toby"})``
4. ``element(*pets*, "Toby")``
5. ``find-pet("Toby")``
6. ``handle-no-person-found({condition <key-not-found-error>: "Toby"})``
7. ``signal({condition <key-not-found-error>: "Toby"})``
8. ``element(*people*, "Toby")``
9. ``find-person-or-pet("Toby")``

Here you can see that each failure signals a new condition, but the program never
backs out of a function call; it just keeps going, leaving the history of
conditions for you to examine.

Handlers
========

A function :term:`establishes a handler` with the :drm:`let handler
<let_handler>` statement. The handler remains in effect until the function
exits. Other functions called by the first can establish new handlers. When the
``signal`` function looks for a handler, it looks for the most recently
established handler that fits the condition.

In the example above, there are two handlers: ``handle-no-person-found`` and
``handle-no-pet-found``. Both handlers are for the ``<key-not-found-error>``
condition. Let us assume that the ``find-person-or-pet`` function established
the ``handle-no-person-found`` handler and that the ``find-pet`` function
established the ``handle-no-pet-found`` handler. Since ``handle-no-pet-found``
was established later, it was the one chosen and called by ``signal`` in frame
3.

The code to establish the handlers may have looked like this:

.. code-block:: dylan

   let handler <key-not-found-error> = handle-no-pet-found;
   
A handler can be a normal function, but it can also be a local method or bare
method, complete with access to local variables.

Recovery
========

Dylan's condition system allows it to offer a couple of useful error recovery
techniques.

Returning from ``signal``
-------------------------

Because a ``signal`` call is just like any other function call, it can return
values. It returns whatever values the handler function returns. In the above
example, ``signal`` never returns because we break into the debugger, and the
``element`` function wouldn't do anything with the value if it did return, but
your own code could call ``signal`` and handle any return values appropriately.

This technique allows you to use conditions as a sort of callback. You can
establish a condition handler that returns a rarely-needed value, and another
deeply nested function could retrieve that value if needed by signaling that
condition and then taking the return value of the ``signal`` function.

Restart handlers
----------------

You can recover from a problem by returning a fall-back value from the
``signal`` function, but that technique has limitations. It does not provide
much encapsulation or allow for complicated recovery information, and the
recovery information has to be processed locally.

Another way to return recovery information is through the use of a
:term:`restart`. A restart is a condition that includes recovery information.
But unlike most conditions, this condition provides a solution instead of
indicating a problem. A restart handler — which may be established anywhere
useful — can use the information included in the restart to work around the
problem.

For example, if the ``find-pet`` function above does not succeed, the
``handle-no-pet-found`` function could create a new goldfish object and signal a
``<possible-new-pet>`` restart, returning the goldfish. The callers of
``find-pet`` would establish a handler for that restart. The restart handler
established by the ``find-person-or-pet`` function would probably ignore the
goldfish and signal a different condition instead, but other callers may
establish different restart handlers with the appropriate behavior.

Regardless, when the restart handler finishes, it returns, and then its caller
returns, and so on until the original ``signal`` function returns, at which
point the program resumes work where it left off. You cannot use restart
handlers or conditions to escape the program's normal flow of control. For that,
Dylan offers blocks.

Blocks
======

A :term:`block` is a group of statements. As with
other control structures, it may return a value. A simple block
might appear as follows:

.. code-block:: dylan

    block ()
      1 + 1
    end; // returns 2

But in addition to returning a value normally, a block can use a :term:`nonlocal
exit`. This allows the block to exit at any time, optionally returning a value.
In some ways, it is similar to the ``goto`` statement, the ``break`` statement,
or the POSIX ``longjmp`` function. To use a nonlocal exit,
specify a name in the parentheses following a ``block`` statement. Dylan
binds this name to an :term:`exit function` which can be
called from anywhere within the block or the functions it calls. The
following block returns either ``"Weird!"`` or ``"All's well."``,
depending on the color of the sky.

.. code-block:: dylan

    block (finished)
      if (sky-is-green())
        finished("Weird!");
      end;
      "All's well."
    end block;

Many programs need to dispose of resources or perform other cleanup work when
exiting a block. The block may contain optional ``afterwards`` and ``cleanup``
clauses. Neither affects the block's return value. The ``afterwards`` clause
executes if the block ends normally without using its nonlocal exit, and the
``cleanup`` clause executes when the block ends whether it ends normally or via
nonlocal exit.

.. code-block:: dylan

    let fd = open-input-file();
    block (return)
      let (errorcode, data) = read-data(fd);
      if (errorcode)
        return(errorcode);
      end if;
      process-data(data);
    afterwards
      report-success();
    cleanup
      close(fd);
    end;

Blocks and conditions
---------------------

In addition to the ``afterwards`` and ``cleanup`` clauses, a block may also
contain any number of ``exception`` clauses. The exception clauses establish handlers for
a condition much like the ``let handler`` statement, but before they run the
handler calls the block's exit procedure and takes a nonlocal exit. In other
words, it takes a short cut out of the normal flow of control. The ``signal``
function that signaled the condition never returns to its caller. Instead, the
program resumes execution after the block.

The end result is similar to the ``try...catch...finally`` statements of C++ or
Java:

.. code-block:: dylan

    let fd = open-input-file();
    block ()
      let data = read-data(fd);
      process-data(data);
    cleanup
      close(fd);
    exception (error :: <file-error>)
      report-problem(error);
    end;
   
You can use a block with a restart to abort some work entirely and fall back to
the data supplied by the restart object, neatly circumventing the problem mentioned
at the end of the `Restart handlers`_ section above:

.. code-block:: dylan

    let fd = open-input-file();
    block ()
      let data = read-data(fd);
      process-data(data);
    cleanup
      close(fd);
    exception (restart :: <fallback-data-restart>)
      process-data(restart.fallback-data);
    end;
