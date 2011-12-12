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

Blocks
======

A :term:`block` is a group of statements. As with
other control structures, it may return a value. A simple block
might appear as follows:

.. code-block:: dylan

    block ()
      1 + 1;
    end; // returns 2

Blocks also support non-local exits. These allow a block to exit at
any time, optionally returning a value. In some ways, they are similar
to ``goto`` statements or the POSIX ``longjmp`` function. To use them,
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

Many programs need to dispose of resources or perform other cleanup
work, regardless of how a block is exited. Blocks may contain
an optional ``cleanup`` clause, which doesn't affect
the return value of the block and will always be executed.

.. code-block:: dylan

    let fd = open-input-file();
    block (return)
      let (errorcode, data) = read-data(fd);
      if (errorcode)
        return(errorcode);
      end if;
      process-data(data);
    cleanup
      close(fd);
    end;
