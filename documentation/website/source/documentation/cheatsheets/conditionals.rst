*********************
Conditionals in Dylan
*********************

``if``
======

:drm:`if` is pretty straightforward:

.. code-block:: dylan

    if (test-expression)
      // if test-expression is true
    elseif (other-test)
      // otherwise, if other-test is true
    else
      // If no tests above were true
    end;

``unless``
==========

:drm:`unless` executes a body of code when the test expression
is false:

.. code-block:: dylan

    unless (test-expression)
      // do work, if test-expression is false
    end;

``case``
========

:drm:`case` tests a series of conditions and executes the body
of code corresponding to the first condition that is true.

.. code-block:: dylan

    case
       player1.money <= 0
         => end-game(player1);
       player2.money <= 0
         => end-game(player2);
       otherwise
         => move(player1);
            move(player2);
    end case; 

``select``
==========

:drm:`select` takes a target object and compares it against
a series of matches and executes the body of code corresponding
to the first match found.

.. code-block:: dylan

    select (as-uppercase(char))
      'D' => collect(number-to-string(argument(char, <number>)));
      'B' => collect(integer-to-string(argument(char, <integer>), base: 2));
      'O' => collect(integer-to-string(argument(char, <integer>), base: 8));
      'X' => collect(integer-to-string(argument(char, <integer>), base: 16));
      'C' => collect-character(argument(char, <character>));
      'S' => print-pretty-name(buffer, argument(char, <object>));
      '=' => print-unique-name(buffer, argument(char, <object>));
      '%' => collect-character('%');
      otherwise =>
        error("Invalid format directive '%s' in \"%s\"",
              char, format-string);
    end;

``when``
========

This is just a short hand for ``if`` when there's no ``elseif`` or
``else`` branches.

.. code-block:: dylan

    when (test-expression)
      // if test-expression is true
    end;

``when`` isn't defined in the DRM. To use it, you must ``use`` the
``dylan-extensions`` module from the ``dylan`` library.
