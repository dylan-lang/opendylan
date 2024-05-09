Conditions and Multiple Values: The Quadratic Formula
-----------------------------------------------------

Now we'll look at a slightly more complex function, one which finds the
roots of a quadratic equation. A Pascal version of such a function might
be:

.. code-block:: pascal

      (* Solve quadratic equation, putting roots in root1 and root2
         and returning the number of roots found. *)
      function solveQuadratic(a, b, c: real; var root1, root2: real): int;
      var
        discriminant: real;
        sqrtDiscriminant: real;
      begin
        discriminant := b * b - 4.0 * a * c;
        if discriminant < 0.0 then
          solveQuadratic := 0
        else if discriminant = 0.0 then
          begin
            solveQuadratic := 1;
            root1 := -b/(2.0 * a)
          end
        else
          begin
            solveQuadratic := 2;
            sqrtDiscriminant := sqrt(discriminant);
            root1 := (-b + sqrtDiscriminant)/(2.0 * a);
            root2 := (-b - sqrtDiscriminant)/(2.0 * a)
          end
      end;

There are a few things to note here. The first is the interface to the
function: since we want to return up to two separate numbers, but maybe
zero or one in the degenerate cases, we can't just use the normal
paradigm of returning the result, because Pascal lets functions accept
many values, but return only one. To work around this restriction, we
return an integer indicating how many roots there were, and use ``var``
parameters (also known as *call by reference parameters*) to pass the
actual results from the function back to its caller. In order for this
mechanism to work, the function which calls ``solveQuadratic`` must
define two variables for receiving the results. The caller also has to
be careful not to use the variable ``root2`` if there was only one root
returned. Other approaches, such as returning a record containing the
roots and the number of roots found, are also possible.

There are several other things worth mentioning. The local variable
``sqrtDiscriminant`` is defined for the entire function, even though it
is only used in the body of the second ``else`` clause. Because Pascal
is strongly typed and does not allow operations between integers and
real numbers, we have to write all the constants, even though they are
integers, as real numbers in decimal notation. Finally, we see that the
body of the first ``if`` clause does not have a ``begin`` and ``end``
because it is a single statement.

--------------

A Dylan function for the same operation would have a different
interface, at very least because Dylan does not have ``var``
parameters. Here's one approach:

.. code-block:: dylan

      // Solve quadratic equation, returning the roots
      define method solve-quadratic(a, b, c) => (root1, root2);
        let discriminant = b * b - 4 * a * c;
        if (discriminant < 0)
          values()
        elseif (discriminant = 0)
          values(- b / (2 * a))
        else
          let sqrt-discriminant = sqrt(discriminant);
          values((- b + sqrt-discriminant) / (2 * a),
                 (- b - sqrt-discriminant) / (2 * a))
        end if
      end method solve-quadratic;

The first thing we notice is the notation for comments. Dylan has two
forms of comments. The sequence ``//`` introduces a comment that
continues to the end of the line; all the intervening text is ignored by
the language. The sequence ``/*`` introduces a block comment that
encloses all the text up to a ``*/``; the enclosed text is ignored
except that pairs of ``/*`` and ``*/`` inside the comment are matched,
so block comments nest. The nesting property of block comments means
that it is safe to use ``/*`` and ``*/`` to comment out code even if
that code contains block comments. (Dylan shares this comment convention
with C++, except that in C++, block comments don't nest.)

The most significant difference between this function and the Pascal
version is what it returns. For the Dylan function, the comment says
"returning the roots," and that is what it does. In Pascal, C, Fortran,
and, in fact, most programming languages, a function can take an
arbitrary number of arguments but the number of values returned can be
only zero or one. (In Pascal, a function returning no values is called a
procedure.) In Dylan, functions can return as many values as are
appropriate for the task at hand, just as they can accept as many
arguments as make sense.

How does a function return multiple values? We saw earlier that a
function returns the value of the last expression that makes up its
body. There is a built-in function named values which returns all of its
arguments. By calling values as the last statement of a body, the
arguments passed to values are used as the value of the body. For
example, the following method could be used to return the sine and
cosine of an angle:

.. code-block:: dylan

      define method sincos (angle) => (sin :: <real>, cos :: <real>);
        values(sin(angle), cos(angle))
      end method sincos;

To use both values returned returned by ``sincos``, one uses a special
form of ``let`` which is said to *accept multiple values*:

.. code-block:: dylan

      let (s, c) = sincos(theta);
      ...

Inside the body of the let, the variable s will hold the sine of theta
and c the cosine. Note, by the way, the return declaration for
``sincos`` indicates that the method returns two real numbers. The
reason that names are used in return declarations is to document which
value is which in the case of multiple-value returning functions.

As a special but typical case, when you are calling a function that
returns more than one value and you are only interested in the first
value, you don't have to bind the values with let, you can just use the
function as an expression. In fact, there is no difference between
calling a function that returns one value and calling a function that
returns multiple values if you only want the first value returned.

Let's return to our quadratic equation solver. To see what the function
returns, we have to look for the last expression in the body. In this
case, the last statement is an ``if`` statement. In Dylan, an ``if``
statement is an expression and can be used anywhere a value is expected.
The value of an if statement is the value of the body of the branch that
is taken. For example, in the simple case of:

.. code-block:: dylan

      if (even?(n))
        "even"
      else
        "odd"
      end if

If the number ``n`` is even, the value of the if statement is the string
``"even"``; otherwise, the value is the string ``"odd"``. Incidentally,
Dylan is classified as an "expression language" because all statements
can return values and be used as expressions.

In Pascal and C, one can construct a series of tests by putting if
statements in the ``else`` clause of another ``if`` statement. In Dylan
``elseif`` is one word and not two, and the ``elseif`` branch is
actually part of the if statement. There can be any number of ``elseif``
clauses between the ``if`` clause and the optional ``else`` clause.

In ``solve-quadratic``, the last statement of every branch of the ``if``
statement is a call to ``values``. If the first test is true, ``values``
is called with no arguments, so the function returns no values. If the
test for the ``elseif`` clause is true, the function returns one value,
the single root. (In this case, the call to values is unnecessary, but
it is useful as documentation to contrast with the cases where zero and
two values are returned.) Finally, if there are two roots, both are
returned.

How is this function used? In particular, how does a caller determine
how many roots were found? If a caller is expecting more results than a
function returns, all the variables for which no value was returned are
given the false value. So, for example, we could write a function to
count the number of roots as

.. code-block:: dylan

      define method number-of-quadratic-roots(a, b, c)
        let (r1, r2) = solve-quadratic(a, b, c);
        if (r2)
          2
        elseif (r1)
          1
        else
          0
        end if
      end method number-of-quadratic-roots;

--------------

Before we leave this example behind, let's look at another way to write
it in Dylan, keeping the same interface:

.. code-block:: dylan

      // Solve quadratic equation, returning the roots
      define method solve-quadratic(a, b, c) => (root1, root2);
        let discriminant = b * b - 4 * a * c;
        case
          negative?(discriminant) =>
            values();
          zero?(discriminant) =>
            values(- b / (2 * a));
          otherwise =>
            let sqrt-discriminant = sqrt(discriminant);
            values((- b + sqrt-discriminant) / (2 * a),
                   (- b - sqrt-discriminant) / (2 * a))
        end case
      end method solve-quadratic;

There are two differences here. First, the ``if`` statement has been
rewritten as a ``case`` statement. A ``case`` statement contains a
series of tests and bodies, separated by the arrow symbol. The tests are
checked in order and the clause that corresponds to the first test which
evaluates to true is run, and the value of that clause is used as the
result of the whole statement. The last test in a ``case`` statement may
be ``otherwise``: the ``otherwise`` clause is used if none of the tests
are true. ``Case`` statements are often easier to read than long chains
of ``elseif`` statements, but choosing one or the other is simply a matter
of personal style.

Also, the tests have been changed from explicit comparisons with zero to
calls to function which do those tests. There is no real difference
between writing ``x < 0`` and ``negative?(x)``, but sometimes one form
is clearer than the other. A function which returns a boolean value is
called a *predicate* in Dylan, and, by convention, the names of
predicates end in question marks, as we can see from ``negative?`` and
``zero?``.

--------------

`Back -- A Simple Function: Distance <1-distance.html>`_ | `Next -- Iteration and Sequences: Dot Product <3-dot-product.html>`_

Copyright © 1995 Paul Haahr. All rights reserved.
