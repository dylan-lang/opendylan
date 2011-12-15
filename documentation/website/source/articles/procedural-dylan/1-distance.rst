A Simple Function: Distance
---------------------------

To get the flavor of the language, let us first consider a few simple
Pascal subroutines and their equivalents in Dylan. Here we have a Pascal
function for calculating the distance between two points in a two
dimensional plane, using the Pythagorean theorem:

.. code-block:: pascal

      function distance(x1: real; y1: real; x2: real; y2: real): real;
      begin
        distance := sqrt((x2-x1) * (x2-x1) + (y2-y1) * (y2-y1))
      end;

and here's an equivalent function in Dylan:

.. code-block:: dylan

      define method distance (x1 :: <real>, y1 :: <real>, x2 :: <real>, y2 :: <real>)
       => distance :: <real>;
        sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
      end method distance;

Let's first look at the syntactic differences between the two functions.
First, Pascal introduces the definition with the keyword ``function``
where Dylan uses the phrase ``define method``. ``Define`` is used in
Dylan when defining all *module variables*, which are the equivalent of
program variables, types, functions, and procedures in Pascal. We'll see
later why Dylan calls this function a *method*; for now, it is useful to
think of methods in Dylan as the same as functions in Pascal.

The next thing to notice is that Dylan uses the double colon symbol
(``::``) to separate variables from their types; we'll see later that a
single colon is used for another purpose, which is why the double colon
is used in type declarations. Also note that the Dylan program has a
space between the variable name and the double colon.

The next important difference between the two examples is that the name
of the type, ``real``, is enclosed in "angle brackets" (the less-than
and greater-than signs) in the Dylan version. The reason for this is
that, unlike in Pascal, types and variables live in the same *name
space* in Dylan. For example, in Pascal, one can have a type named
``list`` and a variable named ``list`` at the same time, and the meaning
of the identifier ``list`` refers to one or the other depending on
context. In Dylan, there isn't always enough context to know whether
something is a type or value -- and, in fact, types *are* values in
Dylan -- so a convention is used to separate types from non-types in
order to avoid confusion and name clashes. That convention is putting
angle brackets around the names of types. (C programmers face a similar
problem, and at least two distinct conventions have evolved: using
identifiers with the first letter capitalized or with a suffix of ``_t``
for type names.)

The convention for type names brings up another difference between Dylan
and Pascal: What characters are legal in names? Pascal, C, and many
traditional languages restrict the set of characters allowed in names to
the letters, digits, and perhaps a few graphical characters, such as the
underscore or dollar sign. Dylan is more flexible: all those characters
are legal, but so are many graphical characters, such as hyphens,
asterisks, question marks, exclamation marks, and, as we have seen, the
greater-than and less-than signs. The general rule is that a Dylan
identifier can't start with a digit and should contain at least one
letter, but any characters that don't otherwise have special meaning are
allowed.

This flexibility does come at some cost: because the character set for
variable names overlaps with the character set for operators, we need to
put spaces between variable names and operators. For example,
``x2 - x1`` is two two-character long variables separated by the minus
operator and means the same thing as it does in Pascal, but ``x2-x1`` is
a single five-character long variable name in Dylan. This may seem
confusing or awkward at first to programmers not used to putting space
between variables and operators, but it adds flexibility in the choice
of names.

Now back to the example function. In Pascal, the return type of the
function is declared with a colon, just like the type of a variable,
where Dylan uses an arrow (``=>``) combined with what looks like a
variable declaration. The return description for our function says that
the function returns the distance, and that the result has the type
``<real>``. The name used in a return description is for documentation
purposes only. Upon reflection, one might come to the conclusion that
the name is unnecessary as documentation, since the function name should
describe the meaning of the return value. Later, we'll see a reason why
Dylan puts the name in the result.

Let's skip the contents of the function body of the function for a
moment, and notice that Pascal surrounds the body with the words
``begin`` and ``end``. Dylan is similar, except that the word ``begin``
is omitted. All methods must have bodies, so where it begins is
implicit: right after the declaration of parameters and return values.
The word ``method`` and the name of the method are repeated after the
word ``end``; both ``method`` and the name are optional after the
``end``, but I personally prefer to put them in, so I've done that here.
Individual styles vary, and people who think that the words after
``end`` clutter the program are free to leave them out.

Now, let's look at the bodies of the functions. Aside from the spaces
separating the operators from the variable names in the Dylan version,
there is one major difference. In Pascal, the pseudo-variable with the
same name as the function, ``distance``, is assigned the value to return
from the function. In Dylan, on the other hand, the body is treated as
an expression, and the value of that expression is returned by the
function. If that body contains multiple statements or expressions
separated by semicolons, the function returns whatever the last
expression returns. (C is different from both Dylan and Pascal in this
regard: it uses the ``return`` statement to both return the value and
exit immediately from the function.)

The last important detail to notice is that, aside from the issue of
spaces, the expression used to calculate the distance is the same in
both Dylan and Pascal.

--------------

In Pascal, the programmer has to declare types for all variables,
parameters, and functions. In Dylan, type declarations may be omitted;
if no type is declared for a variable, any value may be used. Similarly,
the description of what a method returns may be left out. Thus a more
concise version of distance would be:

.. code-block:: dylan

      define method distance (x1, y1, x2, y2)
        sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
      end method distance;

When used with four real numbers, this distance function produces the
same result as the original. But this version will work just as well on
integers. (In fact, the original version also works on integers, because
the type ``<integer>`` is a *subtype* of ``<real>``; that is, all
integers are also reals, which matches our mathematical intuition. The
type ``<float>`` in Dylan corresponds more directly with Pascal's
``real`` type.)

Since any value can be used as an argument to distance, what happens if
we use something which doesn't make sense, like
``distance(0, 0, 3, "four")``? The function starts as normal, but when
it tries to subtract ``0`` from ``"four"``, the program stops with an
error, since it makes no sense to subtract a number from a string. If
we had used the original version of the distance function, we would have
gotten the error when we tried to call distance.

For now, we'll omit most type declarations in our Dylan examples. Later,
we'll see some of the reasons for using them other than catching errors.

--------------

Looking back at the distance function, we see that the subtraction
``x2 - x1`` is done twice in order to multiple the result by itself; the
same is then done for the y values. To reduce the work that the function
does, we can store the results of the subtractions in local variables.
(Many compilers will do this kind of optimization -- known as *common
subexpression elimination* -- without requiring the code to be
rewritten.) In Pascal, that might look like:

.. code-block:: pascal

      function distance(x1: real; y1: real; x2: real; y2: real): real;
      var
        deltaX, deltaY: real;
      begin
        deltaX := x2 - x1;
        deltaY := y2 - y1;
        distance := sqrt(deltaX * deltaX + deltaY * deltaY)
      end;

which could be written in Dylan as

.. code-block:: dylan

      define method distance (x1, y1, x2, y2)
        let delta-x = x2 - x1;
        let delta-y = y2 - y1;
        sqrt(delta-x * delta-x + delta-y * delta-y)
      end method distance;

First, we see that where Pascal puts the definition of the local
variables in a separate section of the function definition, Dylan puts
them in the body. Next, we see that the definition of the local
variables includes the initialization. In general, when you define a
variable in Dylan, you give it a value at the same time -- in this way,
you do not have to worry about initialized variables.

Also notice that where, in Pascal, ``mixedCaseNames`` are often used to
separate words in long identifiers, Dylan conventionally uses
``hyphen-separated-names``.

--------------

Another way to rewrite the distance function to only do the
subtractions once would be to abstract out the squaring operation as a
local function. In Pascal, this would look like:

.. code-block:: pascal

      function distance(x1: real; y1: real; x2: real; y2: real): real;
        function square(n: real): real;
        begin
          square := n * n
        end;
      begin
        distance := sqrt(square(x2 - x1) + square(y2 - y1))
      end;

which could be written in Dylan as

.. code-block:: dylan

      define method distance (x1, y1, x2, y2)
        local method square (n :: <real>)
                n * n
              end method square;
        sqrt(square(x2 - x1) + square(y2 - y1))
      end method distance;

Like local variables created with ``let``, local methods can appear
anywhere inside a body.

--------------

`Next -- Conditions and Multiple Values: The Quadratic
Formula <2-quadratic.html>`_

Copyright © 1995 Paul Haahr. All rights reserved.
