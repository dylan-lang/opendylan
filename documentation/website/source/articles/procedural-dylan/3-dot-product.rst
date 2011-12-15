Iteration and Sequences: Dot Product
------------------------------------

The next function we'll look at calculates the inner product, or *dot
product*, of two one-dimensional arrays of numbers. This is defined as
the sum of the pairwise products of numbers from the arrays. As an added
complication, we'll not fix the length of the arrays, so our Pascal
version will use conformant arrays, a late addition to the language.

.. code-block:: pascal

      function dotProduct(x: array[xlo..xhi: integer] of real,
                          y: array[ylo..yhi: integer] of real): real;
      var
        i: integer;
        sum: real;
      begin
        if (xhi - xlo <> yhi - ylo)
          error("dotProduct: arrays of different lengths");
        sum := 0.0;
        for i := xlo to xhi do
          sum := sum + x[i] * y[i - xlo + ylo];
        dotProduct := sum
      end;

Because a dot product only makes sense for arrays of the same length, we
need to check that the arrays have the same length before proceeding.
Since Pascal has no built-in error-handling facilities, we've invented a
minimal interface -- a procedure named ``error`` which takes a string
description of the error -- that has to be implemented on every
different system using non-standard facilities.

There is one other complication worth mentioning here. Since the two
arrays can have different lower bounds and the loop index ``i`` starts
at the value for the array ``x``, it has to be adjusted before being
used as a subscript for ``y``.

--------------

Our first Dylan version of this function uses a while loop:

.. code-block:: dylan

      define method dot-product (x, y)
        if (size(x) ~= size(y))
          error("dot-product: arrays of different lengths")
        end if;
        let sum = 0;
        let i = 0;
        while (i < size(x))
          sum := sum + x[i] * y[i]
        end while;
        sum
      end method dot-product;

Let's first look at the syntactic differences. Other than using ``~=``
for the "is not equal to" relationship, Dylan is very similar to Pascal
for the operations needed in this function. The operator ``:=`` is used
for assignment to an existing variable (as opposed to ``=``, which is
used for setting the initial value for a variable created with ``let``)
and the notation *array*``[``*index*``]`` is used for subscripting. As
with ``if``, the ``while`` statement is always ended with ``end``, and
no explicit ``begin`` is needed.

The main difference between these two functions is in how they
manipulate the arrays. When declaring an array in Pascal, a programmer
specifies both the lower and upper bound; these bounds can be integers,
characters, or enumerated types. In this regard, Dylan is less flexible
than Pascal: arrays are always indexed with increasing integers starting
at zero. (Many other languages, such as C, share this restriction.
Fortran is similar, but it starts all arrays at one.)

In standard Pascal, the length of all arrays must be known ahead of
time. The one exception to this is the conformant array feature of ISO
Pascal, where the bounds of the array are treated as an extra set of
parameters to a function which accepts an array as an argument. In
Dylan, all arrays carry with them their length, so we only have to
specify the size of an array when it is not already known, such as when
the array is first created. The function ``size`` is used to obtain the
length of the array. For any array ``a``, the valid indexes of the array
are from ``0`` to ``size(a) - 1``; that is, ``size(a)`` elements,
starting at zero.

One more thing to note is that, in Pascal, we had to invent the
``error`` procedure and say that it is implemented in some
platform-dependent way. Our Dylan version also calls a function named
``error``, but this function is a part of the Dylan language. The full
behavior of ``error`` is rather complicated and beyond the scope of this
tutorial -- Dylan has a powerful exception handling and recovery
mechanism for dealing with and possibly correcting errors discovered
while a program is running -- but if you're running the program in a
debugger, a call to error will generally result in the debugger stopping
the program and reporting the message.

--------------

The original Pascal version of this function used a ``for`` loop, which
has a direct equivalent in Dylan, as we see in this version of the
function:

.. code-block:: dylan

      define method dot-product (x, y)
        unless (size(x) = size(y))
          error("dot-product: arrays of different lengths")
        end unless;
        let sum = 0;
        for (i :: <integer> from 0 to size(x) - 1)
          sum := sum + x[i] * y[i]
        end for;
        sum
      end method dot-product;

We've changed the error check at the beginning of the function from an
``if`` statement to an ``unless``, which executes its body if the
condition is false. Sometimes, often with error checks, it is clearer to
write a conditional with ``unless`` than with an ``if`` that tests the
opposite condition. Note that there is no ``else`` clause for
``unless``; if you need to do one thing if something is true and another
if it is false, use ``if`` or ``case``.

The ``for`` loop in Dylan takes several forms, one of which we see
above, which is called a *numeric for clause*. The syntax of ``for`` in
Dylan is similar to the ``while`` statement, except instead of a
condition inside parentheses, we have a clause that describes the loop.
A numeric for clause counts from a number to some other. In this case,
the for-clause counts from zero to one less than the size of the array.
The syntax of the clause is straightforward:

    ``variable from initial-value to bounding-value``

As in Pascal's ``for`` statement and unlike the ``while`` loop, the
initial value and the bounding value are evaluated exactly once, before
the first iteration of the loop.

Unlike the Pascal ``for`` loop, Dylan's ``for`` defines the loop
variable -- ``i`` in this example -- as part of its job. That is, no
declaration of ``i`` outside the loop is needed or used. This also means
that the loop variable can't be used outside the body of the loop. (I
included a type declaration for ``i`` above, just to show that one could
be used; typically, it would be omitted.)

--------------

The for loop above is not very typical Dylan code. If written first in
Dylan, it might look more like this, taking advantage of a few
constructs that don't correspond directly to ones in Pascal:

.. code-block:: dylan

      define method dot-product (x, y)
        let sum = 0;
        for (i from 0 below x.size)
          sum := sum + x[i] * y[i]
        finally
          sum
        end for
      end method dot-product;

(We've eliminated the error check at the beginning of the function from
this and future versions, not because it is unnecessary, but because it
would stay the same -- there is no need to repeat it.)

We see here a slight variation on numeric iteration. The word ``to`` in
the iteration clause has been changed to ``below``. While ``to`` means
all values between the initial value through, and including, the final
value, ``below`` means all those values from the initial value that are
less than, but not equal to, the bounding value.

In addition to what we've seen here, numeric iteration clauses can use
the ``by`` keyword, which introduces an amount to count by, as in
Pascal. The ``by`` phrase can be omitted, as we've seen, in which case
the value is incremented by one each time through the loop. Also, we can
identify the ending condition with ``above``, the opposite of ``below``,
which means the loop runs while the values are greater than the bound.
The ending condition can even be omitted, which leaves a (potentially)
infinite loop -- something other than finishing the numeric iteration
would have to happen to terminate the loop.

The value specified by ``below`` in the loop is also a new construct,
``x.size``. This is a Dylan shorthand for calling a function of one
argument. That is, *expression``.``function-name* is the same thing as
*function-name``(``expression``)``*. So the expression here is exactly
the same as ``size(x)``, just written somewhat differently. (The
hecklers in the audience might observe that it isn't much of a
shorthand: just one fewer character.)

Pascal and C programmers, among others, are used to using the syntax
*expression*``.``*name* for referring to fields of a compound object
like a record. It makes sense to wonder why this notation was chosen for
something else in Dylan, especially when that something else -- calling
functions -- already had a perfectly reasonable syntax. The reason is
that calling a function in Dylan is the only way to access fields in
compound objects; we'll see more of this later.

The last thing to note about this version of the function is that the
``for`` statement is the last one in the function, so the value returned
by ``dot-product`` is the value of the ``for``. What value does the loop
return? Normally, there is no meaningful value to return from a loop, so
an arbitrary one -- the false value -- was picked in Dylan. But
sometimes it's useful to return a value from a loop, and that's what the
``finally`` clause is for. After the ``for`` loop is done, all the
statements in the ``finally`` clause -- here it's just one expression --
are evaluated, and the value of the last is returned. One detail worth
knowing is that the statements after ``finally`` are still part of the
``for`` statement, so the variable from the numeric iteration clause can
still be used.

--------------

All our versions of ``dot-product`` so far have worked in basically the
same way: step through the arrays by counting from 0 to one less than
the size of the array, and doing something with each element in the
array. This is all we use the variable ``i`` for, and it's pretty common
that loop variables are used in this way. Dylan provides a very
convenient way of doing this kind of iteration, using what's known as a
*collection iteration clause*:

.. code-block:: dylan

      define method dot-product (x, y)
        let sum = 0;
        for (xi in x, yi in y)
          sum := sum + xi * yi
        finally
          sum
        end for
      end method dot-product;

Here we've done away with the numeric iteration clause, and replaced it
with two collection iteration clauses. Note that a ``for`` loop may have
an arbitrary number of iteration clauses, and when any of them
completes, the loop is done. The iteration clause *variable* ``in``
*expression*, where the expression is an array, means execute the loop
for each element in the array, from first to last, setting the variable
to each value in succession. Thus the first time the loop executes,
``xi`` holds the value ``x[0]`` and ``yi`` holds ``y[0]``; the second
time through, ``xi`` holds ``x[1]`` and ``yi`` holds ``y[1]``; and so
on.

The use of multiple iteration clauses in a ``for`` is relatively common.
Note that a single loop can mix numeric and collection iteration clause,
along with a third form, general iteration, which we'll see later. The
important thing to realize is that the loop ends when *any* of the
clauses causes it to end.

Again, we haven't declared types for the parameters ``x`` and ``y`` and
have just referred to them as arrays. This is inaccurate in two ways.
The first, and more minor, is that one-dimensional arrays are referred
to as *vectors* in Dylan. While the code will work on multi-dimensional
arrays, it doesn't really make mathematical sense except for vectors --
there is no dot product of an array of two or more dimensions.

The more important reason that describing the arguments to
``dot-product`` as arrays is wrong is that this function will work for
other types of arguments as well. Dylan has several different types
known collectively as *sequences*, all of which share several
properties: they hold a collection of objects which are numbered from
zero. Vectors are one kind of sequence; others are strings, linked
lists, ranges of numbers, and queues. The subscripting notation using
``[]``, the ``size`` function, and collection iteration clauses -- among
many other operations -- may be used with all sequences.

Sequences are just one kind of *collection* in Dylan. A collection is a
kind of object which can hold other objects. The most commonly used
collection which isn't a sequence is a *table*, which is Dylan's
built-in version of a hash table. Where sequences use integers starting
at zero as indexes, tables can use any object.

Collections are used extensively by most Dylan programs. This should not
be a surprise to people who know Pascal: many complex data structures in
Pascal programs are collections of other objects, usually built up from
arrays, linked lists, etc. One major difference is that where Pascal
only defines the array type, in Dylan there are a variety of existing
collection types to use. Moreover, there is powerful set of operations
predefined on collections and sequences; we'll see two of these in our
next version of ``dot-product``.

Another aspect of collections is that users can define their own
collection types. Again, this is no surprise. What is different from
Pascal is that, when programmers follow a few rules when creating a new
kind of collection, it can be used with any of the predefined functions
that operate on collections. We'll see more about this in a later
section.

Turning back to the example at hand, there is one more thing to
understand about using collection iteration clauses instead of numeric
iteration and indexing: the performance impact of the choice. When we
used numeric iteration, to calculate the dot-product we had to evaluate
``x[i]`` and ``y[i]`` each time through the loop. For vectors this makes
perfect sense, as indexing into a vector is an efficient operation. But
if ``x`` is a linked list, for example, finding ``x[i]`` requires
stepping through the first ``i - 1`` elements of the list, which makes
evaluating ``x[i]`` inside the loop an expensive operation. In more
precise terms, it changes the computational complexity of the function
from *O(n)* for vectors to *O(n^2)* for lists, where *n* is the length
of the arguments.

On the other hand, collection iteration is designed to be efficient when
possible. For both linked lists and arrays, iterating through the
collection takes time proportional to the length of the list, so the
version of ``dot-product`` which uses collection iteration has *O(n)*
complexity for both vectors and lists. In general, one should use
collection iteration when iterating over the elements of a collection
without concern for what their index values are.

--------------

Now let's take a look at a very different version of ``dot-product``,
this time using two built-in collection operations, ``reduce`` and
``map``:

.. code-block:: dylan

      define method dot-product (x, y)
        local method add(a, b)
                a + b
              end method add;
        local method multiply(m, n)
                m * n
              end method mul;
        let x*y-sequence = map(multiply, x, y);
        reduce(add, 0, x*y-sequence)
      end method dot-product;

The thing to note about this function is that there is no explicit
iteration clause. Instead, the iteration is done by the built-in
functions.

Let's carefully step through what this function does. First, it defines
two local functions -- ``add`` and ``mul`` -- which, respectively, add
and multiply their arguments. It then calls the function ``map``.
``Map`` is what is known as a *higher-order function*, which means it is
a function that takes a function as an argument. ``Map`` creates a new
collection from the results of calling a function on each element of one
or more existing collections. The first argument to ``map`` is the
function to use; all the other arguments are collections to draw
elements from. In this case, we've applied ``map`` to the function
``multiply`` and the two sequences which were arguments, so it produces
a sequence that has ``x[0] * y[0]`` as its first element,
``x[1] * y[1]`` as its second, and so on. (The resulting sequence is
only as long as the shorter of the original sequences. If one is longer,
only the elements which have corresponding members in the shorter
sequence are used. As above, we'll assume that the sequences are of the
same length.)

The result of map, a sequence of the products of the elements of ``x``
and ``y`` is then attached to the local variable named ``x*y-sequence``.
There's nothing special about the use of the asterisk in this name --
it's just one of the characters which is legal in Dylan as part of a
name -- but sometimes it is convenient to name a variable with a
mathematical expression.

Next, we call the function ``reduce``, which is also a higher-order
function. Where ``map`` applies a function to the elements of one or
more collections to produce a new collection, ``reduce`` combines the
elements of a collection with a function to produce a single value.
``Reduce`` takes three arguments: a function, an initial value, and a
collection. It keeps track of the result value, which starts off as the
initial value argument. For each element of the collection, it computes
the result of the function called with the value of the result so far
and the current element, and uses that as the next value of the result.
When every element of the collection has been seen, the value that has
been computed so far is retuned.

In our call to ``reduce``, the function used for combining values is
``add``, which sums two numbers, and the initial value is ``0``, which
is the identity for addition. Therefore, this call to ``reduce``
produces the sum of the members of a collection. In this case, the
collection being summed contains the products of the elements of the
arguments, so the result is the dot product.

However, there is a potential drawback to using this approach for
implementing ``dot-product``: it might be less efficient than a version
that directly used a loop. The reason is that memory might have to be
allocated for the intermediary result ``x*y-sequence``, and allocating
that memory takes some time, as does freeing it when it is no longer in
use. (Dylan uses *garbage collection*, also known as *automatic storage
reclamation*, to free memory that isn't being used anymore, but whether
the programmer or the system does it, freeing the memory does take some
time. A full discussion of garbage collection is beyond the scope of
this essay.) So we potentially have a bigger (more memory) and slower
(more time) program. A very clever compiler could eliminate this use of
extra memory, perhaps by inlining the functions and rewriting them
internally as loops, but programmers concerned with efficiency probably
shouldn't count on such an optimization unless they know their compiler
very well.

By the way, there is nothing special about higher-order functions:
anyone can write them. Here, for example, is a portable version of
``reduce``, which could be used if it wasn't already part of the
language:

.. code-block:: dylan

      define method reduce
          (function :: <function>, initial, collection :: <collection>)
       => value;
        let result = initial;
        for (element in collection)
          result := function(result, element)
        finally
          result
        end for
      end method reduce;

This implementation is a direct translation into Dylan of the
description given above. For once, I've given types for the arguments,
because that's how the types are specified by the language definition.
The major difference from other functions we've seen so far is an
argument, ``function``, is used as a function, inside the ``for`` loop;
that works just as one might expect it to.

--------------

The previous version of ``dot-product`` used three locally-bound
identifiers (``add``, ``multiply``, and ``x*y-sequence``) that were only
used once. As in Pascal, if an expression is used only once, there is no
need to associate it with a name. Unlike Pascal, though, functions in
Dylan are expressions, and can be created anonymously, that is, without
names. So we can rewrite ``dot-product`` as:

.. code-block:: dylan

      define method dot-product (x, y)
        reduce(method (a, b) a + b end,
               0, map(method (m, n) m * n end, x, y))
      end method dot-product;

In this version of ``dot-product``, the function we use as the first
argument to ``reduce`` is the expression:

.. code-block:: dylan

      method (a, b)
        a + b
      end

This expression creates a method -- with exactly the same meaning as the
``add`` method from our previous version -- but doesn't give it a name.
Nonetheless, this method can be used anywhere ``add`` could have been.
Similarly, ``multiply`` has been changed to another anonymous method.

We also eliminated the variable ``x*y-sequence``, substituting the
``map`` expression for it in the call to ``reduce``. The meaning of the
function hasn't changed, however, so the potential inefficiency of
allocating extra memory for the result of ``map`` remains.

The full syntax for an anonymous method is

.. code-block:: dylan

      method (arguments ...) => results ...;
        body
      end method

and, as with all other methods, the description of the results and the
word ``method`` after ``end`` can be omitted. Since it doesn't have a
name, there is no name to put after the ``end method``.

--------------

These examples of ``map`` and ``reduce`` have been somewhat artificial
in order to introduce the use of higher-order functions. A version of
``dot-product`` in Dylan would more likely be written as:

.. code-block:: dylan

      define method dot-product (x, y)
        reduce(\+, 0, map(\*, x, y))
      end method dot-product;

In the previous two examples, we created a function which took two
arguments and added them. The first time it was named ``add``; the
second, it was anonymous and passed as the first argument to ``reduce``.
In fact, there is no real need to create such a function, because one
already exists: it's called ``+``.

Because ``+`` is normally used as an operator in traditional arithmetic
notation, it is easy to forget that it's a function. In Pascal and C, in
fact, operators like ``+`` have different rules from functions and can't
just be used like any other function, but in Dylan they can. To avoid
syntactic confusion with ``+`` used as an operator, when, for example,
we refer to the function name in order to pass it as an argument to
another function we have to precede it with a backslash, as above.

This function has the same result as the previous two, but we have
avoided introducing the intermediate functions and let ``map`` and
``reduce`` call ``*`` and ``+`` directly.

This version of ``dot-product`` is very concise and, for a programmer
used to ``map`` and ``reduce``, easy to read. There are some very common
idioms in Dylan built around these and other operators. For example,
there is no built-in function to sum a sequence: ``reduce`` obviates the
need for it.

The use of higher-order functions is usually associated with the
*functional* style of programming, which is identified with languages
such as Standard ML, Haskell, and some dialects of Lisp, notably Scheme.
In Dylan it is often convenient to use this style for parts of programs
-- often those dealing with operations on collections, as we've seen --
and use other styles, such as object-oriented or procedural, in other
parts. One of the goals for Dylan is to support multiple styles, because
there is no one single, "right" way to structure a program. The
functional style is a useful one, and is well supported in Dylan; we'll
see more of it later.

On the other hand, it is possible to take this approach to extremes and
write large, complicated expressions with no named intermediate values.
While some people enjoy writing large blocks of code that way, the
result is often an unreadable program. APL, another language which makes
pervasive use of higher-order functions, is often criticized for
encouraging a "write-only" programming style, with very complex
expressions. Introducing a few named values with ``let`` or splitting
one method into several that calculate different parts of a result can
often turn a large, hard-to-follow expression into something more
readable and maintainable.

--------------

`Back -- Conditions and Multiple Values: The Quadratic
Formula <2-quadratic.html>`_

Copyright © 1995 Paul Haahr. All rights reserved.
