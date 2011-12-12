**********
Why Dylan?
**********

What earthly reason could there be for learning yet
*another* computer language?  And why should that language be
Dylan?

Dylan has an interesting combination of features. It is a dynamic
language, but is designed to perform nearly as well as a static language.
It is a :ref:`functional <functional-languages>` language -- like
Scheme or TCL -- but uses an algebraic infix syntax similar to C's.
Dylan is object-oriented from the ground up, supports multiple 
inheritance and exceptions, implements :doc:`multiple dispatch
<multiple-dispatch>`, and :ref:`collects garbage <garbage-collection>`.

Dynamic vs. Static Languages
============================

:term:`Static` languages need to know the type
of every variable at compile time. Examples of static languages include
C, Pascal, and Eiffel. Code written in static languages typically
compiles efficiently, and strong type-checking at compile-time reduces
the risk of errors.

:term:`Dynamic` languages allow the programmer to
create variables without explicitly specifying the type of information
they contain. This simplifies prototyping and cleans up certain kinds
of object oriented code. Typical dynamic languages include LISP, Perl,
and SmallTalk.

Dylan provides a good balance between the advantages of static
and dynamic languages. The programmer may choose to specify or omit
type declarations as desired. Code using explicit variable types can
be compiled very efficiently, and type mismatch errors can be caught
at compile time. Code omitting those type declarations gains the
flexibility of a dynamic language.

.. _functional-languages:

Functional Languages
====================

:term:`Functional` languages, such as LISP,
Scheme and to a large extent TCL, view an entire program as one large
function to be evaluated. Expressions, statements and even control
structures all return values, which may in turn be used as arguments
elsewhere.

Dylan is a functional language, permitting programmers to write
functions like the following:

.. code-block:: dylan

    define method shoe-size(person :: <string>)
      if (person = "Larry")
        14;
      else
        11;
      end if;
    end method;

The function ``shoe-size`` has one argument,
a string, and an untyped return value. (If this function didn't link
against external code, the compiler could easily infer the return
type.)  If ``person`` equals ``"Larry"``,
then the ``if`` statement evaluates to 14, otherwise
it returns 11. Since no other statements follow the ``if``,
its return value is used as the return value of the entire
function.

The same function could also have been written as follows, in a
more :term:`imperative` idiom:

.. code-block:: dylan

    define method shoe-size(person :: <string>)
      let the-size = 11;
      if (person = "Joe")
        the-size := 14;
      end if;
      the-size;
    end method;

Algebraic Infix Syntax
======================

Languages based on LISP typically use a notation called
:term:`fully-parenthesized prefix syntax`. This consists
of innumerable nested parentheses, as seen in the following Scheme
version of the ``shoe-size`` function:

.. code-block:: scheme

    (define (shoe-size person)
      (if (equal? person "Joe")
        14
        11))

This has a certain elegance, but takes some time to learn to read. Dylan,
as shown in :ref:`the previous section <functional-languages>`, uses a
syntax similar to those of C and Pascal.

Object Orientation
==================

Unlike many other object-oriented languages, Dylan uses objects
for every data value. Integers and strings are objects, as are
functions and classes themselves.

Dylan's design makes this reasonably efficient. Compile-time analysis
and explicit :ref:`type declarations <type-declarations>` allow the
compiler to optimize away most of the overhead. Other language features
permit the programmer to mark certain classes as :term:`sealed`, that is,
ineligible for further subclassing.

Dylan's object model, detailed in the following sections of this
tutorial, differs from that of C++ in several important respects.
Multiple inheritance may be used freely, without concern for
:term:`object slicing`, erroneous down-casting or a
whole host of other gotchas familiar to C++ programmers. Methods are
separate from class declarations, allowing a programmer to write new
polymorphic functions without editing the relevant base class.  Methods
may also dispatch polymorphically on more than one parameter, a
powerful technique known as :term:`multiple dispatch`.
All of these features will be explained in greater detail later on.

.. _garbage-collection:

Garbage Collection
==================

Languages with :term:`garbage collection` have no need of a ``free`` or
``delete`` operator, because unused heap memory gets reclaimed automatically
by the language runtime. This reduces the complexity of source code,
eliminates the need of keeping reference counts for shared objects,
and prevents most memory allocation bugs and all memory leaks.

Over the years, garbage collection has gained a reputation for
inefficiency. A large, object-oriented LISP program performed
terribly compared to hand coded, micro-optimized assembly, and a good
portion of the blame was placed on garbage collection.

Times have changed, however. Garbage collection technology has
improved. Processors speed has increased enormously. Most importantly,
however, the standard practice of the industry has changed, and large
commercial software is now built in C++.

No good benchmarks exist for the relative performance of large
C++ systems (greater than 15 thousand lines of code or so), and
similar systems *designed from the ground up* to use
garbage collection. The benchmarks which do exist typically test the
performance of relatively small pieces of code -- small enough
that one programmer can optimize the overall usage of memory -- or
have compared a good system without garbage collection to a direct
reimplementation of that system using a garbage collector. Overall,
no one seems to know just how fast GC is, relative to a typical large
C++ program. It *is* known, however, that good
GC code uses different designs than non-GC code, and often spends less
time needlessly copying data.

Why Not Dylan?
==============

Dylan's greatest weakness is the lack of a battle-hardened compiler
and IDE.

Even when good Dylan environments become available, experience
suggests that Dylan applications will use more RAM than programs
written in traditional languages.
