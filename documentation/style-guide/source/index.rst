*****************
Dylan Style Guide
*****************

.. contents::
   :depth: 1
   :local:
   :backlinks: none

Purpose of this document
========================

This document describes a coding style that the Dylan group recommends.
There are style some areas of disagreement, and there is still room to
change this style guide. In some places the Dylan book group decided to
use a different style, which is noted in this guide.

Controversial comments
======================

-  return values indented two spaces like case (present in this guide,
   but not used in Dylan book style.)
-  ``#key`` indented two spaces like binary operators (present in this
   guide, but not used in Dylan book style.
-  keith likes a shorter local methods style than jonathan is willing to
   accept.
-  scott thinks that matching definition names (e.g., *end class <bolt>*)
   are a bad idea because they are hard to maintain in the face of
   name changes.
-  keith likes long case slot specifications always, but jonathan is
   willing to abbreviate in groups.
-  keith thinks its ok to have parameters on same line as function name
   with return values on the next line (but jonathan disagrees). Dylan
   book does not split a line between the parameter list and return
   values.
-  haahr claims that there is no space after function name in a function
   definition. we need to chase this up. Dylan book has a space after
   function name in a function definition.
-  scott uses ``%`` prefixes on slot accessors that have short (or
   "unqualified" names) that are not part of the exported API. Dylan
   book does not do this.
-  6 character rule seems like overkill (but jonathan thinks that it
   does help in practice but is willing to get rid of it to simplify the
   pretty printing rules).
-  ``_`` is a weird notation, but does have precendent in syntax-case and
   prolog for dont care. keith likes the return values to be named for
   better documentation. Dylan book names return values.
-  tucker disagrees with consistency rule. he thinks that there should
   be a consistent style on where you break and how you indent, but he
   disagrees that that means you have to break there on all the similar
   nearby statements, even when they don't need it.

80 columns law
==============

Lines should not exceed 80 columns in length.

Consistency
===========

Formatting style should be consistent.

A broader pattern I tend to go for is consistency of indentation within
a group of forms. If the shape of one or two forms in a group force a
long-form indentation I'll often reformat the rest to give them a
consistent look. For example, here is a sequence of ``let`` bindings.

.. code-block:: dylan

    let abigmobynamewithabiginitialization
      = 123456789 * 123456789 * 123456789 * 123456789;

    let a-tiny-name
      = 123456789;

Here is another examples of a group, a bunch of generic function
definitions forming a protocol:

.. code-block:: dylan

    //// Match protocol.
    define generic match
      (pattern, fragment, more-patterns, more-fragments, env, fail) => ();

    define generic match-empty
      (pattern, more-patterns, env, fail) => ();

    define generic folds-separator?
      (pattern) => ();

Naming
======

-  full names
-  lowercase
-  angle bracket notation for types
-  slot-names (keith)

There isn't a recommended Dylan style, although the majority of Dylan
code I've seen from outside Harlequin just uses the unqualified name of
the property as the slot name.

However, previous discussions here, apart from provoking complaints
about the way Dylan works in this area, seem to have suggested that
qualifying slot names is the right thing in most cases in order to avoid
widespread problems with name clashes later on (at least for exported
names). That's not to say it's caught on yet...

I do think the pure Lisp approach is very bad indeed though, and
shouldn't be used. I disagree that it makes the uses of accessors more
intelligible, particularly if you're calling accessors from different
superclasses on the same object in a block of code. And, as you say, it
rather breaks abstraction of implementation.

One possible compromise is to choose a prefix for a whole group of
classes beneath a given root. For example, the compiler's intermediate
representation consists of a number of different graph nodes and we
could choose a blanket prefix for slots of classes in that heterarchy
such as "dfm-" (our IR is known as the DFM).

*Dylan book style:* We will discuss this issue in book, but use shorter
names. They won't be exported, generally, so it won't be an issue.

Dot notation
============

Use for stateless property accessors that return a single value.

I now tend to use dot notation quite widely for any logical "property
access", even if computed. That is, ``foo.size`` is acceptable but the
imperative ``foo.initialize`` and ``foo.close`` aren't for me.

Symbols versus keywords
=======================

Use keywords only for keyword parameters. Do this:

.. code-block:: dylan

    make(<file-stream>, direction: #"input");

instead of this:

.. code-block:: dylan

    make(<file-stream>, direction: input:);

It's reasonable to use keyword syntax to specify a received keyword, for
example in a slot specification or in a parameter list:

.. code-block:: dylan

    slot point-x, init-keyword: x:;

End words
=========

Use end words for everything (e.g., ``if``, ``block``, ...)

Use name as well (e.g., method, class, ...) except you can abbreviate
both to allow for inlined expressions.

Semicolons
==========

Last expression can go without semicolon only where the value is used.
This is actually a useful little practice since if you want to add a
form to the end of a body whose value is significant you're forced to
think a little more.

.. code-block:: dylan

    define method empty? (vector :: <vector>)
      vector.size = 0
    end method empty?;

    define method add (vector :: <vector>, object)
      let new-vector :: <vector>
        = make(vector.class-for-copy, size: vector.size + 1);
      replace-subsequence!(new-vector, vector);
      new-vector[vector.size] := object;
      new-vector
    end method add;

*Dylan book* group is still discussing semicolons.

General indentation
===================

Avoid boxing your code and having big right column:

.. code-block:: dylan

    define method yukyukyukyukyukyukyuk (blahblahblahblahblah :: <foo>,
                                         tolosetrack :: <bar>,
                                         concerned? :: <boolean>)
    end method yukyukyukyukyukyukyuk;

Use two space indentation:

.. code-block:: dylan

    begin
      tell-da-world(bigfish, smallpond);
      world
    end

Operators on newline
====================

.. code-block:: dylan

    a | b

Put operator on new line and indent two spaces:

.. code-block:: dylan

    supercalifragilisticexpealidocious
      | wasthatashovelfull
      | ofraisensorsyrup

Parentheses
===========

Indent past parenthesis + operator rule:

.. code-block:: dylan

    (x + y + z
       + a + b + c)

Calls
-----

Usual is on same line with arguments single spaced and no space between
the function and its argument list:

.. code-block:: dylan

    funkie(a, b, c);

    longfunkiefunctionnamesuperfraligistic(a, b, c);

Function name up to 6 characters keep parens on same line:

.. code-block:: dylan

    values(0,
           sequence.size,
           sequence-next-state,
           sequence-finished-state?,
           sequence-current-key,
           stretchy-vector-current-element,
           stretchy-vector-current-element-setter,
           identity-copy-state)

Function name more than 6 characters break to newline:

.. code-block:: dylan

    redirect-computations!
      (old-c, new-c, previous-computations, next-computations);

More arguments:

.. code-block:: dylan

    redirect-computations!
      (old-c, new-c, previous-computations, next-computations,
       areallylongidthatrequireswrappingtheargs);

if then else
============

General case:

.. code-block:: dylan

    if (expr)
      then statements ...
    else
      else statements ...
    end if;

Abbreviated use:

.. code-block:: dylan

    if (expr) x else y end;

let
===

Same indentation level:

.. code-block:: dylan

    let x = xxxxx;
    let y = yyyyy;
    inc!(x, z);
    let z = f!(x, y);
    z + z;

Big names and initialization:

.. code-block:: dylan

    let superfragilisticespealidoscious
      = someexpressionthatclearlydoesnotfitabove;

select and case
===============

The aligned ``=>``'s help make the cases stand out:

.. code-block:: dylan

    case
      count > 0 & test(item, target)
        => grovel(count - 1, src-index + 1, dst-index);
      otherwise
        => vector[dst-index] := item;
           grovel(count, src-index + 1, dst-index + 1)
    end case;

Abbreviated use:

.. code-block:: dylan

    case
      *blue?*   => 2;
      *yellow?* => 3;
    end case;

Long expression:

.. code-block:: dylan

    select
        (supercalifragilisticexbealidocious
           + someexpressionthatclearlydoesnotfitabove)
      1 => 2;
      2 => 3;
    end select;

Macros
======

.. code-block:: dylan

    define macro collecting
      { collecting () ?body end }
        => { collecting (_collector)
               ?body;
               collected(_collector)
             end }
      { collecting (as ?expression) ?body end }
        => { collecting (_collector as ?expression)
               ?body;
               collected(_collector)
             end }
      { collecting (?vars) ?body end }
        => { ?vars;
             ?body }
    vars:
      { ?var, ... }
        => { ?var; ... }
      { }
        => { }
    end macro;

Assignments broken over two lines
=================================

.. code-block:: dylan

    superfragilisticespealidoscious
      := somereallylongexpressionthatdoesnotfitabove;

Definitions broken over two lines
=================================

.. code-block:: dylan

    define variable lilgirlscryalldatime
      = bigboysdontcry;

for loop
========

.. code-block:: dylan

    for (elementincollectionnumberone in collection1,
         elementincollectionnumbertwo in collection2)
      ...
    end for

Abbreviated use:

.. code-block:: dylan

    for (f in foo, b in bar)
      ...
    end for

Local methods
=============

.. code-block:: dylan

    method (y)
      local method strip (x)
              ...
            end method strip,
            method chars (x)
              ...
            end method chars;
      strip(chars(y))
    end method;

Tight for space:

.. code-block:: dylan

    method (y)
      local
        method strip (x)
          ...
        end method strip,
        method chars (x)
          ...
        end method chars;
      strip(chars(y))
    end method;

Abbreviated use:

.. code-block:: dylan

    method (y)
      local strip (x) ... end,
            chars (x) ... end;
      strip(chars(y))
    end method;

A single recursive method:

.. code-block:: dylan

    method (y)
      local stripchars (x)
          ...
      end;
      stripchars(y)
    end method;

Parameter lists (style A)
=========================

The *Dylan book style* differs for parameters lists and return values.
See `Parameters and return values, Dylan book style`_.

Right after function name:

.. code-block:: dylan

    define method vector (#rest rest)
      rest
    end method vector;

Indentation, style A:

.. code-block:: dylan

    define method union
        (seq-1 :: <sequence>, seq-2 :: <sequence>, #key test = \==)
      remove-duplicates(concatenate(seq-1, seq-2), test: test)
    end method union;

    define method f01234567890123456789
        (a0123456789, b0123456789, c0123456789, d0123456789,
         e0123456789, f0123456789, g0123456789)
      body-starts-here
    end method f01234567890123456789;

Optional parameters: Use the same aesthetic applied to indenting binary
operators continued across lines, indent # names as follows:

.. code-block:: dylan

    define method print
        (object :: <multiple-value-combination>,
          #key stream = *standard-output*, verbose?)
      ...
    end method print;

Return values (style A)
=======================

The *Dylan book style* differs for parameters lists and return values.
See `Parameters and return values, Dylan book style`_.

No semicolon

Parenthesis notation.

If both parameter list and return values fit on the first line:

.. code-block:: dylan

    define method past? (time :: <offset>) => (result :: <boolean>)
      time.total-seconds < 0;
    end method past?;

If parameter list and return values do not both fit on the first line:

.. code-block:: dylan

    define method element-setter
        (new-value, list :: <list>, key :: <small-integer>) => (new-value)
    end method element-setter;

If parameter list and return values do not both fit on the same line:

.. code-block:: dylan

    define method decode-total-seconds
        (time :: <time-of-day>)
          => (hour :: <integer>, min :: <integer>, sec :: <integer>)
      decode-total-seconds(time.total-seconds);
    end method decode-total-seconds;

    define method convert-expressions
        (env :: <environment>, argument-forms)
          => (first :: <computation>, last :: <computation>, temporaries)
    end method convert-expressions;

Optional parameters split across a line:

.. code-block:: dylan

    define method fill!
        (sequence :: <mutable-sequence>, value :: <object>,
           #key start: first = 0, end: last)
             => (sequence :: <mutable-sequence>)
    end method fill!;

Complicated cases

The following is preferred:

.. code-block:: dylan

    define method \<
        (a :: <double-float>, b :: <ratio>) => (res :: <boolean>)
      a < as(<double-float>, b)
    end method \<;

Over this:

.. code-block:: dylan

    define method \< (a :: <double-float>, b :: <ratio>)
        => (res :: <boolean>)
      a < as(<double-float>, b)
    end method \<;

Use other return value name to convey more meaning if possible.

.. code-block:: dylan

    define method reverse! (list :: <list>) => (list :: <list>)
      ...
    end method reverse!;

    define generic munge (list :: <list>) => (new-list :: <list>);

    define generic munge! (list :: <list>) => (list :: <list>);

Use ``_`` for poetry impaired or where the function name corresponds
exactly to the return value name

.. code-block:: dylan

    define method first (s :: <sequence>, #rest keys, #key default) => (_)
      ...
    end method first;

Parameters and return values, Dylan book style
==============================================

The Dylan book style diverges on indentation of parameter lists and
return values.

Parameter lists, Dylan book style
---------------------------------

Right after function name:

.. code-block:: dylan

    define method vector (#rest rest)
      rest
    end method vector;

Indentation, style A:

.. code-block:: dylan

    define method union
        (seq-1 :: <sequence>, seq-2 :: <sequence>, #key test = \==)
      remove-duplicates(concatenate(seq-1, seq-2), test: test)
    end method union;

    define method f01234567890123456789
        (a0123456789, b0123456789, c0123456789, d0123456789,
         e0123456789, f0123456789, g0123456789)
      body-starts-here
    end method f01234567890123456789;

Optional parameters: Treat as a stacked list.

.. code-block:: dylan

    define method print
        (object :: <multiple-value-combination>,
         #key stream = *standard-output*, verbose?)
      ...
    end method print;

Return values, Dylan book style
-------------------------------

No semicolon

Parenthesis notation.

If both parameter list and return values fit on the first line:

.. code-block:: dylan

    define method past? (time :: <offset>) => (result :: <boolean>)
      time.total-seconds < 0;
    end method past?;

If parameter list and return values do not both fit on the first line:

.. code-block:: dylan

    define method element-setter
      (new-value, list :: <list>, key :: <small-integer>) => (new-value)
    end method element-setter;

If parameter list and return values do not both fit on the same line:

.. code-block:: dylan

    define method decode-total-seconds
        (time :: <time-of-day>)
     => (hour :: <integer>, min :: <integer>, sec :: <integer>)
      decode-total-seconds(time.total-seconds);
    end method decode-total-seconds;

    define method convert-expressions
        (env :: <environment>, argument-forms)
      => (first :: <computation>, last :: <computation>, temporaries)
    end method convert-expressions;

Optional parameters split across a line:

.. code-block:: dylan

    define method fill!
        (sequence :: <mutable-sequence>, value :: <object>,
         #key start: first = 0, end: last)
      => (sequence :: <mutable-sequence>)
    end method fill!;

Complicated cases

The following is preferred:

.. code-block:: dylan

    define method \<
        (a :: <double-float>, b :: <ratio>) => (res :: <boolean>)
      a < as(<double-float>, b)
    end method \<;

Over this, which we do not do in the book:

.. code-block:: dylan

    define method \< (a :: <double-float>, b :: <ratio>)
        => (res :: <boolean>)
      a < as(<double-float>, b)
    end method \<;

Use other return value name to convey more meaning if possible.

.. code-block:: dylan

    define method reverse! (list :: <list>) => (list :: <list>)
      ...
    end method reverse!;

    define generic munge (list :: <list>) => (new-list :: <list>);

    define generic munge! (list :: <list>) => (list :: <list>);

In the Dylan book, we do not use ``_`` for poetry impaired or where
the function name corresponds exactly to the return value name

Non generic function methods
============================

.. code-block:: dylan

    define constant curry
      = method (...) => (...)
      ...
    end method curry;

Abbreviated use:

.. code-block:: dylan

    define constant make-region-union = method (#rest regions)
      make(<region-union>, regions: concatenate-as(<vector>, regions))
    end method make-region-union;

Generic function definitions
============================

.. code-block:: dylan

    define open generic choose
      (pred :: <function>, seq :: <sequence>) => (elts :: <sequence>);

    define open generic choose-by
      (pred :: <function>, test-seq :: <sequence>, val-seq :: sequence>)
     => ( _ :: <sequence>);

Class definitions
=================

Lots of direct superclasses:

.. code-block:: dylan

    define class <z>
      (<a>, <b>, <c>)
      ...
    end class <z>;

Long slot initializations:

.. code-block:: dylan

    define class <entry-state> (<temporary>)
      slot name, init-keyword: name:;
      slot me-block, init-keyword: block:;
      slot exits :: <stretchy-vector>,
        init-function: curry(make, <stretchy-vector>),
        init-keyword: exits:;
    end class;

Or even better:

.. code-block:: dylan

    define class <entry-state> (<temporary>)
      slot name, init-keyword: name:;
      slot me-block, init-keyword: block:;
      slot exits :: <stretchy-vector> = make(<stretchy-vector>),
        init-keyword: exits:;
    end class;

