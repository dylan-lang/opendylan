:copyright: Copyright © 2011 Dustin Voss, All Rights Reserved.

.. default-role:: dfn
.. highlight:: dylan
.. sidebar:: Navigation

   :Next:   :doc:`macro-types`
   :Prev:   :doc:`index`
   :Top:    :doc:`index`
   
   .. contents::
      :local:


***********************
Background and Overview
***********************

Macros work on the basis of code fragments. The macro system
does not understand code fragments; it just substitutes some fragments for other
fragments. Once the macro system has substituted and arrange all the code
fragments, they are compiled into executable code.

Because the macro system parses and generates code fragments, it can recognize
the difference between a string containing a macro name and an actual invocation
of a macro. Macros are not affected by such syntactical issues; strings and
other expressions are treated as opaque units.

The following are all examples of elementary code fragments, or `parsed
fragments`. These combine to form the larger code fragments upon which macros
operate.

.. code-block:: none
   
   'a'
   "end times"
   35.552
   3/4
   (3 + 7)
   #t
   #"red"
   red:
   cinnamon
   ==
   #[1, 2, 3]
   as(...)
   list.size


Anatomy and terms
=================

Macros have `main rules` and `auxiliary rules`. Each of the main or auxiliary
rules has a `pattern` and a `template`. Patterns are matched against the code
fragments of your source code. A main rule is matched against the code fragment
that comprises the entire macro call. An auxiliary rule is matched against parts
of that code fragment. The matched code is then replaced by the template.

A pattern can contain code fragments and `pattern variables`. If a code
fragment in the source code matches what is in the pattern, parts of that code
fragment may be pulled out into pattern variables. The rest is discarded.

A template can contain other code fragments and `substitutions`. Substitutions
are placeholders; the contents of a pattern variable are processed and inserted
into the template in place of every corresponding substitution. The template's
combined fragments and substitutions form the macro's `expansion`, which
replaces the original code fragment.

This happens recursively: after a macro is expanded, its expansion is scanned
for additional macro call code fragments, and those are expanded in turn. The
parser recognizes a macro call code fragment by way of a `distinguishing word`
and the type of syntax associated with the macro (discussed further in
:doc:`macro-types`).

Let us examine `Function Macro`_. This macro might be called by the code
`Original Code`_, but the actual call fragment `Call Fragment`_ is what the
parser will attempt to match. The macro's expansion will be `Expansion`_ and the
original code will then become `Replacement Code`_.

Here are the parts of the macro:

- The distinguishing word is ``table``. Whenever the compiler sees ``table(…)``,
  it will expand this macro rather than creating a call to a function named
  "table".
- The main rules are in lines 2–4.
- The macro has one set of auxiliary rules in lines 6–9. A set of auxiliary
  rules has a title written as a symbol. This set of auxiliary rules is titled
  ``table-contents:`` (or, alternatively, ``#"table-contents"``).
- The pattern of the first main rule is in line 2.
- The template of the first main rule is in line 3.
- The patterns in this macro include the pattern variables ``?table-class``,
  ``?table-contents``, ``?rest``, ``?key``, and ``?value``.
- The substitutions in this macro include those same names.

Note that the expansion is surrounded by ``begin`` and ``end``. Macro expansions
are always surrounded by a begin…end block. This helps with macro hygiene (i.e.
preventing bindings outside of a macro call from being affected by bindings used
in a macro's expansion). I discuss hygiene :doc:`here <hygiene>`.

----------

_`Function Macro`:

   .. code-block:: dylan
      :linenos:
   
      define macro table
        { table(?table-class:expression, ?table-contents) }
          => { let ht = make(?table-class); ?table-contents; ht; }
        { table(?rest:*) } => { table(<table>, ?rest); }
      
        table-contents:
        { } => { }
        { ?key:expression => ?value:expression, ... }
          => { ht[?key] := ?value; ... }
      end macro table

_`Original Code`:
   
   .. code-block:: dylan
   
      let lights = table(<string-table>, "red" => "stop", "green" => "go");

_`Call Fragment`:

   .. code-block:: dylan
   
      table(<string-table>, "red" => "stop", "green" => "go")

_`Expansion`:

   .. code-block:: dylan
   
      let ht = make(<string-table>); ht["red"] := "stop"; ht["green"] := "go"; ht;

_`Replacement Code`:

   .. code-block:: dylan
   
      let lights = begin
        let ht = make(<string-table>);
        ht["red"] := "stop"; ht["green"] := "go";
        ht;
      end;
