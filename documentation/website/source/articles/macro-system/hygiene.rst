:copyright: Copyright © 2011 Dustin Voss, All Rights Reserved.

.. default-role:: samp
.. highlight:: dylan
.. sidebar:: Navigation

   :Next:   :doc:`faq-tips`
   :Prev:   :doc:`auxiliary-rules`
   :Top:    :doc:`index`
   
   .. contents::
      :local:


*******
Hygiene
*******

Macro expansions are hygienic, meaning there can be no name conflict between
a local binding in scope that calls the macro and a local binding in the macro
expansion.

Let us say we have two macros A and B. The expansion of A calls B. The following
diagram shows the lexical scopes of bindings used in A and B. The table after
describes the scopes in more detail.

.. raw:: html

   <pre style="line-height: 1em; font-family: Andale Mono, Courier New">
   ╒═════════════════════════════════════╕
   │ [1] Module or lexical scope of a    │    
   │     call to macro A                 │    
   │                                     │    
   │  ┌───────────────────────────────┐  │
   │  │ [2] Expansion of A            │  │    
   │  │                               │  │     
   │  │  ┌─────────────────────────┐  │  │     
   │  │  │ [3] Expansion of B      │  │  │     
   │  │  │                         │  │  │     
   │  │  └─────────────────────────┘  │  │     
   │  │                               │  │ 
   │  └───────────────────────────────┘  │ 
   │                                     │ 
   ╘═════════════════════════════════════╛

   ╒═════════════════════════════════════╕
   │ [4] Module containing definition of │
   │     macro A                         │
   │                                     │
   ╘═════════════════════════════════════╛
   
   ╒═════════════════════════════════════╕
   │ [5] Module containing definition of │
   │     macro B                         │
   │                                     │
   ╘═════════════════════════════════════╛
   </pre>
   

+------------+---------------------+---------------------+---------------------+-------+--------+
| Definition | Definition Location                                                              |
| Visibility +---------------------+---------------------+---------------------+-------+--------+
|            | Box 1               | Box 2               | Box 3               | Box 4 | Box 5  |
+============+=====================+=====================+=====================+=======+========+
| Box 1      | All                 | Only if defined     | Only if defined     |       |        |
|            |                     | with unhygienic     | with unhygienic     |       |        |
|            |                     | reference to name   | reference to name   |       |        |
|            |                     | from 1, captured by | from 1, captured by |       |        |
|            |                     | pattern variable of | pattern variable of |       |        |
|            |                     | A                   | A and recaptured by |       |        |
|            |                     |                     | pattern variable of |       |        |
|            |                     |                     | B                   |       |        |
+------------+---------------------+---------------------+---------------------+-------+--------+
| Box 2      | Only if captured by | All                 | Only if defined     | All   |        |
|            | pattern variable of |                     | with unhygienic     |       |        |
|            | A                   |                     | reference to name   |       |        |
|            |                     |                     | from 2, captured by |       |        |
|            |                     |                     | pattern variable of |       |        |
|            |                     |                     | B                   |       |        |
+------------+---------------------+---------------------+---------------------+-------+--------+
| Box 3      | Only if captured by | Only if captured by | All                 |       | All    |
|            | pattern variable of | pattern variable of |                     |       |        |
|            | A and recaptured by | B                   |                     |       |        |
|            | pattern variable of |                     |                     |       |        |
|            | B                   |                     |                     |       |        |
+------------+---------------------+---------------------+---------------------+-------+--------+
| Box 4      |                     |                     |                     | All   |        |
+------------+---------------------+---------------------+---------------------+-------+--------+
| Box 5      |                     |                     |                     |       | All    |
+------------+---------------------+---------------------+---------------------+-------+--------+


Breaking hygiene
================

A template can prefix a binding with `?=`. This makes the binding come from
and be visible in the macro's caller. This can be illustrated by an example from
:title:`Dylan Programming`.

Say macro A defined in box 4 is `Definition 1`_, and the macro call in box 1 is
the following:

.. code-block:: dylan

   let i = 0;
   repeat
     if (i == 100) stop!() end;
     i := i + 1;
   end

The `?=stop!` substitution in line 3 of the macro becomes a reference to a
binding visible in boxes 1 and 2. In box 1, the binding is visible as `stop!`.
In box 2 (the expansion itself), the binding is visible as `?=stop!` and can be
used like any other binding (e.g. `format-out`) as shown by the highlighted
line.

Note that that a macro expansion cannot create a new name visible outside of the
macro call itself. In other words, box 2 cannot create a binding for use
elsewhere in box 1 unless box 1 supplies the name to be defined.

For example, given the macro in `Definition 2`_, one might expect the macro call
in `Call 2`_ would print "Hello" twice, but the code does not compile. Because
every macro expansion is implicitly surrounded by begin…end as described in
:doc:`background-overview`, the example expands into `Expansion 2`_. After the
macro call, `foo` is no longer in scope.

----------

_`Definition 1`:

   .. code-block:: dylan
      :linenos:
      :emphasize-lines: 3
      
      define macro repeat
        { repeat ?:body end }
          => { block (?=stop!)
                 local method again() ?body; again(); end;
                 again();
               end }
      end macro

_`Definition 2`:

   .. code-block:: dylan
      :linenos:

      define macro do-then-foo
        { do-then-foo(?:expression) ?:body end }
          => { let ?=foo = ?expression; ?body }
      end macro

_`Call 2`:

   .. code-block:: dylan
 
      do-then-foo("Hello\n") format-out(foo) end;
      format-out(foo)

_`Expansion 2`:

   .. code-block:: dylan
   
      begin
        let foo = "Hello\n";
        format-out(foo)
      end;
      format-out(foo)
