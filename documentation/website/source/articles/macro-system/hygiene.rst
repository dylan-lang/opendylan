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

Let us say we have two macros A and B. The expansion of A calls B. The following
diagram shows the sources or lexical scopes of a binding used in the expansion
of A and B.

.. raw:: html

   <pre style="line-height: 1em; font-family: Andale Mono, Courier New">
   ╒═════════════════════════════════════╕
   │ [1] Module or local scope of a call │    
   │     to macro A                      │    
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
   
Macro expansions are hygienic, meaning:

- Bindings from boxes 2 or 4 are visible in box 2.
- Bindings from boxes 3 or 5 are visible in box 3.
- A binding from box 1 is not visible in box 2, but can be wrapped in a pattern
  variable and manipulated in box 2.
- A binding from box 2 is not visible in box 3, but can be wrapped in a pattern
  variable and manipulated in box 3.
- A binding from box 1 is not visible in box 3, but if box 2 wraps the binding
  in a pattern variable and uses that pattern variable in the call of macro B,
  and macro B wraps that pattern variable in a pattern variable of its own, then
  box 3 can manipulate the binding via its pattern variable.

New bindings created by `define` statements are not hygienic. Definition
processing occurs after all macros have been expanded. A class defined in box 3
can be used in boxes 2 and 1 and exported from the module of box 1. If there is
another definition by that name in the other boxes, it is a duplicate definition
and an error is reported.


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

Note that that a macro expansion cannot create a new binding visible outside of
the macro call itself (except by way of a top-level `define` statement). In
other words, box 2 cannot create a local binding for use elsewhere in box 1.

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
