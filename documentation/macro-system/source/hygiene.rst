.. default-role:: samp
.. highlight:: dylan
.. _hygiene:


*******
Hygiene
*******

Let us say we have two macros A and B. The expansion of A calls B. The following
diagram shows the sources or lexical scopes of a binding used in the expansion
of A and B.

.. code-block:: none

   +--------------------------------+    +-------------------------------------+
   | [1] Module or local scope of   |    | [4] Module containing definition of |
   | the call to macro A            |    | macro A                             |
   |                                |    |                                     |
   |  +--------------------------+  |    +-------------------------------------+
   |  | [2] Expansion of A       |  | 
   |  |                          |  |    +-------------------------------------+ 
   |  |  +--------------------+  |  |    | [5] Module containing definition of | 
   |  |  | [3] Expansion of B |  |  |    | macro B                             | 
   |  |  |                    |  |  |    |                                     | 
   |  |  +--------------------+  |  |    +-------------------------------------+ 
   |  |                          |  | 
   |  +--------------------------+  | 
   |                                | 
   +--------------------------------+

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
`Dylan Programming`:title:.

Say macro A is defined in box 4 as [rep]_. Code in box 1 can call the macro as
follows:

.. code-block:: dylan

   let i = 0;
   repeat
     if (i == 100) stop!() end;
     i := i + 1;
   end

The `?=stop!` substitution in line 3 of the macro expands (in box 2) to a
binding visible in boxes 1 and 2. In box 1, the binding is visible as `stop!`.
In box 2 (the expansion itself), the binding is visible as `?=stop!` and if it
were a function, the macro template could contain code fragments like this:

.. code-block:: dylan

   ?=stop!(arg-1, arg-2)

Note that a macro expansion cannot create a new binding visible outside of the
macro call in box 1. For example, given a macro like [dofoo]_, one might expect
the macro call [dofoo-call]_ would print "Hello" twice, but it will not work.
Because every macro expansion is implicitly surrounded by begin…end as described
in `background-overview`:ref:, the example expands into [dofoo-exp]_. After the
macro call, `foo` is no longer in scope.

----------

.. [rep] *Repeat macro*

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

.. [dofoo] *Do-then-foo macro*

   .. code-block:: dylan
      :linenos:

      define macro do-then-foo
        { do-then-foo(?:expression) ?:body end }
          => { let ?=foo = ?expression; ?body }
      end macro

.. [dofoo-call] *Calling do-then-foo*

   .. code-block:: dylan
 
      do-then-foo("Hello\n") format-out(foo) end;
      format-out(foo)

.. [dofoo-exp] *Expansion of do-then-foo*

   .. code-block:: dylan
   
      begin
        let foo = "Hello\n";
        format-out(foo)
      end;
      format-out(foo)
