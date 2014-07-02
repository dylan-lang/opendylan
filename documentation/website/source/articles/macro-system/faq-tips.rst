.. default-role:: samp
.. highlight:: none
.. sidebar:: Navigation

   :Next:   
   :Prev:   :doc:`hygiene`
   :Top:    :doc:`index`
   
   .. contents::
      :local:


************
FAQ and Tips
************ 

General advice and troubleshooting
==================================

- The best way to design a macro is:
   1. Come up with the best non-macro interface you can.
   2. Design the syntax of the macro call up front.
   3. Implement that design.
- Gwydion Dylan sometimes has issues with trailing semi-colons or commas. In
  general, don't include a separator at the end of a template.
- In both Gwydion Dylan and Open Dylan, the variable declaration for a variable
  used in the lexical scope of a macro needs to be in that macro. The variable
  cannot be declared in an auxiliary macro and exposed to the macro through the
  `?=` mechanism. However, the variable can be declared in an auxiliary rule of
  the macro.
- Ensure you haven't accidently given a pattern variable the same name as an
  auxiliary rule set.


How can I combine multiple names into one?
==========================================

There is no real way to do this for names or symbols. The concatenating
substitution forms do not scale, so this template will not work::

   { define ?name-1 ## "-" ## ?name-2 ## "-function" () }
   
However, you can easily combine multiple names into a string by taking advantage
of adjacent string concatenation::

   { format-out(?"name-1" "-" ?"name-2" "-function") }

Your best bet may be to do some sort of string-based introspection, or create
anonymous definitions stored in a table keyed by a string.


How can I write macros that follow a BNF-like syntax?
=====================================================

Macros are designed to follow Dylan language conventions, so you may not be able
to support arbitrary BNF-based syntax. But here are some tricks to help with
common BNF forms.

`{x}?`
      An optional item can be handled by a wildcard pattern variable using an
      auxiliary rule with two patterns::
   
         x-opt:
           { x }
           { }

`{x}? {y}? {z}?`
      If there are several space-separated optional items, put them all in the
      same auxiliary rule, since the upper rule can't have adjacent wildcard
      pattern variables that call out to an individual auxiliary rule for each
      item::

         x-y-z-opts:
           { x ... }
           { y ... }
           { z ... }
           { }

`{x}? | {x} (, {x})*`
      This is a list that may have 0–*n* items. Handle this by calling out to an
      auxiliary rule that calls itself recursively like so::
      
         x-list:
           { ?x:*, ?x-list:* }
           { }

      Note that the calling rule needs to use a wildcard pattern variable to
      collect the comma'd items for the `x-list:` rule set; this pattern
      variable needs to be well-separated from the syntax that follows it by a
      semicolon or intermediate word.

`{x} (, {x})*`
      This is a list that may have 1–*n* items. You simply cannot do this in the
      general case; your best bet is design your macro to handle 0 items
      gracefully and then use a 0-*n* list.

      The following does not work because `?{x}:*` allows an empty code
      fragment, which allows 0 items::
      
         x-list:
           // Doesn't work
           { ?x:*, ?x-list:* }
           { ?x:* }

      Of course, if you can put a constraint on `?x`, it will work fine, but
      remember that a secondary rule can't be used to provide a constraint in a
      primary rule.


I can't make a bare list!
=========================

A macro that makes a bare list (by which I mean a simple list of comma-separated
names) cannot do anything useful with it. Macros build cohesive code fragments,
and a bare list is not such a code fragment.

For example, this will not compile::

   define macro setter-names
     { setter-names(?names) } => { ?names }
   names:
     { ?:name, ... } => { ?name ## "-setter", ... }
   end macro;

   vector(setter-names(alpha, beta, gamma, delta))

It does not compile because the expansion of `setter-names` is wrapped in a
begin…end, resulting in this invalid syntax::

   vector(begin alpha-setter, beta-setter, gamma-setter, delta-setter end)

Instead, do something with the list in the macro itself:

.. code-block:: dylan
   :emphasize-lines: 2, 7
   
   define macro setter-vector
     { setter-vector(?names) } => { vector(?names) }
   names:
     { ?:name, ... } => { ?name ## "-setter", ... }
   end macro;
   
   setter-vector(alpha, beta, gamma, delta)
