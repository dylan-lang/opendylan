:copyright: Copyright © 2011 Dustin Voss, All Rights Reserved.

.. default-role:: samp
.. highlight:: dylan
.. sidebar:: Navigation

   :Next:   :doc:`hygiene`
   :Prev:   :doc:`substitutions`
   :Top:    :doc:`index`
   
   .. contents::
      :local:


******************************
Auxiliary Rules and Expansions
******************************

Auxiliary Rules
===============

Auxiliary rules transform the code fragment contained in a pattern variable
before it is substituted into a template.

Auxiliary rule sets follow the syntax described in :doc:`patterns` and have the
behaviors described on that page. They do not have the special elements like
`define` or `{modifiers}` shown in :ref:`main-rules`, but the macro type does
place certain *de facto* restrictions on what can appear in auxiliary rule
patterns:

- `end` cannot usefully appear in an auxiliary rule pattern of a body-style
  definition macro or a statement macro unless it is enclosed in bracketing
  characters.
- `;` cannot usefully appear in an auxiliary rule pattern of a list-style
  definition macro unless enclosed in bracketing characters.

An auxiliary rule set comes into play when a pattern variable matches a code
fragment and that pattern variable is named the same as the auxiliary rule set.
Usually, the pattern variable is a wildcard variable written without a
constraint, but the pattern variable can use any of the forms described in
:doc:`patterns`, including the `#key` and `??{name}:{constraint}` forms.

After the pattern variable matches and is set to a code fragment, that code
fragment is matched against the rules of the auxiliary rule set. If a rule's
pattern matches the code fragment, that rule's template is expanded and replaces
the code fragment contained by the pattern variable. If no rules match the code
fragment, macro expansion fails.

If the pattern variable being examined is a `??`-style pattern variable, the
process is similar, except each code fragment in the pattern variable is
individually matched and transformed by the auxiliary rules.


Expansions
==========

This section discusses expansions through a series of examples. The examples are
all variations of a function macro named `version` that builds a version number
in a specific format and sets it by calling a function `set-version`. The
`set-version` function is declared like this::

      define function set-version (version-string :: <string>) => ()


Simple expansion
----------------

First, let us consider the macro definition `Definition 1`_. The macro is called
by `Call 1`_ and expands to `Expansion 1`_.

The `?type` pattern variable in line 1 of the macro definition matches `alpha`
in the call. After the variable matches, the `type:` auxiliary rule set in lines
4–7 rewrites the contents of the pattern variable according to the matching rule
in line 5. The matching rule expands to the string `"a"`, which replaces the
`alpha` code fragment in the pattern variable. In the main rule's template (line
3), the pattern variable (now containing `"a"`) is substituted into the
expansion.

----------

_`Definition 1`:

   .. code-block:: dylan
      :linenos:

      define macro version
        { version(?number:expression, ?type:name) }
          => { set-version(?number ?type) }
      type:
        { alpha } => { "a" }
        { beta } => { "b" }
        { release } => { }
      end macro

_`Call 1`:

   .. code-block:: dylan

      version("1.2", alpha)

_`Expansion 1`:

   .. code-block:: dylan

      set-version("1.2" "a")

   .. tip:: Dylan compiles `"1.2" "a"` like `"1.2a"`.

----------

Effect of constraints
^^^^^^^^^^^^^^^^^^^^^

Now consider if the auxiliary rules were rewritten as `Definition 2`_. This
macro is intended to be called by `Call 2`_ to create a version number like
`"1.0a1"`. However, the macro will never succeed. `?type` in line 2 has the
`name` constraint, so it cannot match the call, which includes a comma and an
additional clause. The `type:` auxiliary rule set will not even be consulted and
macro expansion will fail.

----------

_`Definition 2`:

   .. code-block:: dylan
      :linenos:
      :emphasize-lines: 5-7

      define macro version
        { version(?number:expression, ?type:name) }
          => { set-version(?number ?type) }
      type:
        { alpha, ?n:expression } => { "a" ?n }
        { beta, ?n:expression } => { "b" ?n }
        { release, ?n:expression } => { }
      end macro

_`Call 2`:

   .. code-block:: dylan
   
      version("1.0", alpha, "1")

----------

Empty and missing code fragments
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

An auxiliary rule set can match against a missing code fragment. Consider the
following macro call in relation to `Definition 1`_:

.. code-block:: dylan

   version("1.0")

With this macro call, the `?number` pattern variable would contain `"1.0"` and
`?type` would be empty, as described in :ref:`final-items`. The macro would fail
to match this code fragment, since the `name` constraint of the `?type`
variable does not match a missing code fragment.

If we changed the macro definition to include a wildcard constraint, as in
`Definition 3`_, the macro would still fail to match the code fragment because,
while the `?type` pattern variable itself will match, the `type:` auxiliary rule
set does not have a pattern that matches a missing code fragment. We would also
have to add the rule highlighted in `Definition 4`_.

----------

_`Definition 3`:

   .. code-block:: dylan
      :linenos:
      :emphasize-lines: 2

      define macro version
        { version(?number:expression, ?type:*) }
          => { set-version(?number ?type) }
      type:
        { alpha } => { "a" }
        { beta } => { "b" }
        { release } => { }
      end macro

_`Definition 4`:

   .. code-block:: dylan
      :linenos:
      :emphasize-lines: 8

      define macro version
        { version(?number:expression, ?type:*) }
          => { set-version(?number ?type) }
      type:
        { alpha } => { "a" }
        { beta } => { "b" }
        { release } => { }
        { } => { }
      end macro

----------


Complex expansion
-----------------

Now suppose we wanted to support the syntax `Call 5`_. This macro should expand
to `Expansion 5`_ to generate a version number like `"1.042a"`. The macro could
be defined by the code `Definition 5`_.

----------

_`Definition 5`:

   .. code-block:: dylan
      :linenos:

      define macro version
        { version(#key ?major:expression, ??rev:expression, ?type:name = none) }
          => { set-version(concatenate(?major, ".", ??rev, ..., ?type)) }
      major:
        { ?rev } => { ?rev }
      rev:
        { ?:expression } => { format-to-string("%s", ?expression) }
      type:
        { alpha } => { "a" }
        { beta } => { "b" }
        { release } => { }
        { none } => { }
      end macro

_`Call 5`:

   .. code-block:: dylan

      version(major: 1, rev: 0, rev: 4, rev: 2, type: alpha)

_`Expansion 5`:

   .. code-block:: dylan

      set-version(concatenate(format-to-string("%s", 1),
                              ".",
                              format-to-string("%s", 0),
                              format-to-string("%s", 4),
                              format-to-string("%s", 2),
                              "a"))

----------

Property lists and optional properties
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The macro call must include the `major:` property, but the `rev:` and `type:`
properties are optional.

`rev:` is optional because it is a `??`-type pattern variable and, as described
in :ref:`proplist-variables`, that type of pattern variable can handle a missing
property. If the macro call did not include any `rev:` properties, the
substitution for `??rev, ...` would be empty. This would also cause the comma
after `"."` in line 3 to vanish, as described in :ref:`finalitems-subst`.

`type:` is optional because the pattern variable includes a default value. If
the macro call did not include `type:`, the substitution for `?type` in line 3
would be empty. It would initially be `none`, but then the pattern variable
would be processed by the `type:` auxiliary rule set and matched by the rule in
line 12, and its contents replaced by the empty template for that rule. Because
`?type` in line 3 would be empty, the comma after `??rev, ...` would vanish.

You may have noted that the `major:`, `rev:`, and `type:` auxiliary rule sets do
not include the actual `major:`, `rev:`, or `type:` symbols found in the macro
call. This is because `#key`-type pattern variables contain only the value parts
of properties, not the symbol parts.

Auxiliary rule sets in auxiliary rules
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In line 5, `?rev` is equivalent to `?rev:*`. The code fragment matched by that
pattern variable is the code fragment initially contained by the `?major`
pattern variable matched in line 2. This code fragment will be an expression.
Because `rev` is also the name of an auxiliary rule set, that code fragment will
be matched and transformed by the `rev:` rule set. That transformed code
fragment will be inserted in place of the `?rev` substitution in line 5 and then
subsequently inserted in place of the `?major` substitution in line 3.

`??` and `?` pattern variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The main rule and the `major:` auxiliary rule set both contain a pattern
variable named `rev`, though it is `??rev:expression` in the main rule (line 2)
and `?rev` in the auxiliary rule (line 5). Both pattern variables are
transformed by the `rev:` auxiliary rule in line 7 because both pattern
variables have the name `rev`, but they are transformed differently because of
the different natures of the two pattern variables.

Because the `?major` pattern variable in line 2 is a simple pattern variable
that contains only one code fragment, the `rev:` rule in line 7 that acts on it
(for reasons described above) transforms that fragment as you would expect:
`?major` will become a call to `format-to-string`.

However, the `??rev` pattern variable in line 2 is a `??`-type pattern variable
containing has zero or more code fragments, so when acting on *it*, the `rev:`
rule transforms each code fragment individually. The `??rev, ...` substitution
in line 3 then joins each of the transformed code fragments with a comma and
includes the entire collection in the main rule expansion, transforming the list
of revision numbers to a list of calls to `format-to-string`.

Empty `??` pattern variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In line 2, the `?type` variable has a default. If the macro call does not
contain a `type:` property, the default provides a code fragment to match
against the `type:` auxiliary rule set.

In contrast, the `??rev` variable does not have a default. If the call does not
include any `rev:` properties then the pattern variable will not contain a code
fragment. Since the `rev:` rule does not include an empty pattern, you might
expect the macro to fail.

But the macro still works. The `rev:` rule will be applied to each code fragment
in `??rev` individually because it is a `??`-type pattern variable. Since there
are no code fragments in `??rev`, the `rev:` rule set is not applied even once,
so its lack of an empty pattern is irrelevant.


Recursive expansion
-------------------

Any pattern variable named the same as an auxiliary rule is processed by that
rule. That includes pattern variables in the auxiliary rule referring to the
auxiliary rule set itself. This recursive behavior is useful for processing
lists of items.

The `...` pattern variable and substitution syntaxes draw attention to a
recursive rule and makes the author's intention explicit. Using that syntax, the
`path` macros in `Definition 6`_ and `Definition 7`_ are equivalent. But if I
may editorialize, I feel there is a good argument for avoiding that syntax for
the sake of consistency.

Let us trace the following macro call to show how macro recursion works::

      let (x, y) = path(north 5, east 3, south 1, east 2)

The patterns and templates will be evaluated as follows:

1. The main rule pattern matches. `?steps` is set to
   `north 5, east 3, south 1, east 2`.
#. The contents of `?steps` is rewritten by the `steps:` auxiliary rule set.

   a. The "north" rule is matched against `north 5, east 3, south 1, east 2`.
      The pattern is a comma-separated pattern, which matches the code fragment.
      The word `north` and the token `5` match. As described in
      :ref:`final-items`, the `?steps` pattern variable belonging to this
      pattern-match operation is set to `east 3, south 1, east 2`.
   #. The contents of this rule's `?steps` variable is rewritten by the `steps:`
      auxiliary rule set.
      
      i. The "north," "south," and "west" rules fail to match against
         `east 3, south 1, east 2`.
      #. The "east" rule matches and the `?steps` pattern variable of this
         pattern-match operation (different from any other `?steps` variable
         being dealt with) is set to `south 1, east 2`.
      #. `?steps` is rewritten by another pass through the `steps:` rule set.
      
         1. The "south" rule matches and its `?steps` is set to `east 2`.
         #. `?steps` is again rewritten.
         
            a. The "north," "south," and "west" rules fail to match.
            #. The "east" rule is matched against `east 2`. The word `east` and
               the token `2` match. The code fragment does not contain a comma,
               but the pattern matches the code fragment without the comma per
               :ref:`final-items`. The `?steps` pattern variable will contain an
               empty code fragment.
            #. Even though `?steps` contains an empty code fragment, it is still
               rewritten by the `steps:` auxiliary rule set.
               
               i. The "north," "south," "west," and "east" rules fail to match
                  against an empty code fragment.
               #. The empty pattern matches. Its expansion is an empty fragment.
               
            #. The `?steps` pattern variable of the "east" rule is set to the
               expansion of the auxiliary rule set, i.e., an empty fragment.
            #. The rule's expansion is therefore `x := x + 2`.
            
         #. The `?steps` pattern variable of the "south" rule is set to
            `x := x + 2`.
         #. The rule's expansion is therefore `y := y + 1; x := x + 2`.
         
      #. The `?steps` pattern variable of the "east" rule is set to
         `y := y + 1; x := x + 2`.
      #. The rule's expansion is therefore `x := x + 3; y := y + 1; x := x + 2`

…and so on. The key ideas to note are:

- The rule set has to have a non-recursing rule (in this case, ``{ } => { }``)
- Each rule's matching and expansion has its own `?token` and `?steps`
  pattern variable.

----------

_`Definition 6`:

   .. code-block:: dylan
      :linenos:
      :emphasize-lines: 4-7

      define macro path
        { path(?steps) } => { let x = 0; let y = 0; ?steps; values(x, y) }
      steps:
        { north ?:token, ?steps:* } => { y := y - ?token; ?steps }
        { south ?:token, ?steps:* } => { y := y + ?token; ?steps }
        { west ?:token, ?steps:* } => { x := x - ?token; ?steps }
        { east ?:token, ?steps:* } => { x := x + ?token; ?steps }
        { } => { }
      end macro

_`Definition 7`:

   .. code-block:: dylan
      :linenos:
      :emphasize-lines: 4-7

      define macro path
        { path(?steps) } => { let x = 0; let y = 0; ?steps; values(x, y) }
      steps:
        { north ?:token, ... } => { y := y - ?token; ... }
        { south ?:token, ... } => { y := y + ?token; ... }
        { west ?:token, ... } => { x := x - ?token; ... }
        { east ?:token, ... } => { x := x + ?token; ... }
        { } => { }
      end macro
