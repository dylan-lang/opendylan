.. default-role:: samp
.. highlight:: dylan
.. _auxiliary-rules:


******************************
Auxiliary Rules and Expansions
******************************

Auxiliary rules transform the code fragment contained in a pattern variable
before it is substituted into a template.

Auxiliary rule sets follow the syntax described in `patterns`:ref: and have the
behaviors described on that page. They do not have the special elements like
`define` or `{modifiers}` shown in `macro-types`:ref:, but the macro type does
place certain *de facto* restrictions on what can appear in auxiliary rule
patterns:

- `end` cannot usefully appear in patterns of a body-style definition macro or
  a statement macro unless it is enclosed in bracketing characters.
- `;` cannot usefully appear in patterns of a list-style definition macro
  unless enclosed in bracketing characters.

An auxiliary rule set comes into play when a pattern variable matches a code
fragment and that pattern variable is named the same as the auxiliary rule set.
Usually, the pattern variable is a wildcard variable written without a
constraint, but the pattern variable can use any of the forms described in
`patterns`:ref:, including the `#key` and `??{name}:{constraint}` forms.

After the pattern variable matches and is set to a code fragment, that code
fragment is matched against the rules of the auxiliary rule set. If a rule's
pattern matches the code fragment, that rule's template is expanded and replaces
the code fragment contained by the pattern variable. If no rules match the code
fragment, macro expansion fails.

If the pattern variable named the same as the rule set is a ``??``-style pattern
variable, the process is similar, except each code fragment in the pattern
variable is individually matched and transformed by the auxiliary rules.


Expansion examples
==================

Consider a macro [ver1]_ that makes it easier to generate version numbers. The
macro is called by [ver1-call]_ and expands to [ver1-exp]_.

The `?type` pattern variable in line 1 of the macro definition matches ``alpha``
in the call. After the variable matches, the `type:` auxiliary rule set in lines
4–7 rewrite the contents of the pattern variable according to the matching rule
in line 5. The matching rule expands to the string `"a"`, which replaces the
contents of the pattern variable. In the main rule's template (line 3), the
pattern variable (now containing `"a"` instead of `alpha`) is substituted into
the expansion.

----------

.. [ver1] *Version macro*

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

.. [ver1-call] *Macro call*

   .. code-block:: dylan
   
      version("1.2", alpha)

.. [ver1-exp] *Macro expansion*

   .. code-block:: dylan
   
      set-version("1.2" "a")

----------

Effect of constraints
---------------------

Now consider if the auxiliary rules were rewritten as [ver2]_. This macro might
be intended to create a version number like `"1.0a1"` if called by [ver2-call]_.
However, the macro will never succeed. `?type` in line 2 is constrained to
always be a simple name. None of the patterns in the `type:` auxiliary rule set
match a simple name (they all expect commas), so the macro expansion will fail.

----------

.. [ver2] *Version macro 2*

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

.. [ver2-call] *Macro call*

   .. code-block:: dylan
   
      version("1.0", alpha, "1")

----------

Missing code fragments
----------------------

An auxiliary rule set can match against a missing code fragment. Consider the
following code which calls [ver1]_:

.. code-block:: dylan

   version("1.0")

With this macro call, the `?number` pattern variable would
contain `"1.0"` and `?type` would be empty, as described in 
`final-items`:ref:. The macro would fail to match this code
fragment, since the `name`` constraint of the `?type` variable does not match
a missing code fragment.

If we changed the macro definition to include a wildcard constraint [ver3]_, the
macro would still fail to match the code fragment because the `type:`
auxiliary rule set does not have a pattern that matches a missing code fragment.
We would also have to add that rule [ver4]_.

----------

.. [ver3] *Macro with wildcard*

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

.. [ver4] *Macro with wildcard and empty pattern*

   .. code-block:: dylan
      :linenos:
      :emphasize-lines: 2,8

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

Final exam
----------

Now suppose we wanted to support the syntax [ver5-call]_. This macro should
expand to [ver5-exp]_ to generate a version number like "1.042a". The macro
could be defined by the code [ver5]_. There are several interesting aspects to
this version of the macro that I would like to point out.

Required and optional properties
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The macro call must include the `major:` property, but the `rev:` and `type:`
properties are optional. `rev:` is optional because it is a ``??``-type pattern
variable, and `type:` is optional because the pattern variable includes a
default value.

If the macro call did not include any `rev:` properties, the substitution for
`??rev, ...` would be empty. This would cause the comma after `"."` in line 3 to
vanish. If the macro call did not include `type:`, the substitution for
``?type`` in line 3 would be empty because the defaulted pattern variable
matches the pattern of the auxiliary rule in line 12, and the template for that
rule is empty. Because `?type` in line 3 would be empty, the comma after `??rev,
...` would vanish.

Property symbols
^^^^^^^^^^^^^^^^

The `major:`, `rev:`, and `type:` auxiliary rule sets do not include the actual
`major:`, `rev:`, or `type:` symbols found in the macro call. This is because
``#key``-type pattern variables contain only the value parts of properties, not
the symbol parts.

``??`` and ``?`` pattern variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The main rule and the `major:` auxiliary rule set both contain a pattern
variable named `rev`, though it is `??rev:expression` in the main rule (line 2)
and `?rev` in the auxiliary rule (line 5). Both pattern variables are
transformed by the `rev:` auxiliary rule in line 7 because both pattern
variables have the name "rev", but they are transformed differently because of
the different natures of the two pattern variables.

In line 5, `?rev` is equivalent to `?rev:*`. The code fragment matched by that
wildcard is the code fragment contained by the pattern variable `?major` in line
2. This code fragment will be an expression, which will be matched and
transformed by the `rev:` rule and will replace `?major` in line 3.

The `?rev` pattern variable in line 5 is a simple pattern variable that contains
only one code fragment. The `rev:` rule in line 7 transforms that fragment as
you would expect.

However, the `??rev` pattern variable in line 2 is a `??`-type pattern variable
containing has zero or more code fragments, so the `rev:` rule transforms each
individually. The `??rev, ...` substitution in line 3 then joins each of the
transformed code fragments with a comma and includes the entire collection in
the macro expansion.

Empty ``??`` pattern variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In line 2, the `?type` variable has a default. If the macro call
does not contain a `type:` property, the default provides a code fragment to
match against the `type:` auxiliary rule set.

In contrast, the `??rev` variable does not have a default. If the call does not
include any `rev:` properties then the pattern variable will not contain a code
fragment. Since the `rev:` rule does not include an empty pattern, you might
expect the macro to fail.

But the macro still works. The `rev:` rule will be applied to each code fragment
in `??rev` individually because it is a ``??``-type pattern variable. Since
there are no code fragments in `??rev`, the `rev:` is not even applied once, so
its lack of an empty pattern is irrelevant.

----------

.. [ver5-call] *Complex syntax call*

   .. code-block:: dylan

      version(major: 1, rev: 0, rev: 4, rev: 2, type: alpha)

.. [ver5-exp] *Complex expansion*

   .. code-block:: dylan

      set-version(concatenate("1", ".", "0", 4", "2", "a"))

.. [ver5] *Complex macro definition*

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

----------

Recursive expansion
===================

Any pattern variable named the same as an auxiliary rule is processed by that
rule. That includes pattern variables in the auxiliary rule itself. This
recursive behavior is useful for processing lists of items.

The `...` pattern variable and substitution syntaxes draw attention to a
recursive rule. Using that syntax, the macros [rec1]_ and [rec2]_ are
equivalent. But I feel there is a good argument for avoiding that syntax for
clarity's sake.

Tracing a call of the macro [rec1-call]_ shows how macro recursion works.

1. The main rule pattern matches. ``?steps`` is set to ``north 5, east 3, south
   1, east 2``.
#. The contents of ``?steps`` is rewritten by the *steps* auxiliary rule set.

   a. The "north" rule is matched against ``north 5, east 3, south 1, east 2``.
      The pattern is a comma-separated pattern, which matches the code fragment.
      The word ``north`` and the token ``5`` match. As described in
      `final-items`:ref:, the ``?steps`` pattern variable belonging to this
      pattern-match operation is set to ``east 3, south 1, east 2``.
   #. The contents of this rule's ``?steps`` variable is rewritten by the *steps*
      auxiliary rule set.
      
      i. The "north," "south," and "west" rules fail to match against ``east 3,
         south 1, east 2``.
      #. The "east" rule matches and the ``?steps`` pattern variable of this
         pattern-match operation (different from any other ``?steps`` variable
         being dealt with) is set to ``south 1, east 2``.
      #. ``?steps`` is rewritten by another pass through the *steps* rule set.
      
         1. The "south" rule matches and its ``?steps`` is set to ``east 2``.
         #. ``?steps`` is rewritten.
         
            a. The "north," "south," and "west" rules fail to match.
            #. The "east" rule is matched against ``east 2``. The word ``east`` and
               the token ``2`` match. The code fragment does not contain a comma, but
               the pattern matches the code fragment without the comma per
               `final-items`:ref:. The ``?steps`` pattern variable will contain an
               empty code fragment.
            #. Even though ``?steps`` contains an empty code fragment, it is still
               rewritten by the *steps* auxiliary rule set.
               
               i. The "north," "south," "west," and "east" rules fail to match against
                  an empty code fragment.
               #. The empty pattern matches. Its expansion is an empty fragment.
               
            #. The ``?steps`` pattern variable of the "east" rule is set to the
               expansion of the auxiliary rule set, i.e., an empty fragment.
            #. The rule's expansion is therefore ``x := x + 2``.
            
         #. The ``?steps`` pattern variable of the "south" rule is set to ``x := x +
            2``.
         #. The rule's expansion is therefore ``y := y + 1; x := x + 2``.
         
      #. The ``?steps`` pattern variable of the "east" rule is set to ``y := y + 1;
         x := x + 2``.
      #. The rule's expansion is therefore ``x := x + 3; y := y + 1; x := x + 2``

…and so on. The key ideas to note are:

- The rule set has to have a non-recursing rule (in this case, ``{ } => { }``)
- Each rule's matching and expansion has its own ``?token`` and ``?steps``
  pattern variable.

----------

.. [rec1] *Recursive macro*

   .. code-block:: dylan
      :linenos:

      define macro path
        { path(?steps) } => { let x = 0; let y = 0; ?steps; values(x, y) }
      steps:
        { north ?:token, ?steps:* } => { y := y - ?token; ?steps }
        { south ?:token, ?steps:* } => { y := y + ?token; ?steps }
        { west ?:token, ?steps:* } => { x := x - ?token; ?steps }
        { east ?:token, ?steps:* } => { x := x + ?token; ?steps }
        { } => { }
      end macro

.. [rec2] *Recursive macro with ellipses*

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

.. [rec1-call] *Recursive macro call*

   .. code-block:: dylan

      let (x, y) = path(north 5, east 3, south 1, east 2)
