.. default-role:: samp
.. highlight:: none
.. _patterns:


********
Patterns
********

Pattern matching follows these basic rules:

- Pattern-matching starts and ends with the main rule set.
- Patterns in a rule set are tried in order. If a pattern does not match the
  code fragment, the next pattern is tried, and so on. If none of the patterns
  in a rule set match, macro expansion fails.
- When determining whether a pattern matches a code fragment, the compiler will
  not consider auxiliary rules. Any pattern variable corresponding to an
  auxiliary rule matches like any other pattern variable with the same
  constraint.
- If no patterns in an auxiliary rule set match, macro expansion fails. The
  compiler does not backtrack and try a different earlier rule.


Subdivisions
============

A main rule pattern has elements like `define` and `end` as described in
`macro-types`:ref:, but in general, a pattern is a list of fragments or
pattern variables separated at the highest level by semicolons, then by commas.
That is, a pattern has this syntax::

        FRAGMENTS, FRAGMENTS, …; FRAGMENTS, FRAGMENTS, …; …

The parser matches each semicolon-separated sub-pattern individually, and only
then matches the comma-separated sub-patterns within. This can have surprising
side effects in combination with recursive auxiliary rules.

A pattern can include a trailing comma or semicolon, but this is strictly
decorative. The pattern will match a trailing separator in the code fragment
whether or not the pattern contains a trailing separator. Keep this in mind. The
following patterns are equivalent::

        { ?:name }
        { ?:name; }
        { ?:name, }

Any of them will match any of these code fragments::

        alpha
        alpha,
        alpha;
        alpha,;

You can use parentheses, curly brackets ("{…}"), and square brackets to nest
comma- or semicolon-separated patterns inside of other patterns, as in this
example::

        { ?name:name, { ?true-expr:expression; ?false-expr:expression }, ?final:name }

Such a pattern will only match a code fragment with matching bracket characters.
The above pattern will match [#]_ but not [#]_.

----------

.. [#]
   
   .. code-block:: none
   
      alpha, {#t; #f;}, beta

.. [#]
   
   .. code-block:: none
   
      alpha, (#t; #f;), beta

----------


.. _final-items:

Final items
===========

A pattern with at least two list items treats the last item specially. For
example, the pattern [list1]_ will match any of the code fragments of [frags]_
and set the pattern variables as follows:

========  =======  =======  =======================
Fragment  ?item-1  ?item-2  ?item-3
========  =======  =======  =======================
Line 1    `alpha`  `beta`   `gamma`
Line 2    `alpha`  `beta`   
Line 3    `alpha`  `beta`   `gamma, delta, epsilon`
========  =======  =======  =======================

This special behavior is usually only relevant when the last item in the list is
a wildcard pattern variable (see `pattern-variables`:ref:). If the pattern were
[list2]_ instead, the only matching code fragment would be line 1, because
neither an empty fragment (from line 2) nor `gamma, delta, epsilon` (from line
3) match the `name` constraint of `?item-3`.

----------

.. [list1] *Pattern ending in wildcard*

   .. code-block:: none

      { ?item-1:*, ?item-2:*, ?item-3:* }

.. [list2] *Pattern ending in name*

   .. code-block:: none

      { ?item-1:*, ?item-2:*, ?item-3:name }

.. [frags] *Code fragments*

   .. code-block:: none
      :linenos:

      alpha, beta, gamma
      alpha, beta
      alpha, beta, gamma, delta, epsilon

----------


Property lists
==============

The end of a comma-separated list of pattern fragments can include `#rest`,
`#key`, and `#all-keys`, as in this example::

        { …, #rest ?keys:token, #key ?alpha:token, ?beta:token, #all-keys }

If you write a pattern that contains `#all-keys`, you must also include `#key`.
There are several variations on this syntax; they are described in
`pattern-variables`:ref:.

`#rest`, `#key`, and `#all-keys` must be the only pattern fragments in their
comma-separated sub-pattern, and that sub-pattern must be the last of several
comma-separated sub-patterns. Here are some examples of when it is or is not
valid to use this syntax in a pattern::

        /* valid */   { #key ?alpha:token }
        /* invalid */ { ?alpha:token #key ?beta:token }
        /* valid */   { ?anything:*, #key ?alpha:token, #all-keys }
        /* invalid */ { #key ?alpha:token, #all-keys, ?anything:* }
        /* valid */   { #key ?alpha:token, #all-keys; ?anything:* }
        /* invalid */ { #key ?alpha:token, #key ?beta-token }
        /* valid */   { #key ?alpha:token; #key ?beta-token }

This syntax is not used to match a code fragment that contains corresponding
literal `#rest`, `#key`, and `#all-keys` fragments. Instead, this syntax
matches a code fragment consisting of keyword/value pairs, called a `property
list`:dfn:. An example of a property list is::

        alpha: "a", beta: "b"

In this code fragment, `alpha:` and `beta:` are the symbol parts of the
property list and `"a"` and `"b"` are the value parts.

If you want to match literal `#rest`, `#key`, or `#all-keys` fragments, escape
them in the pattern like `\#rest`, `\#key`, or `\#all-keys`.
