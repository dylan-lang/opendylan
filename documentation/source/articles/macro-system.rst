:copyright: Copyright © 2011 Dustin Voss, All Rights Reserved.
:orphan:

.. sectionauthor:: Dustin Voss <d_j_v@me.com>
.. default-role:: samp
.. highlight:: dylan

######################
The Dylan Macro System
######################

Dylan macros simplify boilerplate code and provide syntactic shorthand. They are
useful in small jobs within a particular file (e.g. making a series of
repetitive declarations) and for larger jobs (e.g. constructing a GUI through
creating and associating objects). Easy jobs are easy to do with macros, but the
complicated jobs get hard fast.

This document describes how the Dylan macro system works and some techniques you
can use when writing your own macros. I gloss over some of the implementation
details and present this information more informally than the :title:`Dylan
Reference Manual` does.


***********************
Background and Overview
***********************

Macros work on the basis of code fragments. The macro system
does not understand code fragments; it just substitutes some fragments for other
fragments. Once the macro system has substituted and arranged all the code
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
`Macro Types`_).

Let us examine this function macro:

   .. code-block:: dylan
      :linenos:

      define macro table
          { table(?table-class:expression, ?table-contents) }
       => { let ht = make(?table-class); ?table-contents; ht }

          { table(?rest:*) }
       => { table(<table>, ?rest) }

       table-contents:
           { ?key:expression => ?value:expression, ... }
        => { ht[?key] := ?value; ... }

           { }
        => { }
      end macro table

Here are the parts of the macro:

- The distinguishing word is `table`. Whenever the compiler sees `table(...)`,
  it will expand this macro rather than creating a call to a function named
  "table".
- The main rules are in lines 2–6.
- The macro has one set of auxiliary rules in lines 8-13. A set of auxiliary
  rules has a title written as a symbol. This set of auxiliary rules is titled
  `table-contents:`.
- The pattern of the first main rule is in line 2.
- The template of the first main rule is in line 3.
- The patterns in this macro include the pattern variables `?table-class`,
  `?table-contents`, `?rest`, `?key`, and `?value`.
- The substitutions in this macro include those same names.

This macro might be called as follows:

   .. code-block:: dylan

      let lights = table(<string-table>, "red" => "stop", "green" => "go");

But this actual call fragment is what the parser will attempt to match:

   .. code-block:: dylan

      table(<string-table>, "red" => "stop", "green" => "go")

The macro's expansion will be

   .. code-block:: dylan

      let ht = make(<string-table>);
      ht["red"] := "stop";
      ht["green"] := "go";
      ht

and the replacement code will then become

   .. code-block:: dylan

      let lights = begin
        let ht = make(<string-table>);
        ht["red"] := "stop";
        ht["green"] := "go";
        ht
      end;

Note that the expansion is surrounded by `begin` and `end`. Macro expansions
are always surrounded by a begin...end block. This helps with macro hygiene (i.e.
preventing bindings outside of a macro call from being affected by bindings used
in a macro's expansion). See `Hygiene`_.


***********
Macro Types
***********

There are four types of macro.

`Body-style definition macro`:dfn:
      This kind of macro lets you create `define x ... end` syntax. This is the
      most popular kind of macro. Example: `define function`
      (see `DEP-002 <https://opendylan.org/proposals/dep-0002-define-function.html>`_).

`List-style definition macro`:dfn:
      This kind of macro lets you create `define x ...` syntax, such as `define
      variable $pi`.

`Statement macro`:dfn:
      This kind of macro lets you create `do-something ... end` syntax. Use this
      kind of syntax to simplify blocks or to create new kinds of loops. It is
      most commonly used to simplify resource management. Examples include
      :drm:`block`, :drm:`for`, and `with-open-file`.

`Function macro`:dfn:
      This kind of macro lets you create `x(...)` syntax. Use this instead of a
      function call if the syntax you want in the parentheses is more
      complicated than a normal function call, or if there is additional setup
      needed around a normal function call.


Macro definitions
=================

All macros are defined by the :drm:`define macro` macro, which follows this general
syntax, with optional parts in brackets::

      define macro MACRO-NAME
         MAIN-RULE-SET
         [AUXILIARY-RULE-SETS]
      end macro MACRO-NAME

`{MACRO-NAME}`
      For statement and function macros, this is the macro's distinguishing
      word. For body-style and list-style definition macros, though, it is the
      distinguishing word plus `-definer`.

`{MAIN-RULE-SET}`
      One or more pattern/template pairs. The syntax that the patterns all
      follow determine the type of the macro, and are described :ref:`below
      <main-rules>`. The patterns are matched in order; see `Patterns`_.

`{AUXILIARY-RULE-SETS}`
      One or more auxiliary rule sets, described in more detail in
      `Auxiliary Rules`_. Each rule set has a name (which is syntactically a
      symbol) and one or more pattern/template pairs. The name may be written as
      `my-aux-ruleset:` or `#"my-aux-ruleset"`; both are the same.


.. _main-rules:

Main Rules
==========

The pattern of each main rule of a macro (and thus the way the macro is called)
must follow a specific syntactic style depending on the type of macro.

When the Dylan compiler sees a macro call, it first finds the end of the call,
and only afterwards attempts to expand the macro. While looking for the end of
the call, the compiler recognizes inner macro calls along the way and
recursively looks for the end of them first. If a code fragment *looks* like
the end of a macro call, the parser will assume that that code fragment *is*
the end of the macro call. Below, I describe what the end of each type of macro
call *looks* like.


Body-style definition macro
---------------------------

The main rules' patterns must follow this syntax, with optional parts in
brackets::

      { define [MODIFIERS] DISTINGUISHING-WORD [NAME]
          [BODY-PATTERNS] [;]
        end }

`{MODIFIERS}`
      One or more words or pattern variables.

`{NAME}`
      A name or a pattern variable with a name constraint.

`{BODY-PATTERNS}`
      One or more sets of code fragments and pattern variables separated by
      semicolons and/or commas.

The parser will end the macro call at the first matching `end`. The final `end`
in each main rule is the only `end` that the macro's patterns can have.

As a special case, the final `end` matches these code fragments::

      end
      end DISTINGUISHING-WORD
      end DISTINGUISHING-WORD NAME


List-style definition macro
---------------------------

The main rules' patterns must follow this syntax, with optional parts in
brackets::

      { define [MODIFIERS] DISTINGUISHING-WORD [LIST-PATTERNS] }

`{MODIFIERS}`
      One or more words or pattern variables.

`{LIST-PATTERNS}`
      One or more sets of code fragments and pattern variables separated by
      commas.

The parser will end the macro call at the first matching `;` or the end of the
enclosing source code. None of the macro's patterns can have a semicolon, and it
is probably better to avoid `?:body` or `?:case-body` pattern variables.


Statement macro
---------------

The main rules' patterns must follow this syntax, with optional parts in
brackets::

      { DISTINGUISHING-WORD [BODY-PATTERNS] [;] end }

`{BODY-PATTERNS}`
      One or more sets of code fragments and pattern variables separated by
      semicolons and/or commas.

The parser will end the macro call at the first matching `end`. The final `end`
in each main rule is the only `end` that the macro's patterns can have.

As a special case, the final `end` matches these code fragments::

      end
      end DISTINGUISHING-WORD


Function macro
--------------

The main rules' patterns must follow this syntax, with optional parts in
brackets::

      { DISTINGUISHING-WORD ( [BODY-PATTERNS] ) }

`{BODY-PATTERNS}`
      One or more sets of code fragments and pattern variables separated by
      semicolons and/or commas.

The parser will end the macro call when it sees the closing parenthesis. Other
patterns in the macro can also include parentheses, so long as they are matched;
the parser understands nested parentheses.

As a special case, function macros can be called using operator, slot access, or
element access syntax. The function macro has to accept expressions for its
`{BODY-PATTERN}` arguments like a normal function call in order to be used with
these syntaxes.


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
`Macro Types`_, but in general, a pattern is a list of fragments or
pattern variables separated at the highest level by semicolons, then by commas.
That is, a pattern has this syntax::

        FRAGMENTS, FRAGMENTS, ...; FRAGMENTS, FRAGMENTS, ...; ...

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

You can use parentheses, curly brackets ("{...}"), and square brackets to nest
comma- or semicolon-separated patterns inside of other patterns, as in this
example::

        { ?name:name, { ?true-expr:expression; ?false-expr:expression }, ?final:name }

Such a pattern will only match a code fragment with matching bracket characters.
The above pattern will match the first line of the following, but not the
second::

      alpha, {#t; #f;}, beta
      alpha, (#t; #f;), beta


.. _final-items:

Final items
===========

A pattern with at least two list items treats the last item specially. For example, the
pattern `{ ?item-1:*, ?item-2:*, ?item-3:* }` will match any of these code fragments,

   .. code-block:: none
      :linenos:

      alpha, beta, gamma
      alpha, beta
      alpha, beta, gamma, delta, epsilon

and will set the pattern variables as follows:

==============  =======  =======  =======================
Code Fragments  ?item-1  ?item-2  ?item-3
==============  =======  =======  =======================
Line 1          `alpha`  `beta`   `gamma`
Line 2          `alpha`  `beta`
Line 3          `alpha`  `beta`   `gamma, delta, epsilon`
==============  =======  =======  =======================

This special behavior is usually only relevant when the last item in the list is a
wildcard pattern variable (see :ref:`wildcard-variables`). If the pattern were `{
?item-1:*, ?item-2:*, ?item-3:name }` instead, the only matching code fragment would be
line 1, because neither an empty fragment (from line 2) nor `gamma, delta, epsilon` (from
line
3) match the `name` constraint of `?item-3`.


Property lists
==============

The end of a comma-separated list of pattern fragments can include `#rest`,
`#key`, and `#all-keys`, as in this example::

        { ..., #rest ?keys:token, #key ?alpha:token, ?beta:token, #all-keys }

This syntax is *not* used to match a code fragment that contains literal
`#rest`, `#key`, and `#all-keys` fragments. Instead, this syntax matches a code
fragment consisting of keyword/value pairs, called a `property list`:dfn:. An
example of a property list is::

        alpha: "a", beta: "b"

In this code fragment, `alpha:` and `beta:` are the keyword or `symbol
parts`:dfn: of the property list and `"a"` and `"b"` are the `value parts`:dfn:.

If you want to match literal `#rest`, `#key`, or `#all-keys` fragments, escape
them in the pattern like `\\#rest`, `\\#key`, or `\\#all-keys`.

If you write a pattern that contains `#all-keys`, you must also include `#key`.
There are several variations on this syntax; they are described in
:ref:`proplist-variables`.

`#rest`, `#key`, and `#all-keys` must be the only pattern fragments in their
comma-separated sub-pattern, and that sub-pattern must be the last of several
comma-separated sub-patterns. Here are some examples of when it **is** or **is
not** valid to use this syntax in a pattern::

        /* valid */     { #key ?alpha:token }
        /* not valid */ { ?alpha:token #key ?beta:token }
        /* valid */     { ?anything:*, #key ?alpha:token, #all-keys }
        /* not valid */ { #key ?alpha:token, #all-keys, ?anything:* }
        /* valid */     { #key ?alpha:token, #all-keys; ?anything:* }
        /* not valid */ { #key ?alpha:token, #key ?beta-token }
        /* valid */     { #key ?alpha:token; #key ?beta-token }


*****************
Pattern Variables
*****************

A macro pattern variable pulls out and transforms part of a code fragment. This
partial code fragment is then substituted into the macro's expansion. The
substitution can be altered in some ways, or intercepted and more extensively
transformed using auxiliary rules.

Every pattern variable has a name and a `constraint`:dfn:. The constraint forces
the pattern variable to only match certain code fragments. If the pattern
variable cannot match, the pattern containing the variable will not match.
Unless the pattern variable has the wildcard (or `*`) constraint, it can only
match a code fragment that is part of the core language or a macro call; a
pattern variable cannot match a code fragment that is only legal with respect to
a given inner macro. An example of this is given in the discussion of the
`?:body` constraint below.

The scope of a pattern variable is the rule that uses it. Other rules or
auxiliary rule sets cannot use the pattern variable.


Simple pattern variables
========================

`?{name}:{constraint}`
        This is the basic pattern variable.

`?:{constraint}`
        This is a pattern variable where its constraint is also its name. For
        example, `?:expression` is equivalent to `?expression:expression`,
        that is, a pattern variable named `expression` with a constraint of
        `expression`.

`?{name}:name`
        This matches a name.

`?{name}:token`
        This matches a name, operator, or simple literal such as a string,
        character constant, or number. It does not match vector literals or
        function calls.

`?{name}:expression`
        This matches any expression, including vector literals, function calls,
        and begin...end blocks.

`?{name}:variable`
        This matches a variable name and optional specialization, for example,
        `color` or `color :: <color>`.

`?{name}:name :: ?{specialization}:expression`
        This matches a variable name and optional specialization, like
        `?:variable`, but lets you extract each part separately. If the code
        fragment just has the name part, the substitution for
        `?{specialization}` will be `<object>`. Note that `?{specialization}`
        will not match every expression; it will only match an expression that
        happens to also be a valid type specialization.


.. _proplist-variables:

Property list pattern variables
===============================

`#rest ?{name}:{constraint}`
        This matches a property list where every value part meets the
        constraint. If the constraint is `*`, any value part will match. The
        substitution for `?{name}` is the entire property list code fragment,
        including both the symbol and value parts of each property.

`#key ?{prop-1}:{constraint}, ?{prop-2}:{constraint}`
        This matches a property list that only includes the `{prop-1}:` and
        `{prop-2}:` properties. If the property list includes any other property
        such as `alpha:` or if either `{prop-1}:` or `{prop-2}:` are missing,
        this pattern variable will not match. Additionally, the properties'
        value parts have to meet the constraints given. If the constraint is
        `*`, any value part will match.

        The substitution for `?{prop-1}` is the value part of the `{prop-1}:`
        property.

`#key ??{prop-1}:{constraint}, ??{prop-2}:{constraint}`
        This matches a property list that has several properties with a symbol
        part of `{prop-1}:` or `{prop-2}:`. The substitution for `??{prop-1}` is
        several code fragments, each being the value part of a `{prop-1}:`
        property. The substitution may use one of the separators listed in
        :ref:`finalitems-subst` between each code fragment.

        For example, consider this pattern::

                { #key ??my-key:name }

        It will match the following code fragment::

                my-key: alpha, my-key: beta

        The substitution will be the following code fragment::

                alpha beta

        If the property list did not include a `my-key:` property, the
        substitution for `??my-key` would have been empty.

`#key ?{prop}:{constraint}, #all-keys`
        This matches a property list that contains `{prop}:`, but also matches
        if the property list contains other properties in addition to
        `{prop}:`.

        For example, consider this code fragment::

                my-key: alpha, another-key: beta

        This pattern would not match::

                { #key ?my-key:name }

        However, this pattern would::

                { #key ?my-key:name, #all-keys }

`#key ?{prop}:{constraint} = {default-value}`
        This matches a property list that contains `{prop}:`, but also matches
        a property list that is missing that property. If the property is
        missing, the substitution will be the default value given.

        The default value is not evaluated during macro expansion. Instead, it
        is simply treated as a code fragment and substituted for `?{prop}` in
        the template. The default value code fragment does not have to abide by
        the pattern variable's constraint. For example, the following pattern is
        valid even though `#f` is not a name::

                { #key ?name:name = #f }

`#key ??{prop}:{constraint} = {default-value}`
        This matches a property list containing zero or more `{prop}:`
        properties. If `{prop}:` properties are present, the substitution for
        `??{prop}` will be a sequence of value parts as it is for the `#key
        ??{prop}:{constraint}` pattern. However, if the property list does not
        have any `{prop}:` properties, the substitution will be a sequence of
        only one code fragment — the default value code fragment.

`#rest {...}, #key {...}`
        With these two syntaxes are combined, both match separately against the
        same property list.


Body and macro pattern variables
================================

`?{name}:body`
        This matches a series of semicolon-separated statements and expressions.
        If the code fragment does not have any statements or expressions, the
        substitution will be `#f`. The substitution will wrap the code
        fragment in `begin` and `end` to make an expression.

        A `?:body` pattern variable matches statements and expressions in a code
        fragment until it reaches some word, called an `intermediate word`:dfn:.
        You must ensure that all your `?:body` pattern variables are either
        followed by a word, or followed by a pattern variable referring to an
        auxiliary rule set whose rules all start with a word. Those words will
        become the intermediate words that tell the parser to stop matching the
        pattern variable.

        In this example, the `?:body` variable matches all code fragments up to
        `endif`::

                { if (?:expression) ?:body endif }

        In this example with auxiliary rules, the `?:body` variable matches
        all code fragments up to `endif` or `else`::

                { if (?:expression) ?:body ?else-or-end }
                else-or-end:
                { endif }
                { else ?:body endif }

        In this example, the macro will not work because the pattern does not
        include an intermediate word following the `?:body` variable::

                { when (?:expression) ?:body }

        A `?:body` pattern variable matches semicolons. It cannot be used in a
        series of comma- or semicolon-separated sub-patterns, and cannot itself
        be followed by a comma or semicolon in the pattern. The following will
        not work::

                { if (?:expression) ?:body; ?else-or-end }

        A `?:body` pattern variable does not match things that are not
        statements or expressions. For example, the following pattern is
        designed to be used with the above `if` macro::

                { if-into (?:expression) ?rest:body => ?:name } => { let ?name = if (?expression) ?rest }

        You might expect that you can use this macro on the following code::

                if-into (x = #f) format-out("false") else x + 1 endif => x

        However, the `?rest:body` variable will not match the words `else` or
        `endif` because they are not part of the core Dylan language. They are
        not statements or expressions. Those words are actually an extension to
        the language allowed by the `if` macro, but the `if` macro will never
        see them because the `?rest:body` variable does not match or pass them
        on to the `if` macro. To match arbitrary fragments for the `if` macro,
        the `if-into` macro must use the wildcard constraint on the variable
        instead, like `?rest:*`.

`?{name}:case-body`
        This matches a list of cases separated by semicolons, where each case
        consists of: a list of expressions, an arrow, and a body. For example,
        this pattern variable would match the following::

                "red" => "stop";
                "green", "blue" => "go";
                otherwise => error("I don't know what this means.")

        Since a case includes a body, a `?:case-body` pattern variable must be
        followed with an intermediate word just like a `?:body` pattern
        variable and cannot be followed by a comma or semicolon.

`?{name}:macro`
        This matches any macro call. The substitution will be the expanded
        macro, without the begin...end block that normally surrounds macro
        expansions.

        While you can use `?:expression` and `?:body` pattern variables to match
        macro calls, their substitutions will include a called macro's begin...end
        wrapper, and `?:expression` can only match function macro calls.


.. _wildcard-variables:

Wildcard pattern variables
==========================

`?{name}:*`
        Wildcard pattern variables match as many code fragments as can be
        matched before the next comma, semicolon, or other pattern fragment in
        the pattern. For example, consider the following pattern::

                { ?many-things:* ?:name }

        `?many-things` will match everything up to but not including a name. The
        substitution for `?many-things` will be everything except that name. If
        the code fragment only has a name, the substitution for `?many-things`
        will be empty.

        There can only be one wildcard pattern variable in a comma- or
        semicolon-separated sub-pattern. Each must be separated from other
        wildcards by a semicolon or comma. For example, this is not a legal
        pattern::

                { ?first:* ?second:* }

        However, this is::

                { ?first:*, ?second:* }

        As a special case, main rules of definition macros can have wildcards in
        both the `{MODIFIERS}` part and the `{LIST-PATTERN}` or `{BODY-PATTERN}`
        part without an intervening comma or semicolon. This allows patterns
        like the following that would normally not be allowed::

                { define ?modifiers:* collection ?:name ?contents:* end }

        Finally, consider this pattern::

                { ?first:*, ?second:* }

        As described in `Patterns`_, it will match any of the following::

                alpha, beta
                alpha, beta, gamma
                alpha,
                alpha

        In all cases, the wildcard constraint on `?first` will match up to the
        first comma in the code fragment. `?first` will contain `alpha`.
        `?second` will contain nothing, `beta`, or `beta, gamma`.


Auxiliary rule set pattern variables
====================================

`?{aux-rules}`
        This syntax can only be used when there is an auxiliary rule set named
        the same as the pattern variable. It is equivalent to `?{aux-rules}:*`.
        See `Auxiliary Rules`_.

`...`
        This syntax can only be used within an auxiliary rule set. If the rule
        set is named `my-aux-rules`, `...` is equivalent to `?my-aux-rules:*`.


*************
Substitutions
*************

Pattern variables contain code fragments, which can be inserted into the macro
expansion via a substitution. A substitution looks much like a pattern variable,
but it is on the template side of the rule and has different syntax forms.

A template can only use pattern variables from its corresponding pattern. It
cannot use pattern variables from other rules' patterns.


.. _finalitems-subst:

Final items
===========

As a special case, if the template has a separator followed by any of the
substitution forms below, and the substituted code fragment is empty, the
preceding separator is removed. For example, consider this template::

        { ?alpha, ?beta }

If `?alpha` contains `a` and `?beta` is empty, the expansion will not be `a,`, but will
instead be `a`. This special case applies with any of the following separators in place
of the comma: `, ; + - * / ^ = == ~= ~== < <= > >= & | :=`



Simple substitutions
====================

`?{name}`
        This is the basic substitution. The pattern variable's code fragment is
        inserted into the expansion according to the syntax used in the pattern,
        as described in `Pattern Variables`_.


Conversion substitutions
========================

`?#"{name}"`
        The code fragment of the pattern variable `{name}`, which must be a
        simple name, is turned into a symbol and inserted into the expansion.

`?"{name}"`
        The code fragment of the pattern variable `{name}`, which must be a
        simple name, is turned into a string and inserted into the expansion.


Concatenation substitutions
===========================

`"{prefix}" ## ?{name} ## "{suffix}"`
        The prefix and suffix are added to the pattern variable's code fragment,
        which must be a simple name. The result is inserted into the expansion.
        Either the prefix or the suffix may be omitted.

        For example, consider a pattern variable, `?name-part`, that contains
        the following code fragment::

                alpha

        The pattern variable is used by the following template::

                { ?name-part ## "-function" }

        The expansion will be the following code fragment::

                alpha-function

`"{prefix}" ## ?"{name}" ## "{suffix}"`
        As above, but results in a string. In the above example, the resulting
        code fragment would be the following::

                "alpha-function"

`"{prefix}" ## ?#"{name}" ## "{suffix}"`
        As above, but results in a symbol::

                #"alpha-function"

        Or, equivalently::

                alpha-function:


List substitutions
==================

`??{name} ...`
        Used with a `??`-style pattern variable to make a list. Consider a
        pattern variable, `??name-parts`, that contains the following code
        fragments::

                alpha beta gamma

        The pattern variable is referenced by the following template and
        substitution::

                { ??name-parts ... }

        The expansion will be the following code fragment::

                alpha beta gamma

`??{name}, ...`
        As above, but the expansion would be the following::

                alpha,beta,gamma

        Consider if `??name-parts` contained the following code fragment::

                alpha

        The expansion would be the following, without any commas::

                alpha

        Any of the following separators may be used in place of a comma in
        the template: `, ; + - * / ^ = == ~= ~== < <= > >= & | :=`


Auxiliary rule set substitution
===============================

`...`
        This syntax can only be used within an auxiliary rule set. If the rule
        set is named `my-aux-rules`, this syntax is equivalent to
        `?my-aux-rules`.


Unhygienic reference
====================

`?={binding}`
        This is not a substitution, but a way to refer to a binding in the
        macro's caller. See `Hygiene`_.


******************************
Auxiliary Rules and Expansions
******************************

Auxiliary Rules
===============

Auxiliary rules transform the code fragment contained in a pattern variable
before it is substituted into a template.

Auxiliary rule sets follow the syntax described in `Patterns`_ and have the
behaviors described in that section. They do not have the special elements like
`define` or `{modifiers}` shown in `Main Rules`_, but the macro type does
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
`Patterns`_, including the `#key` and `??{name}:{constraint}` forms.

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

First, let us consider the `version` macro below.

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

The macro is called like this

   .. code-block:: dylan

      version("1.2", alpha)

and the generated code looks like this:

   .. code-block:: dylan

      set-version("1.2" "a")

   .. tip:: Dylan compilers concatenate consecutive literal strings such as `"1.2" "a"`,
            giving `"1.2a"`.

The `?type` pattern variable in line 1 of the macro definition matches `alpha`
in the call. After the variable matches, the `type:` auxiliary rule set in lines
4–7 rewrites the contents of the pattern variable according to the matching rule
in line 5. The matching rule expands to the string `"a"`, which replaces the
`alpha` code fragment in the pattern variable. In the main rule's template (line
3), the pattern variable (now containing `"a"`) is substituted into the
expansion.

Effect of constraints
^^^^^^^^^^^^^^^^^^^^^

Now consider if the auxiliary rules were rewritten this way:

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

This macro is intended to be called like this

   .. code-block:: dylan

      version("1.0", alpha, "1")

to create a version number like `"1.0a1"`. However, the macro will never succeed. `?type`
in line 2 has the `name` constraint, so it cannot match the call, which includes a comma
and an additional clause. The `type:` auxiliary rule set will not even be consulted and
macro expansion will fail.


Empty and missing code fragments
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

An auxiliary rule set can match against a missing code fragment. Consider the
following macro call in relation to the `version` macros above.

.. code-block:: dylan

   version("1.0")

With this macro call, the `?number` pattern variable would contain `"1.0"` and
`?type` would be empty, as described in :ref:`final-items`. The macro would fail
to match this code fragment, since the `name` constraint of the `?type`
variable does not match a missing code fragment.

If we changed the macro definition to include a wildcard constraint, like this,

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

the macro would still fail to match the code fragment because,
while the `?type` pattern variable itself will match, the `type:` auxiliary rule
set does not have a pattern that matches a missing code fragment. We would also
have to add the rule highlighted below:

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


Complex expansion
-----------------

Now suppose we wanted to support this syntax:

   .. code-block:: dylan

      version(major: 1, rev: 0, rev: 4, rev: 2, type: alpha)

This macro should expand to

   .. code-block:: dylan

      set-version(concatenate(format-to-string("%s", 1),
                              ".",
                              format-to-string("%s", 0),
                              format-to-string("%s", 4),
                              format-to-string("%s", 2),
                              "a"))

to generate a version number like `"1.042a"`. The macro could be defined like this:

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
containing zero or more code fragments, so when acting on *it*, the `rev:`
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

The `...` pattern variable and substitution syntaxes draw attention to a recursive rule
and make the author's intention explicit. Using that syntax, these two `path` macros are
equivalent:

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

But if I may editorialize, I feel there is a good argument for avoiding that syntax for
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

\...and so on. The key ideas to note are:

- The rule set has to have a non-recursing rule (in this case, `{ } => { }`)
- Each rule's matching and expansion has its own `?token` and `?steps`
  pattern variable.


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

In this table, each lexical scope is identified by its number as "Box 1"
through "Box 5". The table describe which bindings defined in each column's
lexical scope are visible in the lexical scope of each row. For example, the
table shows that the only bindings from Box 1 visible in Box 2 are those that
are captured by one of Macro A's pattern variables and included in the
expansion.

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

Say this is the Macro A defined in Box 4,

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

and it is called in Box 1 like so:

.. code-block:: dylan

   let i = 0;
   repeat
     if (i == 100) stop!() end;
     i := i + 1;
   end

The `?=stop!` substitution in line 3 of the macro becomes a reference to a
binding visible in Boxes 1 and 2. In Box 1, the binding is visible as `stop!`.
In Box 2 (the expansion itself), the binding is visible as `?=stop!` and can be
used like any binding (e.g., `format-out("%=", ?=stop!)`).

Note that that a macro expansion cannot create a new name visible outside of the
macro call itself. In other words, Box 2 cannot create a binding for use
elsewhere in Box 1 unless Box 1 supplies the name to be defined.

For example, given this macro,

   .. code-block:: dylan
      :linenos:

      define macro do-then-foo
        { do-then-foo(?:expression) ?:body end }
          => { let ?=foo = ?expression; ?body }
      end macro

one might expect the macro call

   .. code-block:: dylan

      do-then-foo("Hello\n") format-out(foo) end;
      format-out(foo)

would print "Hello" twice, but the code does not compile. Because every macro expansion
is implicitly surrounded by `begin...end` as described in `Background and Overview`_,
the example expands into:

   .. code-block:: dylan

      begin
        let foo = "Hello\n";
        format-out(foo)
      end;
      format-out(foo)

After the macro call, `foo` is no longer in scope.


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
- In Open Dylan, use `define traced macro` to get additional debug output while
  developing macros.


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
begin...end, resulting in this invalid syntax::

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
