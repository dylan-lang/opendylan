:copyright: Copyright © 2011 Dustin Voss, All Rights Reserved.

.. default-role:: samp
.. highlight:: none
.. sidebar:: Navigation

   :Next:   :doc:`substitutions`
   :Prev:   :doc:`patterns`
   :Top:    :doc:`index`
   
   .. contents::
      :local:


*****************
Pattern Variables
*****************

A macro pattern variables pulls out and transforms part of a code fragment. This
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
        and begin…end blocks.

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

`#rest {…}, #key {…}`
        With these two syntaxes are combined, both match separately against the
        same property list.


Body and macro pattern variables
================================

`?{name}:body`
        This matches a series of semicolon-separated statement and expressions.
        If the code fragment does not have any statements or expressions, the
        substitution will be `#f`. The substitution will wrap the code
        fragment in `begin` and `end` to make an expression.

        A `?:body` pattern variable matches statements and expressions in a code
        fragment until it reaches some word, called an `intermediate word`:dfn:.
        You must ensure that all your `?:body` pattern variables are either
        followed by a word, or followed by a pattern variable referring to an
        auxiliary rule set whose rules all start with a word. Those word will
        become the intermediate words that tells the parser to stop matching the
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
        macro, without the begin…end block that normally surrounds macro
        expansions.

        While you can use `?:expression` and `?:body` pattern variables to match
        macro calls, their substitutions will include a called macro's begin…end
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

        As described in :doc:`patterns`, it will match any of the following::

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
        See :doc:`auxiliary-rules`.

`...`
        This syntax can only be used within an auxiliary rule set. If the rule
        set is named `my-aux-rules`, `...` is equivalent to `?my-aux-rules:*`.

