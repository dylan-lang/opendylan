.. default-role:: samp
.. highlight:: none
.. _macro-types:


***********
Macro Types
***********

There are four types of macro.

`Body-style definition macro`:dfn:
      This kind of macro lets you create `define x … end` syntax. This is the
      most popular kind of macro. This type of macro can only be used at the top
      level of source code.

`List-style definition macro`:dfn:
      This kind of macro lets you create `define x …` syntax, such as `define
      variable $pi`.

`Statement macro`:dfn:
      This kind of macro lets you create `do-something … end` syntax. Use this
      kind of syntax to simplify blocks or to create new kinds of loops. It is
      most commonly used to simplify resource management.

`Function macro`:dfn:
      This kind of macro lets you create `x(…)` syntax. Use this instead of a
      function call if the syntax you want in the parentheses is more
      complicated than a normal function call, or if there is additional setup
      needed around a normal function call.


Main rules
==========

The pattern of each main rule of a macro (and thus the way the macro is called)
must follow a specific syntactic style depending on the type of macro.

When the Dylan compiler sees a macro call, it first finds the end of the call,
then expands the macro, then recursively expands any macro calls in the
expansion. The compiler is able to recognize inner macro calls along the way,
but if a code fragment *looks* like the end of a macro and is not part of an
inner macro call, the parser will assume that that code fragment *is* the end of
the macro.


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

      { DISTINGUISHING-WORD [`{BODY-PATTERNS}`] [;] end }

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

