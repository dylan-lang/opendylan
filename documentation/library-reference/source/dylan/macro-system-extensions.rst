***********************
Macro System Extensions
***********************

Traced Macros
=============

Macros can be defined with an adjective, ``traced``.  This causes the
macro expander to output some useful information during compilation:

.. code-block:: dylan

    define traced macro when2
      { when2 (?cond:expression) ?:body end } => { if (?cond) ?body end }
    end;

    when2 (23)
      format-out("Hello, world!\n");
    end;

Compiling this will result in this output from the compiler::

    { when2 } > when2 (23) format-out ("Hello, world!\n"); end
    { when2 } < if (23) format-out ("Hello, world!\n"); end

Template Calls
==============

.. note:: This extension was originally described in an appendix to
   `D-Expressions: Lisp Power, Dylan Style
   <http://people.csail.mit.edu/jrb/Projects/dexprs.pdf>`_.
   This text is largely copied from there.

A number of limitations of local rewrite rules require resorting to
top-level auxiliary macros. The first problem is that local rewrite
rules do not have access to pattern variables defined in main rule
sets. The usual solution is to employ an auxiliary macro which takes
those needed variables as extra macro arguments. For example, suppose
we want to add a prefix option to allow the creation of properties with
a common prefix. We need to introduce an auxiliary macro so that the
prefix variable can be in scope when iterating over the properties.
For example, consider the following:

.. code-block:: dylan

    define macro properties-definer
      { define properties ?kind:name
             prefixed-by ?prefix:name ?properties:* end }
        => { define variable ?kind
               = concatenate
                   (prefixed-props (?"prefix") ?properties end) }
      { define properties ?kind:name ?properties:* end }
        => { define variable ?kind
               = concatenate
                   (prefixed-props ("") ?properties end) }
    end macro;

    define macro prefixed-props
      { prefixed-props (?prefix:name) end }
        => { #() }
      { prefixed-props (?prefix:name) ?prop:name; ?more:* end }
        => { list(?prefix ## ?prop),
             prefixed-props (?prefix) ?more end) }
    end macro;

Auxiliary macros are also needed when pattern variables must be walked in two
different ways. Consider a macro, called ``iterate``, for creating internally
recursive procedures that are as convenient as loops. It has the following
basic form:

.. code-block:: dylan

    iterate name (variable = init, ...)
      body
    end iterate

and has the semantics of evaluating the body with the variables initially
bound to the inits and such that any call to ``name`` inside the body
recursively calls the procedure with the variables bound to the arguments
of the call.  In writing a macro for ``iterate``, the parenthesized
fragments must be walked once to extract variables and another time to
extract initial values.  Unfortunately, with the current macro system,
there is no way to walk the same pattern variable with more than one
set of local rewrite rules. Instead, an extra copy of the pattern variable
must be made and passed on to an auxiliary macro:

.. code-block:: dylan

    define macro iterate
      { iterate ?loop:name (?args:*) ?:body end }
        => { iterate-aux ?loop (?args) (?args)
               ?body
             end }
    end macro iterate;

    define macro iterate-aux
      { iterate-aux ?loop:name (?args) (?inits) ?:body end }
        => { local method ?loop (?args) ?body end;
             ?loop(?inits) }

    args:
      { }
        => { }
      { ?:variable = ?:expression, ... }
        => { ?variable, ... }

    inits:
      { }
        => { }
      { ?:variable = ?:expression, ... }
        => { ?expression, ... }
    end macro iterate-aux;

Both of these reasons for needing auxiliary macros are somewhat artificial
because in fact local rewrite rules are really like local functions and
should allow extra arguments and should be directly callable on any
pattern variable. The problem lies in the fact that the local rewrite
rule is artificially tied to one pattern variable by virtue of its name.

In order to overcome this problem, we introduce a direct template call,
obviating the need for auxiliary macros in many cases. This leads to a
much more elegant solution to these more complicated macros. A template
auxiliary rule set call has the following form:

.. code-block:: dylan

   ?@rule-name{ <arbitrary-template-stuff> }

where a new macro punctuation ``?@`` marks a template call. For example,
in the prefixed ``properties-definer`` macro, we can now directly invoke
a local rewrite rule set with extra arguments:

.. code-block:: dylan

    define macro properties-definer
      { define properties ?kind:name
             prefixed-by ?prefix:name ?properties:* end }
        => { define variable ?kind
               = concatenate
                   (?@prefixed-properties{ ?"prefix"; ?properties }) }
      { define properties ?kind:name ?properties:* end }
        => { define variable ?kind
               = concatenate(?@prefixed-properties{ ""; ?properties } }

    prefixed-properties:
      { ?prefix:name }
        => { #() }
      { ?prefix:name; ?property:name; ?more:* }
        => { list(?prefix ## ?property),
             ?@prefixed-properties{ ?prefix; ?more } }
    end macro;

Similarly, ``iterate`` can now be written without auxiliary macros using two
template calls:

.. code-block:: dylan

    define macro iterate2
      { iterate2 ?:name (?bindings:*) ?:body end }
        => { local method ?name (?@vars{ ?bindings }) ?body end;
             ?name(?@inits{ ?bindings }) }

    vars:
      { }
        => { }
      {?:variable=?:expression,... }
        => {?variable,... }

    inits:
      { }
        => { }
      { ?:variable = ?:expression, ... }
        => { ?expression, ... }
    end macro;

We can also introduce a template macro call

.. code-block:: dylan

    ?@{ <arbitrary-template-stuff-that-forms-a-macro-call> }

which acts as a kind of shorthand for the ``:macro`` constraint and permits
the definition of macros for use as shared rewriting tools. For example:

.. code-block:: dylan

    define traced macro mcreverse
      { mcreverse(?list:*) } => { ?list }

    list:
      { } => { }
      { ?:expression } => { ?expression }
      { ?:expression, ... } => { ..., ?expression }
    end macro;

    define traced macro user
      { user(?stuff:*) } => { list(?@{ mcreverse(?stuff) }) }
    end macro;

where the ``traced`` modifier causes macro expansion to be traced. For
example, here’s the trace for ``user(1, 2, 3)``::

    { user } > user(1, 2, 3)
    { mcreverse } > mcreverse(1, 2, 3)
    { mcreverse } < 3,2,1
    { user } < list(3,2,1)

Like normal macro calls, a new hygiene context in created for ``?@{ }``
calls, so you could define ``gensym`` thusly:

.. code-block:: dylan

    define macro gensym
      { gensym() } => { gensymed-name }
    end macro;

