:copyright: Copyright © 2011 Dustin Voss, All Rights Reserved.

.. default-role:: samp
.. highlight:: none
.. sidebar:: Navigation

   :Next:   :doc:`auxiliary-rules`
   :Prev:   :doc:`pattern-variables`
   :Top:    :doc:`index`
   
   .. contents::
      :local:


*************
Substitutions
*************

Pattern variables contain code fragments, which can be inserted into the macro
expansion via a substitution. A substitution looks much like a pattern variable,
but it are on the template side of the rule and has different syntax forms.

A template can only use pattern variables from its corresponding pattern. It
cannot use pattern variables from other rules' patterns.

As a special case, if the template has a separator followed by any of the
substitution forms below, and the substituted code fragment is empty, the
preceding separator is removed. For example, consider this template::

        { ?alpha, ?beta }

If `?alpha` contains `a` and `?beta` is empty, the expansion will not be
`Expansion 1`_, but will instead be `Expansion 2`_. This special case applies
with any of the separators listed in `Separators`_ in place of the comma.

----------

_`Expansion 1`:

   .. code-block:: none
   
      a,

_`Expansion 2`:

   .. code-block:: none

      a

_`Separators`:

   .. code-block:: none
   
      , ; + - * / ^ = == ~= ~== < <= > >= & | :=

----------


Simple substitutions
====================

`?{name}`
        This is the basic substitution. The pattern variable's code fragment is
        inserted into the expansion according to the syntax used in the pattern,
        as described in :doc:`pattern-variables`.


Conversion substitutions
========================

`?#"{name}"`
        The pattern variable's code fragment, which must be a simple name, is
        turned into a symbol and inserted into the expansion.

`?"{name}"`
        The pattern variable's code fragment, which must be a simple name, is
        turned into a string and inserted into the expansion.


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
        As above, but results in a symbol.


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

        Any of the separators [seps]_ may be used in place of a comma in the
        tempate.


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
        macro's caller. See :doc:`hygiene`.
