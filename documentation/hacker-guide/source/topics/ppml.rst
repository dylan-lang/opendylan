****************
The PPML library
****************

.. current-library:: ppml
.. current-module:: ppml

Program notes need to be stored and later displayed in browsers.  This
presents us with two problems.  At the point of creation we have no way of
knowing  the column width that will be used when the note is displayed.
There may even be more than one width if we want to be smart when a browser
window is resized.  A second problem arises if we store a program note in
the form of a condition string + arguments and the arguments are context
sensitive.  We could just save everything as a string, but then the logical
structure of the message is lost.  An alternative is to store the text in
a more structured form.  This is the purpose of the :class:`<ppml>` class
and its derivatives.   The interface is based on Oppen's 1980 TOPLAS paper.

The PPML module
***************

PPML Tokens and Constructors
============================

.. class:: <ppml>
   :abstract:

   :superclasses: :drm:`<object>`

   :description:

     The abstract base class for all PPML tokens.

.. class:: <ppml-block>

   :superclasses: :class:`<ppml>`

   :keyword break-type: An instance of :type:`<ppml-break-type>`.
   :keyword constituents: An instance of :type:`<ppml-sequence>`.
   :keyword offset: An instance of :type:`<nat>`.

   :description:

     To add structure to the output, we can package up a sequence
     of tokens into a block.  There are a couple of attributes
     associated with a block.  The *offset* indicates how much
     to indent subsequent lines of the block if a break is
     necessary.  When a block is longer than a line then we have
     a number of options.  We can display as much on each line as
     possible, only breaking when really necessary.  Alternatively,
     we can break the block at each break point, e.g.::

            aaa             aaa bbb
            bbb             ccc ddd
            ccc
            ddd

     The choice of layout depends on whether the *break-type*
     attribute of the block is ``#"consistent"`` or ``#"inconsistent"``.
     The third alternative is ``#"fit"``. This suppresses all breaks
     and truncates the output if it won't fit on a line.

     The size of the block is cached in the block for efficiency.

.. function:: ppml-block

   :signature: ppml-block (constituents #key offset type) => (ppml)

   :parameter constituents: An instance of :type:`<ppml-sequence>`.
   :parameter #key offset: An instance of :type:`<nat>`.
   :parameter #key type: An instance of :type:`<ppml-break-type>`.
   :value ppml: An instance of :class:`<ppml-block>`.

.. class:: <ppml-break>

   :superclasses: :class:`<ppml>`

   :keyword blank-space: An instance of :type:`<nat>`.
   :keyword offset: An instance of :type:`<nat>`.

   :description:

     A ``<ppml-break>`` indicates a position in the output where it is
     permissible to break the output if it won't fit on a single line.
     If we don't need to break the line then we output blank-space spaces.
     If we do need to break then we indent offset spaces relative to the
     current line indent.

.. function:: ppml-break

   :signature: ppml-break (#key space offset) => (ppml)

   :parameter #key space: An instance of :type:`<nat>`.
   :parameter #key offset: An instance of :type:`<nat>`.
   :value ppml: An instance of :class:`<ppml-break>`.

.. class:: <ppml-browser-aware-object>

   :superclasses: :class:`<ppml>`

   :keyword object: An instance of :drm:`<object>`.

   :description:

     The browser "knows" about some of the objects manipulated by
     the compiler, e.g. the various kinds of definition, and so
     we store these directly.  Furthermore we recompute the ppml
     representation of the object every time token-size is called
     as the representation may depend on browser settings.

.. function:: ppml-browser-aware-object

   :signature: ppml-browser-aware-object (o) => (ppml)

   :parameter o: An instance of :drm:`<object>`.
   :value ppml: An instance of :class:`<ppml-browser-aware-object>`.

.. class:: <ppml-separator-block>

   :superclasses: :class:`<ppml-block>`

   :keyword separator: An instance of :type:`<ppml-sequence>`.

   :description:

     When constructing blocks representing collections it is wasteful
     to explicitly store the separators between elements.  The
     :class:`<ppml-separator-block>` class captures this common case.

     The default value for the *separator* is
     ``vector(ppml-string(","), ppml-break(space: 1))``.

.. function:: ppml-separator-block

   :signature: ppml-separator-block (constituents #key separator offset type left-bracket right-bracket) => (ppml)

   :parameter constituents: An instance of :type:`<ppml-sequence>`.
   :parameter #key separator: An instance of :type:`<ppml-sequence>`.
   :parameter #key offset: An instance of :type:`<nat>`.
   :parameter #key type: An instance of :type:`<ppml-break-type>`.
     The default value is ``#"inconsistent"``.
   :parameter #key left-bracket: An instance of ``false-or(<ppml>)``.
   :parameter #key right-bracket: An instance of ``false-or(<ppml>)``.
   :value ppml: An instance of :class:`<ppml>`.

.. class:: <ppml-string>

   :superclasses: :class:`<ppml>`

   :keyword string: An instance of :drm:`<byte-string>`.

   :description:

     The simplest ppml token is just a string.

.. function:: ppml-string

   :signature: ppml-string (str) => (ppml)

   :parameter str: An instance of :drm:`<byte-string>`.
   :value ppml: An instance of :class:`<ppml-string>`.

.. class:: <ppml-suspension>

   :superclasses: :class:`<ppml>`

   :keyword cache-token?: An instance of :drm:`<boolean>`.
     The default value is ``#t``.
   :keyword pair: Either an instance of :class:`<ppml>` or
     a :drm:`<pair>` of :drm:`<function>` and its arguments.

   :description:

     Sometimes it is more space efficient to delay the construction
     of the :class:`<ppml>` equivalent of an object until we need
     to print it.  The :class:`<ppml-suspension>` class supports this.
     It contains either a :class:`<ppml>` token, or a pair of a function
     and its arguments.  When we need a token and encounter the pair we
     apply the function to its arguments.  This should return an
     instance of :class:`<ppml>`.  Optionally we can overwrite the
     pair by the result.

.. function:: ppml-suspension

   :signature: ppml-suspension (fun #rest args) => (ppml)

   :parameter fun: An instance of :drm:`<function>`.
   :parameter #rest args: An instance of :drm:`<object>`.
   :value ppml: An instance of :class:`<ppml-suspension>`.

Conversion to PPML
==================

.. method:: as
   :specializer: class == <ppml>, <object>

   :parameter class: The class :class:`<ppml>`.
   :parameter object: An instance of :drm:`<object>`.
   :value ppml: An instance of :class:`<ppml>`.

   :description:

     Returns the result of calling ``print-object`` on the
     object as PPML. (It does this by using ``"%="`` along
     with ``format-to-string``.

.. method:: as
   :specializer: class == <ppml>, <byte-string>

   :parameter class: The class :class:`<ppml>`.
   :parameter object: An instance of :drm:`<byte-string>`.
   :value ppml: An instance of :class:`<ppml>`.

   :description:

     Returns the quoted string value as PPML.

.. method:: as
   :specializer: class == <ppml>, <symbol>

   :parameter class: The class :class:`<ppml>`.
   :parameter object: An instance of :drm:`<symbol>`.
   :value ppml: An instance of :class:`<ppml>`.

   :description:

     Returns the string value of the symbol as PPML.

.. method:: as
   :specializer: class == <ppml>, <collection>

   :parameter class: The class :class:`<ppml>`.
   :parameter object: An instance of :drm:`<collection>`.
   :value ppml: An instance of :class:`<ppml>`.

   :description:

     Returns PPML representing the collection as a comma-separated list
     surrounded by ``#(...)``.

.. method:: as
   :specializer: class == <ppml>, <explicit-key-collection>

   :parameter class: The class :class:`<ppml>`.
   :parameter object: An instance of :drm:`<explicit-key-collection>`.
   :value ppml: An instance of :class:`<ppml>`.

   :description:

     Returns PPML representing the collection.

.. method:: as
   :specializer: class == <ppml>, <vector>

   :parameter class: The class :class:`<ppml>`.
   :parameter object: An instance of :drm:`<vector>`.
   :value ppml: An instance of :class:`<ppml>`.

   :description:

     Returns PPML representing the vector as a comma-separated list
     surrounded by ``#[...]``.

.. method:: as
   :specializer: class == <ppml>, <list>

   :parameter class: The class :class:`<ppml>`.
   :parameter object: An instance of :drm:`<list>`.
   :value ppml: An instance of :class:`<ppml>`.

   :description:

     Returns PPML representing the list as a comma-separated list
     surrounded by ``#(...)``.

Printing / Formatting
=====================

.. generic-function:: format-to-ppml

   :signature: format-to-ppml (string #rest args) => (ppml)

   :parameter string: An instance of :drm:`<byte-string>`.
   :parameter #rest args: An instance of :drm:`<object>`.
   :value ppml: An instance of :class:`<ppml>`.

   :description:

     We insert breaks at the places where arguments are inserted
     in the format string.  This will hopefully give us reasonable
     output, but not always as good as we could do by hand.  We
     separate out the processing of the format string so that we
     can share the constant components of the resulting ppml-block
     if the same format expression is used multiple times.

.. generic-function:: ppml-format-string

   :signature: ppml-format-string (string) => (f)

   :parameter string: An instance of :drm:`<byte-string>`.
   :value f: An instance of :drm:`<function>`.

   :description:

     Used by :gf:`format-to-ppml`.

.. generic-function:: ppml-print

   :signature: ppml-print (t pp) => ()

   :parameter t: An instance of :class:`<ppml>`.
   :parameter pp: An instance of :class:`<ppml-printer>`.

   :description:

     This is the best way to display PPML.

.. generic-function:: ppml-print-one-line

   :signature: ppml-print-one-line (t pp) => ()

   :parameter t: An instance of :class:`<ppml>`.
   :parameter pp: An instance of :class:`<ppml-printer>`.

   :description:

     This prints in the same way as :gf:`ppml-print`, but will limit
     the output to a single line. It does this by internally using
     a line-break-type of ``#"fit"``.

.. class:: <ppml-printer>

   :superclasses: :drm:`<object>`

   :keyword margin: An instance of :type:`<nat>`.
   :keyword newline-function: An instance of :drm:`<function>`, taking
     no arguments. The default value writes a newline to
     ``*standard-output*``.
   :keyword output-function: An instance of :drm:`<function>`, taking
     a single :drm:`<string>` argument. The default value writes to
     ``*standard-output*``.
   :keyword terse-depth: An instance of :drm:`<integer>`. The default
     value is ``100``.

   :description:

     When outputting ppml we need to keep track of the space left on the
     current line and the current margin.  We store these values in a
     :class:`<ppml-printer>` object, along with the functions used to
     display text and line breaks.

     The *terse-depth* is used to limit recursion amongst
     :class:`<ppml-block>` instances. Once printing has recursed
     through *terse-depth* blocks, it will change the *break-type*
     to ``#"fit"`` to abbreviate things.

Type Aliases and Constants
==========================

.. type:: <nat>

   :equivalent: ``limited(<integer>, min: 0)``

.. type:: <ppml-break-type>

   :equivalent: ``one-of(#"consistent", #"inconsistent", #"fit")``

.. type:: <ppml-sequence>

   :equivalent: :drm:`<sequence>`

   :description:

     .. note:: This should be ``limited(<sequence>, of: <ppml>)``.

.. constant:: $line-break

   :value: ``ppml-break(space: 999)``

   :description:

     A way to force a line break by making a break with a space larger
     than the column width.
