***********************
The Common Dylan Module
***********************

.. current-library:: common-dylan
.. current-module:: common-dylan

The *common-dylan* module contains a variety of useful basic
extensions to the Dylan language.

The Common Dylan extensions are:

- Collection model extensions: :class:`<stretchy-sequence>`,
  :class:`<string-table>`, :gf:`difference`, :func:`fill-table!`,
  :gf:`find-element`, :gf:`position`, :gf:`remove-all-keys!`, and
  :macro:`define table`.
- Condition system extensions: :class:`<format-string-condition>`,
  :class:`<simple-condition>`, and :gf:`condition-to-string`.
- Program constructs: :macro:`iterate` and :macro:`when`.
- Application development conveniences: :macro:`iterate`,
  :func:`debug-message`, :func:`ignore`, :func:`ignorable`,
  :macro:`timing`, :func:`$unsupplied`, :func:`unsupplied?`,
  :func:`unsupplied`, :macro:`when`, :const:`$unfound`, :func:`one-of`,
  :func:`unfound?`, and :func:`found?`.
- Type conversion functions: :func:`integer-to-string`,
  :func:`string-to-integer`, and :func:`float-to-string`.

.. macro:: assert
   :statement:

   Signals an error if the expression passed to it evaluates to false.

   :macrocall:

     .. code-block:: dylan

       assert *expression* *format-string* [*format-arg* ]* => *false*

     .. code-block:: dylan

       assert *expression* => *false*

   :parameter expression: A Dylan expression *bnf*.
   :parameter format-string: A Dylan expression *bnf*.
   :parameter format-arg: A Dylan expression *bnf*.

   :value false: ``#f``.

   :description:

     Signals an error if *expression* evaluates to ``#f``.

     An assertion or “assert” is a simple tool for testing that
     conditions hold in program code.

     The *format-string* is a format string as defined on page 112 of
     the DRM. If *format-string* is supplied, the error is formatted
     accordingly, along with any instances of *format-arg*.

     If *expression* is not ``#f``, ``assert`` does not evaluate
     *format-string* or any instances of *format-arg*.

   See also

   - :macro:`debug-assert`

.. class:: <byte-character>
   :sealed:

   The class of 8-bit characters that instances of ``<byte-string>`` can
   contain.

   :superclasses: <character>

   :description:

     The class of 8-bit characters that instances of ``<byte-string>``
     can contain.

.. generic-function:: concatenate!
   :open:

   A destructive version of the Dylan language’s :drm:`concatenate`;
   that is, one that might modify its first argument.

   :signature: concatenate! *sequence* #rest *more-sequences* => *result-sequence*

   :parameter sequence: An instance of ``<sequence>``.
   :parameter #rest more-sequences: Instances of ``<sequence>``.
   :value result-sequence: An instance of ``<sequence>``.

   :description:

     A destructive version of the Dylan language’s :drm:`concatenate`;
     that is, one that might modify its first argument.

     It returns the concatenation of one or more sequences, in a
     sequence that may or may not be freshly allocated. If
     *result-sequence* is freshly allocated, then, as for
     :drm:`concatenate`, it is of the type returned by
     :drm:`type-for-copy` of *sequence*.

   :example:

     .. code-block:: dylan

       > define variable *x* = "great-";
       "great-"
       > define variable *y* = "abs";
       "abs"
       > concatenate! (*x*, *y*);
       "great-abs"
       > *x*;
       "great-abs"
       >

.. generic-function:: condition-to-string
   :open:

   Returns a string representation of a condition object.

   :signature: condition-to-string *condition* => *string*

   :parameter condition: An instance of ``<condition>``.
   :value string: An instance of ``<string>``.

   :description:

     Returns a string representation of a general instance of
     ``<condition>``. There is a method on
     :class:`<format-string-condition>` and method on
     :drm:`<type-error>`.

.. macro:: debug-assert
   :statement:

   Signals an error if the expression passed to it evaluates to false —
   but only when the code is compiled in interactive development mode.

   :macrocall:
     .. code-block:: dylan

       debug-assert *expression* *format-string* [ *format-arg* ]* => *false*

     .. code-block:: dylan

       debug-assert *expression* => *false*

   :parameter expression: A Dylan expression *bnf*.
   :parameter format-string: A Dylan expression *bnf*.
   :parameter format-arg: A Dylan expression *bnf*.
   :value false: ``#f``.

   :description:

     Signals an error if *expression* evaluates to false — but only when
     the code is compiled in debugging mode.

     An assertion or “assert” is a simple and popular development tool
     for testing conditions in program code.

     This macro is identical to *assert*, except that the assert is
     defined to take place only while debugging.

     The Open Dylan compiler removes debug-assertions when it compiles
     code in “production” mode as opposed to “debugging” mode.

     The *format-string* is a format string as defined on page 112 of
     the DRM.

.. function:: debug-message

   Formats a string and outputs it to the debugger.

   :signature: debug-message *format-string* #rest *format-args* => ()

   :parameter format-string:An instance of ``<string>``.
   :parameter #rest format-args: Instances of ``<object>``.

   :description:

     Formats a string and outputs it to the debugger.

     The *format-string* is a format string as defined on page 112 of
     the DRM.

.. method:: default-handler
   :specializer: <warning>

   Prints the message of a warning instance to the Open Dylan debugger
   window’s messages pane.

   :signature: default-handler *warning* => *false*

   :parameter warning: An instance of :drm:`<warning>`.
   :value false: ``#f``.

   :description:

     Prints the message of a warning instance to the Open Dylan debugger
     window’s messages pane. It uses :func:`debug-message`, to do so.

     This method is a required, predefined method in the Dylan language,
     described on page 361 of the DRM as printing the warning’s message
     in an implementation-defined way. We document this method here
     because our implementation of it uses the function
     :func:`debug-message`, which is defined in the *common-dylan*
     library. Thus to use this :drm:`default-handler` method on
     ``<warning>``, your library needs to use the *common-dylan* library
     or a library that uses it, rather than simply using the Dylan
     library.

   :example:

     In the following code, the signalled messages appear in the Harlequin
     Dylan debugger window.

     .. code-block:: dylan

       define class <my-warning> (<warning>)
       end class;

       define method say-hello()
         format-out("hello there!\\n");
         signal("help!");
         signal(make(<my-warning>));
         format-out("goodbye\\n");
       end method say-hello;

       say-hello();

     The following messages appear in the debugger messages pane::

       Application Dylan message: Warning: help!
       Application Dylan message: Warning: {<my-warning>}

     Where ``{<my-warning>}`` means an instance of ``<my-warning>``.

   See also

   - :func:`debug-message`.
   - :drm:`default-handler`, page 361 of the DRM.

.. function:: default-last-handler

   Formats and outputs a Dylan condition using *format-out* and passes
   control on to the next handler.

   :signature: default-last-handler *serious-condition* *next-handler* => ()

   :parameter serious-condition: A object of class ``<serious-condition>``.
   :parameter next-handler: A function.

   :description:

     A handler utility function defined on objects of class
     ``<serious-condition>`` that can be by bound dynamically around a
     computation via :drm:`let handler <handler>` or installed globally
     via :macro:`define last-handler`.

     This function formats and outputs the Dylan condition
     *serious-condition* using *format-out* from the Format-Out library,
     and passes control on to the next handler.

     This function is automatically installed as the last handler if
     your library uses the Common Dylan library.

   :example:

     The following form defines a dynamic handler around some body:

     .. code-block:: dylan

       let handler <serious-condition> = default-last-handler;

     while the following form installs a globally visible last-handler:

     .. code-block:: dylan

       define last-handler <serious-condition>
         = default-last-handler;

   See also

   - :macro:`define last-handler`
   - *win32-last-handler* in the *C FFI and Win32* library reference, under
     library *win32-user* and module *win32-default-handler*.

.. macro:: define last-handler
   :defining:

   Defines a “last-handler” to be used after any dynamic handlers and
   before calling :drm:`default-handler`.

   :macrocall:
     .. code-block:: dylan

       define last-handler (*condition*, #key *test*, *init-args*)
         = *handler* ;

       define last-handler condition = handler;

       define last-handler;

   :parameter condition: A Dylan expression *bnf*. The class of
     condition for which the handler should be invoked.
   :parameter test: A Dylan expression *bnf*. A function of one argument
     called on the condition to test applicability of the handler.
   :parameter init-args: A Dylan expression *bnf*. A sequence of
     initialization arguments used to make an instance of the handler’s
     condition class.
   :parameter handler: A Dylan expression *bnf*. A function of two
     arguments,
   :parameter condition: and *next-handler*, that is called on a
     condition which matches the handler’s condition class and test
     function.

   :description:

     A last-handler is a global form of the dynamic handler introduced
     via :drm:`let handler <handler>`, and is defined using an identical
     syntax. The last handler is treated as a globally visible dynamic
     handler. During signalling if a last-handler has been installed
     then it is the last handler tested for applicability before
     :drm:`default-handler` is invoked. If a last-handler has been
     installed then it is also the last handler iterated over in a call
     to :drm:`do-handlers`.

     The first two defining forms are equivalent to the two alternate
     forms of let handler. If more than one of these first defining
     forms is executed then the last one executed determines the
     installed handler. The current last-handler can be uninstalled by
     using the degenerate third case of the defining form, that has no
     condition description or handler function.

     The intention is that libraries will install last handlers to
     provide basic runtime error handling, taking recovery actions such
     as quitting the application, trying to abort the current
     application operation, or entering a connected debugger.

   :example:

     The following form defines a last-handler function called
     *default-last-handler* that is invoked on conditions of class
     ``<serious-condition>``:

     .. code-block:: dylan

       define last-handler <serious-condition>
         = default-last-handler;

   See also

   - *win32-last-handler* in the *C FFI and Win32* library reference,
     under library *win32-user* and module *win32-default-handler*.

.. macro:: define table
   :defining:

   Defines a constant binding in the current module and initializes it
   to a new table object.

   :macrocall:
     .. code-block:: dylan

       define table *name* [ :: *type* ] = { [ *key* => *element* ]* }

   :parameter name: A Dylan name *bnf*.
   :parameter type: A Dylan operand *bnf*. Default value: ``<table>``.
   :parameter key: A Dylan expression *bnf*.
   :parameter element: A Dylan expression *bnf*.

   :description:

     Defines a constant binding *name* in the current module, and
     initializes it to a new table object, filled in with the keys and
     elements specified.

     If the argument *type* is supplied, the new table created is an
     instance of that type. Therefore *type* must be ``<table>`` or a
     subclass thereof. If *type* is not supplied, the new table created
     is an instance of a concrete subclass of ``<table>``.

   :example:

     .. code-block:: dylan

       define table $colors :: <object-table>
         = { #"red" => $red,
             #"green" => $green,
             #"blue" => $blue };

.. generic-function:: difference
   :open:

   Returns a sequence containing the elements of one sequence that are
   not members of a second.

   :signature: difference *sequence-1* *sequence-2* #key *test* => *result-sequence*

   :parameter sequence-1: An instance of ``<sequence>``.
   :parameter sequence-2: An instance of ``<sequence>``.
   :parameter test: An instance of ``<function>``. Default value: ``\==``.
   :value result-sequence: An instance of ``<sequence>``.

   :description:

     Returns a sequence containing the elements of *sequence-1* that are
     not members of *sequence-2*. You can supply a membership test
     function as *test*.

   :example:

     .. code-block:: dylan

       > difference(#(1,2,3), #(2,3,4));
       #(1)
       >

.. function:: false-or

   Returns a union type comprised of ``singleton(#f)`` and one or more types.

   :signature: false-or *type* #rest *more-types* => *result-type*

   :parameter type: An instance of ``<type>``.
   :parameter #rest more-types: Instances of ``<type>``.
   :value result-type: An instance of ``<type>``.

   :description:

     Returns a union type comprised of ``singleton(#f)``, *type*, any
     other types passed as *more-types*.

     This function is useful for specifying slot types and function
     return values.

     The expression

     .. code-block:: dylan

       false-or(*t-1*, *t-2*, ..)

     is type-equivalent to

     .. code-block:: dylan

       type-union(singleton(#f), *t-1*, *t-2*, ..)

.. function:: fill-table!

   Fills a table with the keys and elements supplied.

   :signature: fill-table! *table* *keys-and-elements* => *table*

   :parameter table: An instance of ``<table>``.
   :parameter keys-and-elements: An instance of ``<sequence>``.
   :value table: An instance of ``<table>``.

   :description:

     Modifies table so that it contains the keys and elements supplied
     in the sequence *keys-and-elements*.

     This function interprets *keys-and-elements* as key-element pairs,
     that is, it treats the first element as a table key, the second as
     the table element corresponding to that key, and so on. The keys
     and elements should be suitable for *table*.

     Because *keys-and-elements* is treated as a sequence of paired
     key-element values, it should contain an even number of elements;
     if it contains an odd number of elements, *fill-table!* ignores the
     last element (which would have been treated as a key).

.. generic-function:: find-element
   :open:

   Returns an element from a collection such that the element satisfies
   a predicate.

   :signature: find-element *collection* *function* #key *skip* *failure* => *element*

   :parameter collection: An instance of ``<collection>``.
   :parameter predicate: An instance of ``<function>``.
   :parameter #key skip: An instance of ``<integer>``. Default value: 0.
   :parameter #key failure: An instance of ``<object>``. Default value: ``#f``.
   :value element: An instance of ``<object>``.

   :description:

     Returns a collection element that satisfies *predicate*.

     This function is identical to Dylan’s :drm:`find-key`, but it
     returns the element that satisfies *predicate* rather than the key
     that corresponds to the element.

.. function:: float-to-string

   Formats a floating-point number to a string.

   :signature: float-to-string *float* => *string*

   :parameter float: An instance of ``<float>``.
   :value string: An instance of ``<string>``.

   :description:

     Formats a floating-point number to a string. It uses scientific
     notation where necessary.

.. class:: <format-string-condition>
   :sealed:
   :instantiable:

   The class of conditions that take a format string.

   :superclasses: <condition>

   :description:

     The class of conditions that take a format string, as defined by
     the DRM.

     It is the superclass of Dylan’s :class:`<simple-condition>`.

   See also

   The Format library.

.. function:: format-to-string

   Returns a formatted string constructed from its arguments.

   :signature: format-to-string *format-string* #rest *format-arguments* => *string*

   :parameter format-string: An instance of ``<byte-string>``.
   :parameter #rest format-arguments: Instances of ``<object>``.
   :value result-string: An instance of ``<byte-string>``.

   :conditions:

     This function signals an error if any of the format directives in
     *format-string* are invalid.

   :description:

     Returns a formatted string constructed from its arguments, which
     include a *format-string* of formatting directives and a series of
     *format-arguments* to be formatted according to those directives.

     The *format-string* must be a Dylan format string as described on
     :drm:`pages 112–114 of the DRM <Condition_Messages>`.

.. function:: found?

   Returns true if *object* is not equal to :const:`$unfound`, and false otherwise.

   :signature: found? *object* => *boolean*

   :parameter object: An instance of ``<object>``.
   :value boolean: An instance of ``<boolean>``.

   :description:

     Returns true if *object* is not equal to :const:`$unfound`, and false otherwise.

     It uses ``\=`` as the equivalence predicate.

.. function:: ignore

   A compiler directive that tells the compiler it must not issue a
   warning if its argument is bound but not referenced.

   :signature: ignore *variable* => ()

   :parameter variable: A Dylan variable-name *bnf*.

   :description:

     When the compiler encounters a variable that is bound but not
     referenced, it normally issues a warning. The ``ignore`` function
     is a compiler directive that tells the compiler it *must not* issue
     this warning if *variable* is bound but not referenced. The
     ``ignore`` function has no run-time cost.

     The ``ignore`` function is useful for ignoring arguments passed to,
     or values returned by, a function, method, or macro. The function
     has the same extent as a :drm:`let`; that is, it applies to the
     smallest enclosing implicit body.

     Use ``ignore`` if you never intend to reference *variable* within
     the extent of the ``ignore``. The compiler will issue a warning to
     tell you if your program violates the ``ignore``. If you are not
     concerned about the ``ignore`` being violated, and do not wish to
     be warned if violation occurs, use :func:`ignorable` instead.

   :example:

     This function ignores some of its arguments:

     .. code-block:: dylan

       define method foo (x ::<integer>, #rest args)
         ignore(args);
         ...
       end

     Here, we use *ignore* to ignore one of the values returned by *fn*:

     .. code-block:: dylan

       let (x,y,z) = fn();
       ignore(y);

   See also

   - :func:`ignorable`

.. function:: ignorable

   A compiler directive that tells the compiler it *need not* issue a
   warning if its argument is bound but not referenced.

   :signature: ignorable *variable* => ()

   :parameter variable: A Dylan variable-name *bnf*.

   :description:

     When the compiler encounters a variable that is bound but not
     referenced, it normally issues a warning. The ``ignorable``
     function is a compiler directive that tells the compiler it *need
     not* issue this warning if *variable* is bound but not referenced.
     The ``ignorable`` function has no run-time cost.

     The ``ignorable`` function is useful for ignoring arguments passed
     to, or values returned by, a function, method, or macro. The
     function has the same extent as a :drm:`let`; that is, it applies
     to the smallest enclosing implicit body.

     The ``ignorable`` function is similar to :func:`ignore`. However,
     unlike :func:`ignore`, it does not issue a warning if you
     subsequently reference *variable* within the extent of the
     ``ignorable`` declaration. You might prefer ``ignorable`` to
     :func:`ignore` if you are not concerned about such violations and
     do not wish to be warned about them.

   :example:

     This function ignores some of its arguments:

     .. code-block:: dylan

       define method foo (x ::<integer>, #rest args)
         ignorable(args);
         ...
       end

     Here, we use ``ignorable`` to ignore one of the values returned by
     *fn*:

     .. code-block:: dylan

       let (x,y,z) = fn();
       ignorable(y);

   See also

   - :func:`ignore`

.. function:: integer-to-string

   Returns a string representation of an integer.

   :signature: integer-to-string *integer* #key *base* *size* *fill* => *string*

   :parameter integer: An instance of ``<integer>``.
   :parameter base: An instance of ``<integer>``. Default value: 10.
   :parameter size: An instance of ``<integer>`` or ``#f``. Default value: ``#f``.
   :parameter fill: An instance of ``<character>``. Default value: 0.
   :value string: An instance of ``<byte-string>``.

   :description:

     Returns a string representation of *integer* in the given *base*,
     which must be between 2 and 36. The size of the string is
     right-aligned to *size* if *size* is not ``#f``, and it is filled
     with the *fill* character. If the string is already larger than
     *size* then it is not truncated.

.. macro:: iterate
   :statement:

   Iterates over a body.

   :macrocall:
     .. code-block:: dylan

       iterate *name* ({*argument* [ = *init-value* ]}*)
         [ *body* ]
       end [ iterate ]

   :parameter name: A Dylan variable-name *bnf*.
   :parameter argument: A Dylan variable-name *bnf*.
   :parameter init-value: A Dylan expression *bnf*.
   :parameter body: A Dylan body *bnf*.
   :value value: Zero or more instances of ``<object>``.

   :description:

     Defines a function that can be used to iterate over a body. It is
     similar to *for*, but allows you to control when iteration will
     occur.

     It creates a function called *name* which will perform a single
     step of the iteration at a time; *body* can call *name* whenever it
     wants to iterate another step. The form evaluates by calling the
     new function with the initial values specified.

.. function:: one-of

   Returns a union type comprised of singletons formed from its arguments.

   :signature: one-of *object* #rest *more-objects* => *type*

   :parameter object: An instance of ``<object>``.
   :parameter #rest more-objects: Instances of ``<object>``.
   :value type: An instance of ``<type>``.

   :description:

     Returns a union type comprised of ``singleton(object)`` and the
     singletons of any other objects passed with *more-object*.

     .. code-block:: dylan

       one-of(x, y, z)

     Is a type expression that is equivalent to

     .. code-block:: dylan

       type-union(singleton(x), singleton(y), singleton(z))

.. generic-function:: position
   :open:

   Returns the key at which a particular value occurs in a sequence.

   :signature: position *sequence* *target* #key *test* *start* *end* *skip* *count* => *position*

   :parameter sequence: An instance of ``<sequence>``.
   :parameter target: An instance of ``<object>``.
   :parameter #key test: An instance of ``<function>``. Default value: ``\==``.
   :parameter #key start: An instance of ``<integer>``. Default value: 0.
   :parameter #key end: An instance of ``<object>``. Default value: ``#f``.
   :parameter #key skip: An instance of ``<integer>``. Default value: 0.
   :parameter #key count: An instance of ``<object>``. Default value: ``#f``.
   :value position: An instance of ``false-or(<integer>)``.

   :description:

     Returns the position at which *target* occurs in *sequence*.

     If *test* is supplied, *position* uses it as an equivalence
     predicate for comparing *sequence* ’s elements to *target*. It should
     take two objects and return a boolean. The default predicate used is
     ``\==``.

     The *skip* argument is interpreted as it is by Dylan’s :drm:`find-key`
     function: *position* ignores the first *skip* elements that match
     *target*, and if *skip* or fewer elements satisfy *test*, it
     returns ``#f``.

     The *start* and *end* arguments indicate, if supplied, which subrange
     of the *sequence* is used for the search.

.. generic-function:: remove-all-keys!
   :open:

   Removes all keys in a mutable collection, leaving it empty.

   :signature: remove-all-keys! *mutable-collection* => ()

   :parameter mutable-collection: An instance of ``<mutable-collection>``.

   :description:

     Modifies *mutable-collection* by removing all its keys and leaving it
     empty. There is a predefined method on ``<table>``.

.. class:: <simple-condition>
   :sealed:
   :instantiable:

   The class of simple conditions.

   :superclasses: <format-string-condition>

   :description:

     The class of simple conditions. It is the superclass of ``<simple-error>``,
     ``<simple-warning>``, and ``<simple-restart>``.

   :operations:

     - :drm:`condition-format-string`
     - :drm:`condition-format-args`

.. class:: <stretchy-sequence>
   :open:
   :abstract:

   The class of stretchy sequences.

   :superclasses: <sequence> <stretchy-collection>

   :description:

     The class of stretchy sequences.

.. class:: <string-table>
   :sealed:
   :instantiable:

   The class of tables that use strings for keys.

   :superclasses: <table>

   :description:

     The class of tables that use instances of ``<string>`` for their
     keys. It is an error to use a key that is not an instance of
     ``<string>``.

     Keys are compared with the equivalence predicate ``\=``.

     The elements of the table are instances of ``<object>``.

     It is an error to modify a key once it has been used to add an element
     to a ``<string-table>``. The effects of modification are not defined.

     .. note:: This class is also exported from the *table-extensions* module
        of the *table-extensions* library.

.. function:: string-to-integer

   Returns the integer represented by its string argument, or by a
   substring of that argument, in a number base between 2 and 36.

   :signature: string-to-integer *string* #key *base* *start* *end* *default* => *integer* *next-key*

   :parameter string: An instance of ``<byte-string>``.
   :parameter #key base: An instance of ``<integer>``. Default value: 10.
   :parameter #key start: An instance of ``<integer>``. Default value: 0.
   :parameter #key end: An instance of ``<integer>``. Default value: ``sizeof(*string*)``.
   :parameter #key default: An instance of ``<integer>``. Default value: :const:`$unsupplied`.
   :value integer: An instance of ``<integer>``.
   :value next-key: An instance of ``<integer>``.

   :description:

     Returns the integer represented by the characters of *string* in
     the number base *base*, where *base* is between 2 and 36. You can
     constrain the search to a substring of *string* by giving values
     for *start* and *end*.

     This function returns the next key beyond the last character it
     examines.

     If there is no integer contained in the specified region of the
     string, this function returns *default*, if specified. If you do
     not give a value for *default*, this function signals an error.

     This function is similar to C’s ``strtod`` function.

.. function:: subclass

   Returns a type representing a class and its subclasses.

   :signature: subclass *class* => *subclass-type*

   :parameter class: An instance of ``<class>``.
   :value subclass-type: An instance of ``<type>``.

   :description:

     Returns a type that describes all the objects representing
     subclasses of the given class. We term such a type a *subclass
     type*.

     The ``subclass`` function is allowed to return an existing type if
     that type is type equivalent to the subclass type requested.

     Without ``subclass``, methods on generic functions (such as Dylan’s
     standard :drm:`make` and :drm:`as`) that take types as arguments
     are impossible to reuse without resorting to ad hoc techniques. In
     the language defined by the DRM, the only mechanism available for
     specializing such methods is to use singleton types. A singleton
     type specializer used in this way, by definition, gives a method
     applicable to exactly one type. In particular, such methods are not
     applicable to subtypes of the type in question. In order to define
     reusable methods on generic functions like this, we need a type
     which allows us to express applicability to a type and all its
     subtypes.

     For an object *O* and class *Y*, the following :drm:`instance?`
     relationship applies:

     **INSTANCE-1**: ``instance?(*O*, subclass(*Y*))``
       True if and only if *O* is a class and *O* is a subclass of *Y*.

     For classes *X* and *Y* the following :drm:`subtype?` relationships hold
     (note that a rule applies only when no preceding rule matches):

     **SUBTYPE-1**: ``subtype?(subclass(*X*), subclass(*Y*))``
       True if and only if *X* is a subclass of *Y*.

     **SUBTYPE-2**: ``subtype?(singleton(*X*), subclass(*Y*))``
       True if and only if *X* is a class and *X* is a subclass of *Y*.

     **SUBTYPE-3**: ``subtype?(subclass(*X*), singleton(*Y*))``
       Always false.

     **SUBTYPE-4**: ``subtype?(subclass(*X*), *Y*)``
       where *Y* is not a subclass type. True if *Y* is ``<class>`` or
       any proper superclass of ``<class>`` (including ``<object>``, any
       implementation-defined supertypes, and unions involving any of
       these). There may be other implementation-defined combinations of
       types *X* and *Y* for which this is also true.

     **SUBTYPE-5**: ``subtype?(*X*, subclass(*Y*))``
       where *X* is not a subclass type. True if *Y* is ``<object>`` or any
       proper supertype of ``<object>`` and *X* is a subclass of ``<class>``.

     Note that by subclass relationships *SUBTYPE-4* and *SUBTYPE-5*, we get
     this correspondence: ``<class>`` and ``subclass(<object>)`` are type
     equivalent.

     Where the :drm:`subtype?` test has not been sufficient to determine an
     ordering for a method’s argument position, the following further
     method-ordering rules apply to cases involving subclass types (note that
     a rule applies only when no preceding rule matches):

     - **SPECIFICITY+1**. ``subclass(*X*)`` precedes ``subclass(*Y*)``
       when the argument is a class *C* and *X* precedes *Y* in the
       class precedence list of *C*.

     - **SPECIFICITY+2**. ``subclass(*X*)`` always precedes *Y*, *Y* not
       a subclass type. That is, applicable subclass types precede any
       other applicable class-describing specializer.

     The constraints implied by sealing come by direct application of sealing
     rules 1–3 (see page 136 of the DRM) and the following disjointness
     criteria for subclass types (note that a rule applies only when no
     preceding rule matches):

     - **DISJOINTNESS+1**. A subclass type ``subclass(*X*)`` and a
       type *Y* are disjoint if *Y* is disjoint from ``<class>``, or if
       *Y* is a subclass of ``<class>`` without instance classes that
       are also subclasses of *X*.

     - **DISJOINTNESS+2**. Two subclass types ``subclass(*X*)`` and
       ``subclass(*Y*)`` are disjoint if the classes *X* and *Y* are
       disjoint.

     - **DISJOINTNESS+3**. A subclass type ``subclass(*X*)`` and a
       singleton type ``singleton(*O*)`` are disjoint unless *O* is a
       class and *O* is a subclass of *X*.

     The guiding principle behind the semantics is that, as far as possible,
     methods on classes called with an instance should behave isomorphically
     to corresponding methods on corresponding subclass types called with the
     class of that instance. So, for example, given the heterarchy::

       <object>
         \|
         <A>
         / \\
       <B> <C>
        \\ /
         <D>

     and methods:

     .. code-block:: dylan

       method foo (<A>)
       method foo (<B>)
       method foo (<C>)
       method foo (<D>)

       method foo-using-type (subclass(<A>))
       method foo-using-type (subclass(<B>))
       method foo-using-type (subclass(<C>))
       method foo-using-type (subclass(<D>))

     that for a direct instance *D1* of ``<D>``:

     .. code-block:: dylan

       foo-using-type(<D>)

     should behave analogously to:

     .. code-block:: dylan

       foo(D1)

     with respect to method selection.

   :example:

     .. code-block:: dylan

       define class <A> (<object>) end;
       define class <B> (<A>) end;
       define class <C> (<A>) end;
       define class <D> (<B>, <C>) end;

       define method make (class :: subclass(<A>), #key)
         print("Making an <A>");
         next-method();
       end method;

       define method make (class :: subclass(<B>), #key)
         print("Making a <B>");
         next-method();
       end method;

       define method make (class :: subclass(<C>), #key)
         print("Making a <C>");
         next-method();
       end method;

       define method make (class :: subclass(<D>), #key)
         print("Making a <D>");
         next-method();
       end method;

     ::

       ? make(<D>);
       Making a <D>
       Making a <B>
       Making a <C>
       Making an <A>
       {instance of <D>}

.. function:: supplied?

   Returns true if its argument is not equal to the unique “unsupplied”
   value, :const:`$unsupplied`, and false if it is.

   :signature: supplied? *object* => *supplied?*

   :parameter object: An instance of ``<object>``.
   :value supplied?: An instance of ``<boolean>``.

   :description:

     Returns true if *object* is not equal to the unique “unsupplied”
     value, :const:`$unsupplied`, and false if it is. It uses ``\=`` as
     the equivalence predicate.

   See also

   - :const:`$unsupplied`
   - :func:`unsupplied`
   - :func:`unsupplied?`

.. macro:: timing
   :statement:

   Returns the time, in seconds and microseconds, spent executing the body
   of code it is wrapped around.

   :macrocall:
     .. code-block:: dylan

       timing () [ *body* ] end [ timing ]

   :parameter body: A Dylan body *bnf*
   :value seconds: An instance of ``<integer>``.
   :value microseconds: An instance of ``<integer>``.

   :description:

     Returns the time, in seconds and microseconds, spent executing the
     body of code it is wrapped around.

     The first value returned is the number of whole seconds spent in
     *body*. The second value returned is the number of microseconds
     spent in *body* in addition to *seconds*.

   :example:

     .. code-block:: dylan

       timing ()
         for (i from 0 to 200)
           format-to-string("%d %d", i, i + 1)
         end
       end;
       => 1 671000

.. constant:: $unfound

   A unique value that can be used to indicate that a search operation
   failed.

   :type: <list>
   :value: A unique value.

   :description:

     A unique value that can be used to indicate that a search operation
     failed.

  See also

  - :func:`found?`
  - :func:`unfound?`
  - :func:`unfound`

.. function:: unfound

   Returns the unique “unfound” value, :const:`$unfound`.

   :signature: unfound () => *unfound-marker*

   :value unfound-marker: The value :const:`$unfound`.

   :description:

   Returns the unique “unfound” value, :const:`$unfound`.

   See also

   - :func:`found?`
   - :func:`unfound?`
   - :const:`$unfound`

.. function:: unfound?

   Returns true if its argument is equal to the unique “unfound” value,
   :const:`$unfound`, and false if it is not.

   :signature: unfound? *object* => *unfound?*

   :parameter object: An instance of ``<object>``.
   :value unfound?: An instance of ``<boolean>``.

   :description:

     Returns true if *object* is equal to the unique “unfound” value,
     :const:`$unfound`, and false if it is not. It uses ``\=``
     as the equivalence predicate.

   See also

   - :func:`found?`
   - :const:`$unfound`
   - :func:`unfound`

.. constant:: $unsupplied

   A unique value that can be used to indicate that a keyword was not
   supplied.

   :type: <list>
   :value: A unique value.

   :description:

     A unique value that can be used to indicate that a keyword was not
     supplied.

   See also

   - :func:`supplied?`
   - :func:`unsupplied`
   - :func:`unsupplied?`

.. function:: unsupplied

   Returns the unique “unsupplied” value, :const:`$unsupplied`.

   :signature: unsupplied () => *unsupplied-marker*

   :value unsupplied-marker: The value :const:`$unsupplied`.

   :description:

     Returns the unique “unsupplied” value, :const:`$unsupplied`.

   See also

   - :func:`supplied?`
   - :const:`$unsupplied`
   - :func:`unsupplied?`

.. function:: unsupplied?

   Returns true if its argument is equal to the unique “unsupplied”
   value, :const:`$unsupplied`, and false if it is not.

   :signature: unsupplied? *value* => *boolean*

   :parameter value: An instance of ``<object>``.
   :value boolean: An instance of ``<boolean>``.

   :description:

     Returns true if its argument is equal to the unique “unsupplied”
     value, :const:`$unsupplied`, and false if it is not. It uses ``\=``
     as the equivalence predicate.

   See also

   - :func:`supplied?`
   - :const:`$unsupplied`
   - :func:`unsupplied`

.. macro:: when
   :statement:

   Executes an implicit body if a test expression is true, and does
   nothing if the test is false.

   :macrocall:
     .. code-block:: dylan

       when (*test*) [ *consequent* ] end [ when ]

   :parameter test: A Dylan expression *bnf*.
   :parameter consequent: A Dylan body *bnf*.
   :value value: Zero or more instances of ``<object>``.

   :description:

     Executes *consequent* if *test* is true, and does nothing if *test*
     is false.

     This macro behaves identically to Dylan’s standard :drm:`if`
     statement macro, except that there is no alternative flow of
     execution when the test is false.

   :example:

     .. code-block:: dylan

       when (x < 0)
         ~ x;
       end;
