Function Definition
-------------------

.. current-library:: dylan
.. current-module:: dylan

The :macro:`define function` definition macro provides a convenient way
to define functions that have no generic properties and hence are not
suitable for definition with :drm:`define generic <generic>` or
:drm:`define method <method>`. This extension has been accepted as part
of the language since the DRM was published.

The ``define function`` macro provides a way of defining a function that
says clearly to other programmers that the function is not part of any
generic operation; furthermore, the function will not be extended as a
generic function, and calling it need not involve any generic dispatch.
Without this macro, programmers who wanted to do so would have to turn
to ``define constant``. With ``define function``, programmer intent is
more explicit and it relays more information to future maintainers of a
piece of code.

The language definition of ``define function`` explicitly *does not*
specify what it expands into, so that Dylan implementations have
latitude to support this definer in the best way suited to the
implementation.

.. macro:: define function
   :defining:

   Defines a constant binding in the current module and initializes it to a
   new function.

   :macrocall:

     .. code-block:: dylan

       define {*adjective* }* function *name* *parameter-list*
         [ *body* ]
       end [ function ] [ *name* ]

   :parameter adjective: A Dylan unreserved-name *bnf*.
   :parameter name: A Dylan variable-name *bnf*.
   :parameter parameter-list: A Dylan parameter-list *bnf*.
   :parameter body: A Dylan body *bnf*.

   :description:

     Creates a constant module binding with the name *name*, and
     initializes it to a new function described by *parameter-list*,
     *options*, and any adjectives.

     The adjectives permitted depend on the implementation.

     The *parameter-list* describes the number and types of the
     function’s arguments and return values. It is an error to supply
     ``#next`` in the parameter list, and there is no implicit ``#next``
     parameter.

   :operations:

     The following functions return the same values as they would if the
     function had been defined as a bare method with the same signature:

     - :drm:`function-specializers`
     - :drm:`function-arguments`
     - :drm:`function-return-values`

     Calling some of the following reflective operations on a function
     defined with ``define function`` may be an error:

     - :drm:`generic-function-methods`
     - :drm:`add-method`
     - :drm:`generic-function-mandatory-keywords`
     - :drm:`sorted-applicable-methods`
     - :drm:`find-method`
     - :drm:`remove-method`
     - :drm:`applicable-method?`
