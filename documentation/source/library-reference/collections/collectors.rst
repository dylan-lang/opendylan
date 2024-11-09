*********************
The collectors Module
*********************

.. current-library:: collections
.. current-module:: collectors

.. macro:: collecting
   :statement:

   Collect values into a named or unnamed collector.  A collector may be, for example, a
   :drm:`<collection>`, a number into which values are accumulated, etc.

   :macrocall:
     .. parsed-literal:: 

        collecting ([`name`] [as `type`])
          [ `body` ]
        end [ collecting ]

   :parameter name: A Dylan variable-name *BNF*. If omitted, the collection is returned
      from the :macro:`collecting` macro call. If supplied, the caller is responsible for
      calling ``collected(name)`` to retrieve the collection before the call to
      `collecting` terminates.
   :parameter type: A Dylan type. The default value is :drm:`<list>`.
   :parameter body: A Dylan body *BNF*.

   :description:

      Binds *name* (or a default variable name if *name* is not supplied) to a collector
      that can efficiently collect new values into the collection when :macro:`collect`
      or the related ``collect-*`` macros are called.

   :example:

      .. code-block:: dylan

         collecting () collect(1); collect(2) end;
         // => #(1, 2)

         collecting () collect(1); collect-first(2) end;
         // => #(2, 1)

         collecting (as <integer>) collect(1); collect(2) end;
         // => 3

         collecting (a, b, c)
           collect-into(a, 1);
           collect-into(b, 2);
           collect-into(c, 3);
           values(collected(a), collected(b), collected(c))
         end;
         // => #(1), #(2), #(3)

.. macro:: collect

   :description: Collect a value at the end of the unnamed collector: ``collect(100)``
      May only be used when ``collecting () ... end`` was called with no arguments.

   :seealso: :macro:`collecting`

.. macro:: collect-first

   :description: Collect a value at the beginning of the unnamed collector:
      ``collect-first(100)`` May only be used when ``collecting () ... end`` was called
      with no arguments.

   :seealso: :macro:`collecting`

.. macro:: collect-last

   :description: Collect a value at the end of the unnamed collector:
      ``collect-last(100)`` May only be used when ``collecting () ... end`` was called
      with no arguments.

   :seealso: :macro:`collecting`

.. macro:: collect-into

   :description: Collect a value at the end of a named collector: ``collect-into(c,
      100)`` May only be used when ``collecting (c) ... end`` was called with arguments.

   :seealso: :macro:`collecting`

.. macro:: collect-first-into

   :description: Collect a value at the beginning of a named collector:
      ``collect-first-into(c, 100)`` May only be used when ``collecting (c) ... end`` was
      called with arguments.

   :seealso: :macro:`collecting`

.. macro:: collect-last-into

   :description: Collect a value at the end of a named collector: ``collect-last-into(c,
      100)`` May only be used when ``collecting (c) ... end`` was called with arguments.

   :seealso: :macro:`collecting`

.. macro:: collected

   :description: Retrieve the value of the collection associated with a collector.

   :example:

      .. code-block:: dylan

         collecting () ...; do(f, collected()); ... end

         collecting (a, b) ...; do(f1, collected(a)); do(f2, collected(b)); ... end

   :seealso: :macro:`collecting`

.. generic-function:: collector-protocol
   :open:

   :signature: collector-protocol *class* => *new-collector add-first add-last add-sequence-first add-sequence-last collection*

   :parameter class: An instance of :drm:`<object>`.
   :value new-collector: An instance of :drm:`<object>`.
   :value add-first: A :drm:`<function>` that accepts the collection and a value and adds
      the value to the beginning of the collection.
   :value add-last: A :drm:`<function>` that accepts the collection and a value and adds
      the value to the end of the collection.
   :value add-sequence-first: An instance of :drm:`<function>`. **Not yet implemented.**
   :value add-sequence-last: An instance of :drm:`<function>`. **Not yet implemented.**
   :value collection: A :drm:`<function>` that receives the collector and returns the
      collection.

   :seealso: :macro:`collecting`
