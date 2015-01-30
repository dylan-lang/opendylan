*****************
The plists Module
*****************

.. current-library:: collections
.. current-module:: plists

Overview
========

Property lists associate values with keys, but without the overhead of a
:drm:`<table>`.  These are generally useful for a small number of keys or
where memory usage is a concern.

This implementation allows plists to be stored in either a :drm:`<list>`
or a :drm:`<vector>`.  The keys are commonly called *indicators* and are
typically a :drm:`<symbol>`. The values can be any :drm:`<object>`.

In memory, the plist is arranged with keys and values alternating in
a single sequence:

.. code-block:: dylan

   #[key1:, 1, key2:, 2]

Conveniently, this is the same layout as a sequence of arguments when
using ``#rest`` in conjuction with keyword arguments and ``#all-keys``
as can be seen in the example using :macro:`with-keywords-removed`.

Reading
-------

- :gf:`get-property`

Modifying
---------

- :macro:`put-property!`
- :gf:`remove-keywords`
- :macro:`remove-property!`
- :macro:`with-keywords-removed`

Iterating
---------

- :gf:`keyword-sequence`
- :gf:`value-sequence`

Reference
=========

.. generic-function:: get-property

   Return the value for an indicator, with a default should it not exist.

   :signature: get-property *plist* *indicator* #key *default* => *property*

   :parameter plist: An instance of :drm:`<sequence>`.
   :parameter indicator: An instance of :drm:`<object>`.
   :parameter #key default: An instance of :drm:`<object>`.
   :value property: An instance of :drm:`<object>`.

.. generic-function:: keyword-sequence

   Returns a sequence containing the indicators in the *plist*.

   :signature: keyword-sequence *plist* => *keywords*

   :parameter plist: An instance of :drm:`<sequence>`.
   :value keywords: An instance of :drm:`<sequence>`.

   :seealso:

     - :gf:`value-sequence`

.. macro:: put-property!
   :statement:

   Modify the *plist*, adding *indicator* with the given *value*.

   :macrocall:
     .. code-block:: dylan

       put-property!(*plist*, *indicator*, *value*)

   :parameter plist: An instance of :drm:`<sequence>`.
   :parameter indicator: An instance of :drm:`<object>`.
   :parameter value: An instance of :drm:`<object>`.

   :example:

     .. code-block:: dylan

       put-property!(buffer-contents-properties(buffer),
                     #"optimization-colors", #f)

   :seealso:

     - :macro:`remove-property!`

.. generic-function:: remove-keywords

   Returns a copy of the *plist* with *keywords* removed.

   :signature: remove-keywords *plist* *keywords* => *plist*

   :parameter plist: An instance of :drm:`<sequence>`.
   :parameter keywords: An instance of :drm:`<sequence>`.
   :value plist: An instance of :drm:`<sequence>`.

   :seealso:

     - :macro:`remove-property!`
     - :macro:`with-keywords-removed`

.. macro:: remove-property!
   :statement:

   Modify the *plist*, removing *indicator*, returning the old value,
   if any.

   :macrocall:
     .. code-block:: dylan

       remove-property!(*plist*, *indicator*)

   :parameter plist: An instance of :drm:`<sequence>`.
   :parameter indicator: An instance of :drm:`<object>`.
   :value value: An instance of :drm:`<object>`.

   :example:

     .. code-block:: dylan

       remove-property!(buffer-properties(buffer), #"project");

   :seealso:

     - :macro:`put-property!`
     - :gf:`remove-keywords`
     - :macro:`with-keywords-removed`

.. generic-function:: value-sequence

   Returns a sequence containing the values in the *plist*.

   :signature: value-sequence *plist* => *values*

   :parameter plist: An instance of :drm:`<sequence>`.
   :value values: An instance of :drm:`<sequence>`.

   :seealso:

     - :gf:`keyword-sequence`

.. macro:: with-keywords-removed
   :statement:

   :macrocall:
     .. code-block:: dylan

       with-keywords-removed(*var* = *plist*, *keywords*)
         *body*
       end

   :parameter var: A Dylan name *bnf*.
   :parameter plist: An instance of :drm:`<sequence>`.
   :parameter keywords: An instance of :drm:`<sequence>`.
   :parameter body: A Dylan body *bnf*.

   :description:

     Executes the body, with the *keywords* removed from *plist* and
     the modified plist available as *var*.

   :example:
     .. code-block:: dylan

       define sealed method make
           (class == <interval-stream>, #rest initargs,
            #key buffer, interval, direction, #all-keys)
        => (stream :: <interval-stream>)
         ignore(direction);
         let (start-bp, end-bp)
           = values(interval-start-bp(buffer | interval),
                    interval-end-bp(buffer | interval));
         let buffer
           = buffer
             | select (interval by instance?)
                 <buffer>  => interval;
                 otherwise => bp-buffer(start-bp);
               end;
         with-keywords-removed (initargs = initargs, #[interval:])
           apply(next-method, class,
                 start-bp: start-bp, end-bp: end-bp,
                 buffer: buffer, initargs)
         end
       end method make;

   :seealso:

     - :gf:`remove-keywords`
     - :macro:`remove-property!`
     - :macro:`with-keywords-removed`
