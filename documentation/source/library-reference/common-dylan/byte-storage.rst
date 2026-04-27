***********************
The byte-storage Module
***********************

.. current-library:: common-dylan
.. current-module:: byte-storage

.. module:: byte-storage

.. generic-function:: byte-storage-address
   :open:

   Returns the address of the raw byte storage of an object.

   :signature: byte-storage-address (the-buffer) => (result-offset)

   :parameter the-buffer: An instance of :drm:`<object>`.
   :value result-offset: An instance of :class:`<machine-word>`.

   :seealso:

     - :meth:`byte-storage-address(<buffer>)`
     - :meth:`byte-storage-address(<byte-string>)`
     - :meth:`byte-storage-address(<byte-vector>)`
     - :gf:`byte-storage-offset-address`

.. method:: byte-storage-address
   :specializer: <byte-string>
   :sealed:
   :no-contents-entry:

   Returns the address of the raw byte storage of a :drm:`<byte-string>`.

   :seealso:

     - :gf:`byte-storage-address`

.. method:: byte-storage-address
   :specializer: <byte-vector>
   :sealed:
   :no-contents-entry:

   Returns the address of the raw byte storage of a :class:`<byte-vector>`.

   :seealso:

     - :gf:`byte-storage-address`

.. macro:: with-object-byte-storage
   :statement:

   Provides the pinned address of the repeated byte storage of an
   object within the scope of the body of the code.

   :macrocall:
     .. parsed-literal::

        with-object-byte-storage (*name*  = *object*)
          *body*
        end [with-object-byte-storage]

   :parameter name: A Dylan variable name.
   :parameter object: A Dylan object with repeated byte storage.

   :description:

     Binds *name* to the address (a :class:`<machine-word>` as
     returned by :gf:`byte-storage-address`) of the repeated byte
     storage portion of an object within the scope of *body*.
     The object is *pinned* by the garbage collector so that the
     address is valid until *body* exits.
