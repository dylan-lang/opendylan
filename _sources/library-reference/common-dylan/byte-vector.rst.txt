**********************
The byte-vector Module
**********************

.. current-library:: common-dylan
.. current-module:: byte-vector

.. module:: byte-vector

.. note:: Many of the names exported from the ``byte-vector`` module are imported from
          :mod:`dylan-extensions` and re-exported.

.. type:: <byte-vector>

   :equivalent: ``limited(<vector>, of: <byte>)``

   :seealso:

     - :type:`<byte>`

.. type:: <byte>

   :equivalent: ``limited(<integer>, min: 0, max: 255)``

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

.. generic-function:: byte-storage-offset-address
   :open:

   Returns the address of the raw byte storage of an object, with an offset.

   :signature: byte-storage-offset-address (the-buffer data-offset) => (result-offset)

   :parameter the-buffer: An instance of :drm:`<object>`.
   :parameter data-offset: An instance of :drm:`<integer>`.
   :value result-offset: An instance of :class:`<machine-word>`.

   :seealso:

     - :meth:`byte-storage-offset-address(<buffer>)`
     - :meth:`byte-storage-offset-address(<byte-string>)`
     - :meth:`byte-storage-offset-address(<byte-vector>)`
     - :gf:`byte-storage-address`

.. method:: byte-storage-offset-address
   :specializer: <byte-string>
   :sealed:
   :no-contents-entry:

   Returns the address of the raw byte storage of a :drm:`<byte-string>`, with an offset.

   :seealso:

     - :gf:`byte-storage-offset-address`

.. method:: byte-storage-offset-address
   :specializer: <byte-vector>
   :sealed:
   :no-contents-entry:

   Returns the address of the raw byte storage of a :class:`<byte-vector>`, with an offset.

   :seealso:

     - :gf:`byte-storage-offset-address`

.. generic-function:: byte-vector-fill
   :sealed:

   Fill a byte vector with a specific byte value.

   :signature: byte-vector-fill (target value #key start end) => ()

   :parameter target: The byte vector to fill.  An instance of :class:`<byte-vector>`.
   :parameter value: The value with which to fill *target*.  An instance of
      :drm:`<integer>`.
   :parameter start: The index at which to start filling *target*. An instance of
      :drm:`<integer>`, default 0.
   :parameter end: The index before which to stop filling *target*. An instance of
      :drm:`<integer>`, default ``target.size``.

.. method:: byte-vector-fill
   :specializer: <byte-vector>, <integer>
   :sealed:
   :no-contents-entry:

.. method:: byte-vector-fill
   :specializer: <byte-vector>, <byte-character>
   :sealed:
   :no-contents-entry:

.. function:: byte-vector-ref

   :signature: byte-vector-ref (byte-vector index) => (#rest results)

   :parameter byte-vector: An instance of :const:`<byte-vector>`.
   :parameter index: An instance of :drm:`<integer>`.
   :value #rest results: An instance of :drm:`<object>`.

.. function:: byte-vector-ref-setter

   :signature: byte-vector-ref-setter (value byte-vector index) => (#rest results)

   :parameter value: An instance of :drm:`<integer>`.
   :parameter byte-vector: An instance of :const:`<byte-vector>`.
   :parameter index: An instance of :drm:`<integer>`.
   :value value: An instance of :drm:`<integer>`.

.. generic-function:: copy-bytes
   :open:

   Efficiently copy bytes from one sequence to another.

   :signature: copy-bytes (dst dst-start src src-start n) => ()

   :parameter dst: The destination sequence.  An instance of :drm:`<mutable-sequence>`.
   :parameter dst-start: The start index in ``dst`` at which to store bytes.  An instance
      of :drm:`<integer>`.
   :parameter src: The source sequence.  An instance of :drm:`<sequence>`.
   :parameter src-start: The start index in ``src`` at which to read bytes.  An instance
      of :drm:`<integer>`.
   :parameter n: The number of bytes to copy.  An instance of :drm:`<integer>`.

The available method specializers are:

.. method:: copy-bytes
   :specializer: <sequence>, <integer>, <sequence>, <integer>, <integer>
   :open:
   :no-contents-entry:

.. method:: copy-bytes
   :specializer: <vector>, <integer>, <vector>, <integer>, <integer>
   :open:
   :no-contents-entry:

.. method:: copy-bytes
   :specializer: <string>, <integer>, <string>, <integer>, <integer>
   :open:
   :no-contents-entry:

.. method:: copy-bytes
   :specializer: <string>, <integer>, <vector>, <integer>, <integer>
   :open:
   :no-contents-entry:

.. method:: copy-bytes
   :specializer: <vector>, <integer>, <string>, <integer>, <integer>
   :open:
   :no-contents-entry:

.. method:: copy-bytes
   :specializer: <byte-vector>, <integer>, <byte-vector>, <integer>, <integer>
   :sealed:
   :no-contents-entry:

.. method:: copy-bytes
   :specializer: <byte-string>, <integer>, <byte-vector>, <integer>, <integer>
   :sealed:
   :no-contents-entry:

.. method:: copy-bytes
   :specializer: <byte-vector>, <integer>, <byte-string>, <integer>, <integer>
   :sealed:
   :no-contents-entry:

.. method:: copy-bytes
   :specializer: <byte-string>, <integer>, <byte-string>, <integer>, <integer>
   :sealed:
   :no-contents-entry:

.. method:: copy-bytes
   :specializer: <byte-vector>, <integer>, <simple-object-vector>, <integer>, <integer>
   :sealed:
   :no-contents-entry:

.. method:: copy-bytes
   :specializer: <simple-object-vector>, <integer>, <byte-vector>, <integer>, <integer>
   :sealed:
   :no-contents-entry:

.. method:: hexstring
   :specializer: <byte-vector>
   :sealed:

   Returns a string of lowercase hexadecimal digits representing the data.

   :signature: hexstring (data) => (result)

   :parameter data: An instance of :class:`<byte-vector>`.
   :value result: An instance of :drm:`<byte-string>`.

   :seealso:

     - :meth:`from-hexstring(<byte-string>)`

.. method:: from-hexstring
   :specializer: <byte-string>
   :sealed:

   Returns a :class:`<byte-vector>` containing `data` interpreted as a hexadecimal
   representation of a series of bytes.

   :signature: from-hexstring (string) => (result)

   :parameter string: An instance of :drm:`<byte-string>`.
   :value result: An instance of :class:`<vector>`.

   :seealso:

     - :meth:`hexstring(<byte-vector>)`
