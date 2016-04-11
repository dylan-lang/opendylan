**********************
The byte-vector Module
**********************

.. current-library:: common-dylan
.. current-module:: byte-vector


.. type:: <byte-vector>

   :equivalent: ``limited(<vector>, of: <byte>)``

   :seealso:

     - :type:`<byte>`

.. type:: <byte>

   :equivalent: ``limited(<integer>, min: 0, max: 255)``

.. generic-function:: byte-storage-address
   :open:

   :signature: byte-storage-address (the-buffer) => (result-offset)

   :parameter the-buffer: An instance of :drm:`<object>`.
   :value result-offset: An instance of :class:`<machine-word>`.

.. method:: byte-storage-address
   :specializer: <byte-string>
   :sealed:

.. method:: byte-storage-address
   :specializer: <byte-vector>
   :sealed:

.. generic-function:: byte-storage-offset-address
   :open:

   :signature: byte-storage-offset-address (the-buffer data-offset) => (result-offset)

   :parameter the-buffer: An instance of :drm:`<object>`.
   :parameter data-offset: An instance of :drm:`<integer>`.
   :value result-offset: An instance of :class:`<machine-word>`.

.. method:: byte-storage-offset-address
   :specializer: <byte-string>, <integer>
   :sealed:

.. method:: byte-storage-offset-address
   :specializer: <byte-vector>, <integer>
   :sealed:

.. generic-function:: byte-vector-fill

   :signature: byte-vector-fill (target value) => (#rest results)

   :parameter target: An instance of :drm:`<object>`.
   :parameter value: An instance of :drm:`<object>`.
   :value #rest results: An instance of :drm:`<object>`.

.. method:: byte-vector-fill
   :specializer: <byte-vector>, <integer>
   :sealed:

.. method:: byte-vector-fill
   :specializer: <byte-vector>, <byte-character>
   :sealed:

.. function:: byte-vector-ref

   :signature: byte-vector-ref (byte-vector index) => (#rest results)

   :parameter byte-vector: An instance of :const:`<byte-vector>`.
   :parameter index: An instance of :drm:`<integer>`.
   :value #rest results: An instance of :drm:`<object>`.

.. function:: byte-vector-ref-setter

   :signature: byte-vector-ref-setter (value byte-vector index) => (#rest results)

   :parameter value: An instance of :drm:`<object>`.
   :parameter byte-vector: An instance of :const:`<byte-vector>`.
   :parameter index: An instance of :drm:`<integer>`.
   :value #rest results: An instance of :drm:`<object>`.

.. generic-function:: copy-bytes
   :open:

   :signature: copy-bytes (dst dst-start src src-start n) => ()

   :parameter dst: An instance of :drm:`<object>`.
   :parameter dst-start: An instance of :drm:`<object>`.
   :parameter src: An instance of :drm:`<object>`.
   :parameter src-start: An instance of :drm:`<object>`.
   :parameter n: An instance of :drm:`<object>`.

.. method:: copy-bytes
   :specializer: <sequence>, <integer>, <sequence>, <integer>, <integer>
   :open:

.. method:: copy-bytes
   :specializer: <vector>, <integer>, <vector>, <integer>, <integer>
   :open:

.. method:: copy-bytes
   :specializer: <string>, <integer>, <string>, <integer>, <integer>
   :open:

.. method:: copy-bytes
   :specializer: <string>, <integer>, <vector>, <integer>, <integer>
   :open:

.. method:: copy-bytes
   :specializer: <vector>, <integer>, <string>, <integer>, <integer>
   :open:

.. method:: copy-bytes
   :specializer: <byte-vector>, <integer>, <byte-vector>, <integer>, <integer>
   :sealed:

.. method:: copy-bytes
   :specializer: <byte-string>, <integer>, <byte-vector>, <integer>, <integer>
   :sealed:

.. method:: copy-bytes
   :specializer: <byte-vector>, <integer>, <byte-string>, <integer>, <integer>
   :sealed:

.. method:: copy-bytes
   :specializer: <byte-string>, <integer>, <byte-string>, <integer>, <integer>
   :sealed:

.. method:: copy-bytes
   :specializer: <byte-vector>, <integer>, <simple-object-vector>, <integer>, <integer>
   :sealed:

.. method:: copy-bytes
   :specializer: <simple-object-vector>, <integer>, <byte-vector>, <integer>, <integer>
   :sealed:
