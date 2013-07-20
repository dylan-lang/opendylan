The byte-vector Module
----------------------

.. current-library:: collections
.. current-module:: byte-vector

.. constant:: <byte-vector>

.. constant:: <byte>

.. generic-function:: byte-vector-fill

   :signature: byte-vector-fill *target* *value* => #rest *results*

   :parameter target: An instance of :drm:`<object>`.
   :parameter value: An instance of :drm:`<object>`.
   :value #rest results: An instance of :drm:`<object>`.

.. function:: byte-vector-ref

   :signature: byte-vector-ref *byte-vector* *index* => #rest *results*

   :parameter byte-vector: An instance of :class:`<byte-vector>`.
   :parameter index: An instance of :drm:`<integer>`.
   :value #rest results: An instance of :drm:`<object>`.

.. function:: byte-vector-ref-setter

   :signature: byte-vector-ref-setter *value* *byte-vector* *index* => #rest *results*

   :parameter value: An instance of :drm:`<object>`.
   :parameter byte-vector: An instance of :class:`<byte-vector>`.
   :parameter index: An instance of :drm:`<integer>`.
   :value #rest results: An instance of :drm:`<object>`.

.. generic-function:: copy-bytes
   :open:

   :signature: copy-bytes *dst* *dst-start* *src* *src-start* *n* => ()

   :parameter dst: An instance of :drm:`<object>`.
   :parameter dst-start: An instance of :drm:`<object>`.
   :parameter src: An instance of :drm:`<object>`.
   :parameter src-start: An instance of :drm:`<object>`.
   :parameter n: An instance of :drm:`<object>`.
