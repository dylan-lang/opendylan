*********************
The bit-vector Module
*********************

.. current-library:: collections
.. current-module:: bit-vector

.. class:: <bit-vector>
   :open:
   :abstract:
   :primary:

   :superclasses: <vector>

   :keyword size:

.. constant:: <bit>

.. function:: bit-count

   :signature: bit-count *vector* #key *bit-value* => *count*

   :parameter vector: An instance of :class:`<bit-vector>`.
   :parameter #key bit-value: An instance of :class:`<bit>`.
   :value count: An instance of ``<integer>``.

.. function:: bit-vector-and

   :signature: bit-vector-and *vector1* *vector2* #key *pad1* *pad2* => *result* *pad*

   :parameter vector1: An instance of :class:`<bit-vector>`.
   :parameter vector2: An instance of :class:`<bit-vector>`.
   :parameter #key pad1: An instance of :class:`<bit>`.
   :parameter #key pad2: An instance of :class:`<bit>`.
   :value result: An instance of :class:`<bit-vector>`.
   :value pad: An instance of :class:`<bit>`.

.. function:: bit-vector-and!

   :signature: bit-vector-and! *vector1* *vector2* #key *pad1* *pad2* => *result* *pad*

   :parameter vector1: An instance of :class:`<bit-vector>`.
   :parameter vector2: An instance of :class:`<bit-vector>`.
   :parameter #key pad1: An instance of :class:`<bit>`.
   :parameter #key pad2: An instance of :class:`<bit>`.
   :value result: An instance of :class:`<bit-vector>`.
   :value pad: An instance of :class:`<bit>`.

.. function:: bit-vector-andc2

   :signature: bit-vector-andc2 *vector1* *vector2* #key *pad1* *pad2* => *result* *pad*

   :parameter vector1: An instance of :class:`<bit-vector>`.
   :parameter vector2: An instance of :class:`<bit-vector>`.
   :parameter #key pad1: An instance of :class:`<bit>`.
   :parameter #key pad2: An instance of :class:`<bit>`.
   :value result: An instance of :class:`<bit-vector>`.
   :value pad: An instance of :class:`<bit>`.

.. function:: bit-vector-andc2!

   :signature: bit-vector-andc2! *vector1* *vector2* #key *pad1* *pad2* => *result* *pad*

   :parameter vector1: An instance of :class:`<bit-vector>`.
   :parameter vector2: An instance of :class:`<bit-vector>`.
   :parameter #key pad1: An instance of :class:`<bit>`.
   :parameter #key pad2: An instance of :class:`<bit>`.
   :value result: An instance of :class:`<bit-vector>`.
   :value pad: An instance of :class:`<bit>`.

.. function:: bit-vector-not

   :signature: bit-vector-not *vector* #key *pad* => *result* *result-pad*

   :parameter vector: An instance of :class:`<bit-vector>`.
   :parameter #key pad: An instance of :class:`<bit>`.
   :value result: An instance of :class:`<bit-vector>`.
   :value result-pad: An instance of :class:`<bit>`.

.. function:: bit-vector-not!

   :signature: bit-vector-not! *vector* #key *pad* => *result* *result-pad*

   :parameter vector: An instance of :class:`<bit-vector>`.
   :parameter #key pad: An instance of :class:`<bit>`.
   :value result: An instance of :class:`<bit-vector>`.
   :value result-pad: An instance of :class:`<bit>`.

.. function:: bit-vector-or

   :signature: bit-vector-or *vector1* *vector2* #key *pad1* *pad2* => *result* *pad*

   :parameter vector1: An instance of :class:`<bit-vector>`.
   :parameter vector2: An instance of :class:`<bit-vector>`.
   :parameter #key pad1: An instance of :class:`<bit>`.
   :parameter #key pad2: An instance of :class:`<bit>`.
   :value result: An instance of :class:`<bit-vector>`.
   :value pad: An instance of :class:`<bit>`.

.. function:: bit-vector-or!

   :signature: bit-vector-or! *vector1* *vector2* #key *pad1* *pad2* => *result* *pad*

   :parameter vector1: An instance of :class:`<bit-vector>`.
   :parameter vector2: An instance of :class:`<bit-vector>`.
   :parameter #key pad1: An instance of :class:`<bit>`.
   :parameter #key pad2: An instance of :class:`<bit>`.
   :value result: An instance of :class:`<bit-vector>`.
   :value pad: An instance of :class:`<bit>`.

.. generic-function:: bit-vector-word

   :signature: bit-vector-word *v* *i* => *r*

   :parameter v: An instance of :class:`<bit-vector>`.
   :parameter i: An instance of ``<integer>``.
   :value r: An instance of {unknown object}.

.. function:: bit-vector-xor

   :signature: bit-vector-xor *vector1* *vector2* #key *pad1* *pad2* => *result* *pad*

   :parameter vector1: An instance of :class:`<bit-vector>`.
   :parameter vector2: An instance of :class:`<bit-vector>`.
   :parameter #key pad1: An instance of :class:`<bit>`.
   :parameter #key pad2: An instance of :class:`<bit>`.
   :value result: An instance of :class:`<bit-vector>`.
   :value pad: An instance of :class:`<bit>`.

.. function:: bit-vector-xor!

   :signature: bit-vector-xor! *vector1* *vector2* #key *pad1* *pad2* => *result* *pad*

   :parameter vector1: An instance of :class:`<bit-vector>`.
   :parameter vector2: An instance of :class:`<bit-vector>`.
   :parameter #key pad1: An instance of :class:`<bit>`.
   :parameter #key pad2: An instance of :class:`<bit>`.
   :value result: An instance of :class:`<bit-vector>`.
   :value pad: An instance of :class:`<bit>`.

.. generic-function:: word-size

   :signature: word-size *object* => #rest *results*

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.
