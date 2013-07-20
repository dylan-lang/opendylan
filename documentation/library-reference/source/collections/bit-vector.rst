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

   :keyword size: Specifies the number of bits in the bit-vector. The
                  default is 0.

   :description:

     A compact representation of a vector of bits. The elements of the
     vector have the type ``<bit>`` and may be the values 0 and 1. The
     elements are indexed from ``0`` up to ``(size - 1)``.

.. constant:: <bit>

   :description:

     A subtype of :drm:`<integer>`, this is the type of elements of
     :class:`<bit-vector>`. Objects of this type may have the value
     ``0`` or ``1``.

.. function:: bit-count

   :signature: bit-count *vector* #key *bit-value* => *count*

   :parameter vector: An instance of :class:`<bit-vector>`.
   :parameter #key bit-value: An instance of :class:`<bit>`.
   :value count: An instance of :drm:`<integer>`.

   :description:

     Returns the number of bits in ``vector`` which are equal to
     ``bit-value``. This may be a relatively slow operation.

.. function:: bit-vector-and

   :signature: bit-vector-and *vector1* *vector2* #key *pad1* *pad2* => *result* *pad*

   :parameter vector1: An instance of :class:`<bit-vector>`.
   :parameter vector2: An instance of :class:`<bit-vector>`.
   :parameter #key pad1: An instance of :class:`<bit>`.
   :parameter #key pad2: An instance of :class:`<bit>`.
   :value result: An instance of :class:`<bit-vector>`.
   :value pad: An instance of :class:`<bit>`.

   :description:

     Returns a new vector which is the bitwise and of the two argument
     vectors. Each vector has an associated pad value. If the vectors are of
     different lengths, the shorter is considered extended with its pad
     value. The size of the result vector may be extended or shortened
     provided the bits added or dropped are the same as the result-pad value.
     The size of the result will be no smaller than the minimum of the
     argument sizes, and no greater than the maximum of the argument sizes.
     The result-pad value is calculated by taking the logical and of the two
     input pad values.

.. function:: bit-vector-and!

   :signature: bit-vector-and! *vector1* *vector2* #key *pad1* *pad2* => *result* *pad*

   :parameter vector1: An instance of :class:`<bit-vector>`.
   :parameter vector2: An instance of :class:`<bit-vector>`.
   :parameter #key pad1: An instance of :class:`<bit>`.
   :parameter #key pad2: An instance of :class:`<bit>`.
   :value result: An instance of :class:`<bit-vector>`.
   :value pad: An instance of :class:`<bit>`.

   :description:

     Returns a vector which is the bitwise and of the two argument vectors.
     ``vector1`` may or may not be modified by this operation. Each vector
     has an associated pad value. If the vectors are of different lengths,
     the shorter is considered extended with its pad value. The size of the
     result vector may be extended or shortened provided the bits added or
     dropped are the same as the result-pad value. The size of the result
     will be no smaller than the minimum of the argument sizes, and no
     greater than the maximum of the argument sizes. The result-pad value is
     calculated by taking the logical and of the two input pad values.

.. function:: bit-vector-andc2

   :signature: bit-vector-andc2 *vector1* *vector2* #key *pad1* *pad2* => *result* *pad*

   :parameter vector1: An instance of :class:`<bit-vector>`.
   :parameter vector2: An instance of :class:`<bit-vector>`.
   :parameter #key pad1: An instance of :class:`<bit>`.
   :parameter #key pad2: An instance of :class:`<bit>`.
   :value result: An instance of :class:`<bit-vector>`.
   :value pad: An instance of :class:`<bit>`.

   :description:

     Returns a new vector which is the result of taking the bitwise and of
     ``vector1`` and the bitwise not of ``vector2``. Each vector has an
     associated pad value. If the vectors are of different lengths, the shorter
     is considered extended with its pad value. The size of the result vector
     may be extended or shortened provided the bits added or dropped are the
     same as the result-pad value. The size of the result will be no smaller
     than the minimum of the argument sizes, and no greater than the maximum
     of the argument sizes. The result-pad value is calculated by taking the
     logical and of ``pad1`` with the complement of ``pad2``.

.. function:: bit-vector-andc2!

   :signature: bit-vector-andc2! *vector1* *vector2* #key *pad1* *pad2* => *result* *pad*

   :parameter vector1: An instance of :class:`<bit-vector>`.
   :parameter vector2: An instance of :class:`<bit-vector>`.
   :parameter #key pad1: An instance of :class:`<bit>`.
   :parameter #key pad2: An instance of :class:`<bit>`.
   :value result: An instance of :class:`<bit-vector>`.
   :value pad: An instance of :class:`<bit>`.

   :description:

     Returns a vector which is the result of taking the bitwise and of
     ``vector1`` and the bitwise not of ``vector2``. ``vector1`` may or
     may not be modified by this operation. Each vector has an associated
     pad value. If the vectors are of different lengths, the shorter is
     considered extended with its pad value. The size of the result vector
     may be extended or shortened provided the bits added or dropped are
     the same as the result-pad value. The size of the result will be no
     smaller than the minimum of the argument sizes, and no greater than
     the maximum of the argument sizes. The result-pad value is calculated
     by taking the logical and of ``pad1`` with the complement of ``pad2``.

.. function:: bit-vector-not

   :signature: bit-vector-not *vector* #key *pad* => *result* *result-pad*

   :parameter vector: An instance of :class:`<bit-vector>`.
   :parameter #key pad: An instance of :class:`<bit>`.
   :value result: An instance of :class:`<bit-vector>`.
   :value result-pad: An instance of :class:`<bit>`.

   :description:

     Returns a new vector which is the bitwise not of its argument.

.. function:: bit-vector-not!

   :signature: bit-vector-not! *vector* #key *pad* => *result* *result-pad*

   :parameter vector: An instance of :class:`<bit-vector>`.
   :parameter #key pad: An instance of :class:`<bit>`.
   :value result: An instance of :class:`<bit-vector>`.
   :value result-pad: An instance of :class:`<bit>`.

   :description:

     Modifies ``vector`` so that it becomes the bitwise not of its original
     contents. ``result == vector``.

.. function:: bit-vector-or

   :signature: bit-vector-or *vector1* *vector2* #key *pad1* *pad2* => *result* *pad*

   :parameter vector1: An instance of :class:`<bit-vector>`.
   :parameter vector2: An instance of :class:`<bit-vector>`.
   :parameter #key pad1: An instance of :class:`<bit>`.
   :parameter #key pad2: An instance of :class:`<bit>`.
   :value result: An instance of :class:`<bit-vector>`.
   :value pad: An instance of :class:`<bit>`.

   :description:

     Returns a new vector which is the bitwise or of the two argument
     vectors. Each vector has an associated pad value. If the vectors
     are of different lengths, the shorter is considered extended with
     its pad value. The size of the result vector may be extended or
     shortened provided the bits added or dropped are the same as the
     result-pad value. The size of the result will be no smaller
     than the minimum of the argument sizes, and no greater than the
     maximum of the argument sizes. The result-pad value is calculated
     by taking the logical or of the two input pad values.

.. function:: bit-vector-or!

   :signature: bit-vector-or! *vector1* *vector2* #key *pad1* *pad2* => *result* *pad*

   :parameter vector1: An instance of :class:`<bit-vector>`.
   :parameter vector2: An instance of :class:`<bit-vector>`.
   :parameter #key pad1: An instance of :class:`<bit>`.
   :parameter #key pad2: An instance of :class:`<bit>`.
   :value result: An instance of :class:`<bit-vector>`.
   :value pad: An instance of :class:`<bit>`.

   :description:

     Returns a vector which is the bitwise or of the two argument vectors.
     ``vector1`` may or may not be modified by this operation. Each vector
     has an associated pad value. If the vectors are of different lengths,
     the shorter is considered extended with its pad value. The size of the
     result vector may be extended or shortened provided the bits added or
     dropped are the same as the result-pad value. The size of the result
     will be no smaller than the minimum of the argument sizes and no greater
     than the maximum of the argument sizes. The result-pad value is
     calculated by taking the logical or of the two input pad values.

.. function:: bit-vector-xor

   :signature: bit-vector-xor *vector1* *vector2* #key *pad1* *pad2* => *result* *pad*

   :parameter vector1: An instance of :class:`<bit-vector>`.
   :parameter vector2: An instance of :class:`<bit-vector>`.
   :parameter #key pad1: An instance of :class:`<bit>`.
   :parameter #key pad2: An instance of :class:`<bit>`.
   :value result: An instance of :class:`<bit-vector>`.
   :value pad: An instance of :class:`<bit>`.

   :description:

     Returns a new vector which is the bitwise exclusive or of the two
     argument vectors. Each vector has an associated pad value. If the
     vectors are of different lengths, the shorter is considered extended
     with its pad value. The size of the result will be no smaller than
     the minimum of the argument sizes, and no greater than the maximum
     of the argument sizes. The size of the result vector may be extended
     or shortened provided the bits added or dropped are the same as the
     result-pad value. The result-pad value is calculated by taking the
     logical xor of the two input pad values.

.. function:: bit-vector-xor!

   :signature: bit-vector-xor! *vector1* *vector2* #key *pad1* *pad2* => *result* *pad*

   :parameter vector1: An instance of :class:`<bit-vector>`.
   :parameter vector2: An instance of :class:`<bit-vector>`.
   :parameter #key pad1: An instance of :class:`<bit>`.
   :parameter #key pad2: An instance of :class:`<bit>`.
   :value result: An instance of :class:`<bit-vector>`.
   :value pad: An instance of :class:`<bit>`.

   :description:

     Returns a vector which is the bitwise exclusive or of the two
     argument vectors. ``vector1`` may or may not be modified by this
     operation. Each vector has an associated pad value. If the vectors
     are of different lengths, the shorter is considered extended with
     its pad value. The size of the result vector may be extended or
     shortened provided the bits added or dropped are the same as the
     result-pad value. The size of the result will be no smaller than the
     minimum of the argument sizes, and no greater than the maximum of
     the argument sizes. The result-pad value is calculated by taking the
     logical xor of the two input pad values.
