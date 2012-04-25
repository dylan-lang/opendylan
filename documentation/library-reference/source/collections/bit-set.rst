******************
The bit-set Module
******************

.. current-library:: collections
.. current-module:: bit-set

.. class:: <bit-set>
   :primary:

   :superclasses: :class:`<set>`

   :keyword all-members-from:
   :keyword member-vector:
   :keyword members:
   :keyword pad:
   :keyword upper-bound-hint:

.. function:: copy-bit-set!

   :signature: copy-bit-set! *set1* *set2* => ()

   :parameter set1: An instance of :class:`<bit-set>`.
   :parameter set2: An instance of :class:`<bit-set>`.

.. function:: empty-bit-set!

   :signature: empty-bit-set! *set* => ()

   :parameter set: An instance of :class:`<bit-set>`.

.. generic-function:: infinite?

   :signature: infinite? *set* => #rest *results*

   :parameter set: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: set-add

   :signature: set-add *set* *i* => #rest *results*

   :parameter set: An instance of ``<object>``.
   :parameter i: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: set-add!

   :signature: set-add! *set* *i* => #rest *results*

   :parameter set: An instance of ``<object>``.
   :parameter i: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: set-complement

   :signature: set-complement *set* => #rest *results*

   :parameter set: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: set-complement!

   :signature: set-complement! *set* => #rest *results*

   :parameter set: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: set-difference

   :signature: set-difference *set1* *set2* => #rest *results*

   :parameter set1: An instance of ``<object>``.
   :parameter set2: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: set-difference!

   :signature: set-difference! *set1* *set2* => #rest *results*

   :parameter set1: An instance of ``<object>``.
   :parameter set2: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: set-intersection

   :signature: set-intersection *set1* *set2* => #rest *results*

   :parameter set1: An instance of ``<object>``.
   :parameter set2: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: set-intersection!

   :signature: set-intersection! *set1* *set2* => #rest *results*

   :parameter set1: An instance of ``<object>``.
   :parameter set2: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: set-remove

   :signature: set-remove *set* *i* => #rest *results*

   :parameter set: An instance of ``<object>``.
   :parameter i: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: set-remove!

   :signature: set-remove! *set* *i* => #rest *results*

   :parameter set: An instance of ``<object>``.
   :parameter i: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: set-union

   :signature: set-union *set1* *set2* => #rest *results*

   :parameter set1: An instance of ``<object>``.
   :parameter set2: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: set-union!

   :signature: set-union! *set1* *set2* => #rest *results*

   :parameter set1: An instance of ``<object>``.
   :parameter set2: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. function:: universal-bit-set!

   :signature: universal-bit-set! *set* => ()

   :parameter set: An instance of :class:`<bit-set>`.
