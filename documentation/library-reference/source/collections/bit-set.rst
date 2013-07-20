******************
The bit-set Module
******************

.. current-library:: collections
.. current-module:: bit-set

.. class:: <bit-set>
   :primary:

   :superclasses: :class:`<set>`

   :keyword all-members-from: If this is a non-negative integer then the set
      created will be infinite. All integers greater than
      or equal to the one supplied will be members of the
      set. The default is ``#f``.
   :keyword member-vector:
   :keyword members: If supplied, this gives the initial elements of the
      set as a sequence of integers.
   :keyword pad:
   :keyword upper-bound-hint: An integer which indicates that all the elements of
      the set are expected to below this value. This is
      merely an aid to the implementation when allocating
      the set, and integers which are greater than or equal
      to this number can be added at any time. The default
      is zero.

   :description:

     Represents finite sets and some infinite sets over the non-negative
     integers in an efficient manner using a :class:`<bit-vector>`. The
     infinite sets which can be represented are those which are the
     complement of a finite set.


.. function:: copy-bit-set!

   :signature: copy-bit-set! *set1* *set2* => ()

   :parameter set1: An instance of :class:`<bit-set>`.
   :parameter set2: An instance of :class:`<bit-set>`.

   :description:

     Destructively modifies ``set1`` so that it contains exactly the same
     elements as ``set2``. After the copy, ``set1`` and ``set2`` do not share
     any structure.


.. function:: empty-bit-set!

   :signature: empty-bit-set! *set* => ()

   :parameter set: An instance of :class:`<bit-set>`.

   :description:

     Destructively modifies ``set`` by removing all its elements.

.. generic-function:: infinite?
   :sealed:

   :signature: infinite? *set* => *result*

   :parameter set: An instance of :class:`<bit-set>`.
   :value result: An instance of :drm:`<boolean>`.

   :description:

      Returns ``#t`` if the set is infinite, otherwise ``#f``.

.. generic-function:: member?
   :sealed:

   :signature: member? *set* *element* => *result*

   :parameter set: An instance of :class:`<bit-set>`.
   :parameter element: An instance of :drm:`<integer>`.
   :value result: An instance of :drm:`<boolean>`.

   :description:

     Returns ``#t`` if ``element`` is a member of the set, otherwise ``#f``.
     ``element`` must be a non-negative integer.

.. generic-function:: set-add
   :sealed:

   :signature: set-add *set* *element* => *new-set*

   :parameter set1: An instance of :class:`<bit-set>`.
   :parameter set2: An instance of :class:`<bit-set>`.
   :value new-set: An instance of :class:`<bit-set>`.

   :description:

     Returns a new bit set which includes all the elements in ``set`` and
     ``element`` which must be a non-negative integer.

.. generic-function:: set-add!
   :sealed:

   :signature: set-add! *set* *element* => *new-set*

   :parameter set: An instance of :class:`<bit-set>`.
   :parameter element: An instance of :drm:`<integer>`.
   :value new-set: An instance of :class:`<bit-set>`.

   :description:

     Modifies ``set`` to include ``element``. The returned set, ``new-set ==
     set``. ``element`` must be a non-negative integer.

.. generic-function:: set-complement
   :sealed:

   :signature: set-complement *set* => *new-set*

   :parameter set: An instance of :class:`<bit-set>`.
   :value new-set: An instance of :class:`<bit-set>`.

   :description:

     Returns a bit-set which represents the complement of the argument set.

.. generic-function:: set-complement!
   :sealed:

   :signature: set-complement! *set* => *new-set*

   :parameter set: An instance of :class:`<bit-set>`.
   :value new-set: An instance of :class:`<bit-set>`.

   :description:

     Alters ``set`` so that it contains the complement of the original set.
     ``new-set == set``.

.. generic-function:: set-difference
   :sealed:

   :signature: set-difference *set1* *set2* => *new-set*

   :parameter set1: An instance of :class:`<bit-set>`.
   :parameter set2: An instance of :class:`<bit-set>`.
   :value new-set: An instance of :class:`<bit-set>`.

   :description:

     Returns a new bit-set whose elements are determined by removing elements
     from ``set1`` which are also members of ``set2``. Neither ``set1`` or
     ``set2`` will be altered.

.. generic-function:: set-difference!
   :sealed:

   :signature: set-difference! *set1* *set2* => *new-set*

   :parameter set1: An instance of :class:`<bit-set>`.
   :parameter set2: An instance of :class:`<bit-set>`.
   :value new-set: An instance of :class:`<bit-set>`.

   :description:

     Alters ``set1`` by removing those elements which are also members of
     ``set2``. ``new-set == set1``.


.. generic-function:: set-intersection
   :sealed:

   :signature: set-intersection *set1* *set2* => *new-set*

   :parameter set1: An instance of :class:`<bit-set>`.
   :parameter set2: An instance of :class:`<bit-set>`.
   :value new-set: An instance of :class:`<bit-set>`.

   :description:

     Returns a new bit-set containing only elements which appear in both
     ``set1`` and ``set2``. Neither ``set1`` or ``set2`` will be altered.

.. generic-function:: set-intersection!
   :sealed:

   :signature: set-intersection! *set1* *set2* => *new-set*

   :parameter set1: An instance of :class:`<bit-set>`.
   :parameter set2: An instance of :class:`<bit-set>`.
   :value new-set: An instance of :class:`<bit-set>`.

   :description:

     Alters ``set1`` so that it only contains those elements which are also
     members of ``set2``. ``new-set == set1``.

.. generic-function:: set-remove
   :sealed:

   :signature: set-remove *set* *element* => *new-set*

   :parameter set: An instance of :class:`<bit-set>`.
   :parameter element: An instance of :drm:`<integer>`.
   :value new-set: An instance of :class:`<bit-set>`.

   :description:

     Returns a new bit-set which includes all the elements in ``set`` except
     for ``element`` which must be a non-negative integer.


.. generic-function:: set-remove!
   :sealed:

   :signature: set-remove! *set* *element* => *new-set*

   :parameter set: An instance of :class:`<bit-set>`.
   :parameter element: An instance of :drm:`<integer>`.
   :value new-set: An instance of :class:`<bit-set>`.

   :description:

     Modifies ``set`` so that it no longer contains ``element``. The
     returned set, ``new-set == set``. ``element`` must be a non-negative
     integer.


.. generic-function:: set-union
   :sealed:

   :signature: set-union *set1* *set2* => *new-set*

   :parameter set1: An instance of :class:`<bit-set>`.
   :parameter set2: An instance of :class:`<bit-set>`.
   :value new-set: An instance of :class:`<bit-set>`.

   :description:

     Returns a new bit-set containing every element of ``set1`` and
     ``set2``. Neither ``set1`` or ``set2`` will be altered.


.. generic-function:: set-union!
   :sealed:

   :signature: set-union! *set1* *set2* => *new-set*

   :parameter set1: An instance of :class:`<bit-set>`.
   :parameter set2: An instance of :class:`<bit-set>`.
   :value new-set: An instance of :class:`<bit-set>`.

   :description:

     Alters ``set1`` so that it also contains the elements in ``set2``.
     ``new-set == set1``.

.. generic-function:: size
   :sealed:

   :signature: size *set* => *false-or-integer*

   :parameter set: An instance of :class:`<bit-set>`.
   :value size: Either ``#f`` or an instance of :drm:`<integer>`.

   :description:

     Returns the cardinality of the set or ``#f`` if the set is
     infinite. This operation may be relatively slow.

.. function:: universal-bit-set!

   :signature: universal-bit-set! *set* => ()

   :parameter set: An instance of :class:`<bit-set>`.

   :description:

     Destructively modifies ``set`` to include all non-negative integers
     as members.
