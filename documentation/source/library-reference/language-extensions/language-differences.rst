********************
Language Differences
********************

.. current-library:: dylan
.. current-module:: dylan

This document describes the ways in which Open Dylan differs from the
specification in the :drm:`Dylan Reference Manual <Title>`.

Note that in addition to the intentional differences documented below, there
are several `known bugs
<https://github.com/dylan-lang/opendylan/issues?q=is%3Aissue+is%3Aopen+label%3ADRM>`_
related to compliance with the language standard as documented in the DRM.

``add!`` and ``remove!``
========================

The :drm:`add!` and :drm:`remove!` generic functions are defined to work on all
collections instead of only on sequences. This is because the methods are also useful for
tables and sets.  Specifically, the generics are defined as follows:

.. code:: dylan

   define open generic add!
       (coll :: <collection>, new-element)
    => (possibly-new-coll :: <collection>);

   define open generic remove!
       (coll :: <collection>, value, #key test, count)
    => (possibly-new-coll :: <collection>);

Table Protocol
==============

For efficiency, Open Dylan adopts a slightly different table protocol
to that described by the DRM. Hashing functions take an additional
hash-state argument and merge it into the hash-state result. The
function :drm:`merge-hash-codes` is replaced by :func:`merge-hash-ids` because
hash-states are merged as part of the hashing process. The constant
``$permanent-hash-state`` is no longer required; the same effect can be
achieved by returning the argument *hash-state* unchanged as the result
*hash-state*. Finally, :func:`object-hash` has been altered to use the new
protocol.

This section describes the items that have been changed. We also provide
a :doc:`table-extensions module <../collections/table-extensions>`.

.. generic-function:: table-protocol
   :open:

   Returns functions used to implement the iteration protocol for tables.

   :signature: table-protocol *table* => *test-function* *hash-function*

   :parameter table: An instance of :drm:`<table>`.
   :value test-function: An instance of :drm:`<function>`.
   :value hash-function: An instance of :drm:`<function>`.

   :description:

     Returns the functions used to iterate over tables. These functions are
     in turn used to implement the other collection operations on :drm:`<table>`.

     The *test-function* is used to compare table keys. It returns true if,
     according to the table's equivalence predicate, the keys are members of
     the same equivalence class. Its signature must be::

       test-function *key1* *key2* => *boolean*

     The *hash-function* must compute the hash code of a key. Its signature
     must be::

       hash-function *key* *initial-state* => *id* *result-state*

     In this signature, *initial-state* is an instance of :class:`<hash-state>`.
     The hash function computes the hash code of *key*, using the hash
     function that is associated with the table's equivalence predicate. The
     hash code is returned as two values: an integer *id* and a hash-state
     *result-state*. This *result-state* is obtained by merging the
     *initial-state* with the hash-state that results from hashing *key*.
     The *result-state* may or may not be :drm:`==` to *initial-state*. The
     *initial-state* may be modified by this operation.

.. function:: merge-hash-ids

   Returns a hash ID created by merging two hash IDs.

   :signature: merge-hash-ids *id1* *id2* #key *ordered* => *merged-id*

   :parameter id1: An instance of :drm:`<integer>`.
   :parameter id2: An instance of :drm:`<integer>`.
   :parameter ordered: An instance of :drm:`<boolean>`. Default value: :drm:`#f`.
   :value merged-id: An instance of :drm:`<integer>`.

   :description:

     Computes a new hash ID by merging the argument hash IDs in some
     implementation-dependent way. This can be used, for example, to
     generate a hash ID for an object by combining hash IDs of some of
     its parts.

     The *ordered* argument determines whether the algorithm used to merge the
     IDs is permitted to be order-dependent. If false (the default), the merged
     result must be independent of the order in which the arguments are
     provided. If true, the order of the arguments matters because the
     algorithm used need not be either commutative or associative. It is best
     to provide a true value for *ordered* when possible, as this may result in
     a better distribution of hash IDs. However, *ordered* must only be true if
     that will not cause the hash function to violate the second constraint on
     hash functions, :drm:`described in the DRM <Tables#XREF-1049>`.

.. function:: object-hash

   The hash function for the equivalence predicate :drm:`==`.

   :signature: object-hash *object* *initial-state* => *hash-id* *result-state*

   :parameter object: An instance of :drm:`<integer>`.
   :parameter initial-state: An instance of :class:`<hash-state>`.
   :value hash-id: An instance of :drm:`<integer>`.
   :value result-state: An instance of :class:`<hash-state>`.

   :description:

     Returns a hash code for *object* that corresponds to the
     equivalence predicate :drm:`==`.

     This function is a useful tool for writing hash functions in which
     the object identity of some component of a key is to be used in
     computing the hash code.

     It returns a hash ID (an integer) and the result of merging the
     initial state with the associated hash state for the object,
     computed in some implementation-dependent manner.

