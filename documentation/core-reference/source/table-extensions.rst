***********************
The Collections library
***********************

.. current-library:: collections
.. current-module:: table-extensions

Introduction
============

The Collections library’s Table Extensions module extends the Dylan
language’s standard table features. It is available to applications as
the *table-extensions* module.

.. note:: Common Dylan provides a slightly different table implementation
   from that described by the DRM. See :ref:`language-differences`
   for details of these differences.

Basics
------

The *table-extensions* module exports the class :class:`<string-table>`;
the type :class:`<hash-state>` ; the generic function
:gf:`remove-all-keys!` and two methods thereon; and the functions
:func:`collection-hash`, :func:`sequence-hash`, :func:`string-hash`,
:func:`values-hash`, :func:`case-insensitive-string-hash`, and
:func:`case-insensitive-equal`.

The :class:`<string-table>` class is a class of tables that use strings
for keys.

The :class:`<hash-state>` type implements *hash states*. A hash state is
defined by the DRM, page 123, as “an implementation-dependent type that
is associated with a hash id and can be used by the implementation to
determine whether the hash id has been invalidated.” See pages 122–123
of the DRM for more details.

The various hash functions and the :func:`case-insensitive-equal`
equivalence predicate are convenient building blocks for creating new
table classes and hash functions.

Hash functions
--------------

Different hash functions are not required to return the same hash code
for equal or even identical objects. For instance,

.. code-block:: dylan

  collection-hash(#(), object-hash, object-hash);

is not guaranteed to return the same values as

.. code-block:: dylan

  sequence-hash(#(), object-hash);

Furthermore, :func:`collection-hash` with ``ordered: #t`` is not
guaranteed to return the same hash code as :func:`collection-hash` with
``ordered: #f``. Such a requirement would render the ``ordered:``
keyword useless.

Weak tables
-----------

Common Dylan allows all general instances of the built-in class
``<table>`` to be *weak*. See :ref:`weak tables <weak-tables>` of
this volume for information about weakness.

You can create weak tables with the ``<table>`` class’s *weak:*
init-keyword. The legal values for this keyword are:

-  *#"key"* Creates a table with weak keys. When there are no longer any
   strong references to a key, the table entry of which it is part
   becomes eligible for garbage collection.
-  *#"value"* Creates a table with weak values. When there are no longer
   any strong references to a value, the table entry of which it is a
   part becomes eligible for garbage collection.
-  *#f* Creates a table with strong keys and values. This is the default
   value.

The TABLE-EXTENSIONS module
===========================

This section contains a reference description for each item exported
from the module *table-extensions*.

.. class:: <string-table>
   :sealed:

   A table class that uses strings for keys.

   :superclasses: <table>

   :description:

     The ``<string-table>`` class is the class of tables that use
     instances of ``<string>`` for their keys. It is an error to use a
     key that is not an instance of ``<string>``.

     Keys are compared with the equivalence predicate ``\=``.

     The elements of the table are instances of ``<object>``.

     It is an error to modify a key once it has been used to add an
     element to a ``<string-table>``. The effects of modification are
     not defined.

.. class:: <hash-state>

   A hash state.

   :superclasses:  <object>

   :description:

     Anything that the Dylan Reference Manual describes as a *hash
     state* is an instance of this type.

     Examples of hash states include the second argument and second
     return value of :func:`object-hash`.

.. function:: collection-hash

   Hashes the elements of a collection.

   :signature: collection-hash *key-hash-function* *elt-hash-function* *collection* *initial-state* #key *ordered* => *hash-id* *hash-state*

   :parameter key-hash-function: An instance of ``<function>``.
   :parameter elt-hash-function: An instance of ``<function>``.
   :parameter collection: An instance of ``<collection>``.
   :parameter initial-state: An instance of ``<hash-state>``.
   :parameter #key ordered: An instance of ``<boolean>``. Default value: ``#f``.
   :value hash-id: An instance of ``<integer>``.
   :value result-state: An instance of ``<hash-state>``.

   :description:

     Hashes every element of *collection* using *key-hash-function* on
     the keys and *elt-hash-function* on the elements, and merges the
     resulting hash codes in order.

     The *ordered* keyword is passed on to :func:`merge-hash-ids`.

     The functions *key-hash-function* and *elt-hash-function* must be
     suitable for use as hash functions. See page :drm:`123 of the DRM
     <Tables#XREF-1049>`.

.. function:: sequence-hash

   Hashes the elements of a sequence.

   :signature: sequence-hash *elt-hash-function* *sequence* *initial-state* #key *ordered* => *hash-id* *result-state*

   :parameter elt-hash-function: An instance of ``<function>``.
   :parameter sequence: An instance of ``<sequence>``.
   :parameter initial-state: An instance of ``<hash-state>``.
   :parameter #key ordered: An instance of ``<boolean>``. Default value: ``#f``.
   :value hash-id: An instance of ``<integer>``.
   :value result-state: An instance of ``<hash-state>``.

   :description:

     Hashes every element of *sequence* using *elt-hash-function*, and
     merges the resulting hash codes in order.

     The function *elt-hash-function* must be suitable for use as a hash
     function. See page :drm:`123 of the DRM <Tables#XREF-1049>`.

     The *ordered* keyword is passed on to :func:`merge-hash-ids`.

.. function:: values-hash

   Hashes the values passed to it.

   :signature: values-hash *elt-hash-function* *initial-state* #rest *arguments* => *hash-id* *result-state*

   :parameter elt-hash-function: An instance of ``<function>``.
   :parameter hash-state: An instance of ``<hash-state>``.
   :parameter initial-state: An instance of ``<hash-state>``.
   :parameter #rest arguments: Instances of ``<object>``.
   :value hash-id: An instance of ``<integer>``.
   :value result-state: An instance of ``<hash-state>``.

   :description:

     Hashes every object in *arguments* using *elt-hash-function*, and
     merges the resulting hash codes in order.

     The function *elt-hash-function* must be suitable for use as a hash
     function. See page :drm:`123 of the DRM <Tables#XREF-1049>`.

     The *ordered* keyword is passed on to :func:`merge-hash-ids`.

.. function:: string-hash

   Hashes a string.

   :signature: string-hash *string* *initial-state* => *hash-id* *result-state*

   :parameter string: An instance of ``<string>``.
   :parameter initial-state: An instance of ``<hash-state>``.
   :value hash-id: An instance of ``<integer>``.
   :value result-state: An instance of ``<hash-state>``.

   :description:

     Produces a hash code for a string, using the equivalence predicate
     ``\=``.

.. function:: case-insensitive-string-hash

   Hashes a string, without considering case information.

   :signature: case-insensitive-string-hash *string* *initial-state* => *hash-id* *result-state*

   :parameter string: An instance of ``<string>``.
   :parameter initial-state: An instance of ``<hash-state>``.
   :value hash-id: An instance of ``<integer>``.
   :value result-state: An instance of ``<hash-state>``.

   :description:

     Produces a hash code for a string using the equivalence predicate
     :func:`case-insensitive-equal`, which does not consider the case of
     the characters in the strings it compares.

   See also

   :func:`case-insensitive-equal`

.. function:: case-insensitive-equal

   Compares two strings for equality, ignoring case differences between
   them.

   :signature: case-insensitive-equal *string1* *string2* => *boolean*

   :parameter string1: An instance of ``<string>``.
   :parameter string2: An instance of ``<string>``.
   :value boolean: An instance of ``<boolean>``.

   :description:

     Compares *string1* and *string2* for equality, ignoring any case
     differences between them. Returns true if they are equal and false
     otherwise.

     The function has the same behavior as Dylan’s standard method on *=* for
     sequences, except that when comparing alphabetical characters, it
     ignores any case differences.

     This function is used as an equivalence predicate by
     :func:`case-insensitive-string-hash`.

     This function uses *as-uppercase* or *as-lowercase* to convert the
     characters in its string arguments.

   :example:

     The *case-insensitive-equal* function returns true if passed the
     following strings:

     .. code-block:: dylan

       "The Cat SAT ON the Mat"
       "The cat sat on the Mat"

     Conversely, the standard method on *=* returns false when passed those
     strings.

   See also

   :func:`case-insensitive-string-hash`

.. generic-function:: remove-all-keys!
   :open:

   Removes all keys from a collection and leaves it empty.

   :signature: remove-all-keys! *collection* => *collection*

   :parameter collection: An instance of ``<mutable-explicit-key-collection>``.
   :value collection: An instance of ``<mutable-explicit-key-collection>``.

   :description:

     Modifies *collection* by removing all its keys and elements, and leaves
     it empty.

     .. note:: To empty collections that are not instances of
        ``<mutable-explicit-key-collection>``, use *size-setter*.

.. method:: remove-all-keys!
   :specializer: <mutable-explicit-key-collection>

   Removes all keys from a collection and leaves it empty.

   :signature: remove-all-keys! *collection* => *collection*

   :parameter collection: An instance of ``<mutable-explicit-key-collection>``.
   :value collection: An instance of ``<mutable-explicit-key-collection>``.

   :description

     Modifies *collection* by removing all its keys and elements, and
     leaves it empty. This method implements the generic function by
     making repeated calls to ``remove-key!``.

     .. note:: To empty collections that are not instances of
        ``<mutable-explicit-key-collection>``, use *size-setter*.

.. method:: remove-all-keys!
   :specializer: <table>

   Removes all keys from a table and leaves it empty.

   :signature: remove-all-keys! *table* => *table*

   :parameter table: An instance of ``<table>``.
   :parameter table: An instance of ``<table>``.

   :description:

     Modifies *table* by removing all its keys and elements, and leaves
     it empty.

     This method does not use ``remove-key!``.

     .. note:: To empty collections that are not instances of
        ``<mutable-explicit-key-collection>``, use *size-setter*.
