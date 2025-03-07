***************************
The table-extensions Module
***************************

.. current-library:: collections
.. current-module:: table-extensions

The Collections library's *table-extensions* module extends the Dylan
language's standard table features.

.. note:: Open Dylan provides a slightly different table implementation
   from that described by the DRM. See :doc:`../language-extensions/language-differences`
   for details of these differences.

Notes on Hash Functions
=======================

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

Notes on Weak tables
====================

Open Dylan allows all general instances of the built-in class
:drm:`<table>` to be *weak*. See :doc:`weak tables <../language-extensions/weak-tables>`
for information about weakness.

You can create weak tables with the :drm:`<table>` class's *weak:*
init-keyword. The legal values for this keyword are:

- ``#"key"`` Creates a table with weak keys. When there are no longer any
  strong references to a key, the table entry of which it is part
  becomes eligible for garbage collection.
- ``#"value"`` Creates a table with weak values. When there are no longer
  any strong references to a value, the table entry of which it is a
  part becomes eligible for garbage collection.
- :drm:`#f` Creates a table with strong keys and values. This is the default.

String Tables
=============

.. class:: <string-table>
   :sealed:

   A table class that uses strings for keys and compares keys with :drm:`=`.

   :superclasses: :drm:`<table>`

   :description:

     A table that uses
     instances of :drm:`<string>` for its keys.  It is an error to use a
     key that is not an instance of :drm:`<string>`.

     Keys are compared with the equivalence predicate :drm:`=`.

     The elements of the table are instances of :drm:`<object>`.

     Modifying the key once it has been used to add an element to a
     :class:`<string-table>` results in undefined behavior.

.. class:: <case-insensitive-string-table>
   :sealed:

   A table class that uses strings for keys and compares keys with
   :func:`case-insensitive-equal`.

   :superclasses: :drm:`<table>`

   :description:

     A class of tables that use
     instances of :drm:`<string>` for their keys. It is an error to use a
     key that is not an instance of :drm:`<string>`.

     Keys are compared with the equivalence predicate
     :func:`case-insensitive-equal`.

     The elements of the table are instances of :drm:`<object>`.

     Modifying the key once it has been used to add an element to a
     :class:`<case-insensitive-string-table>` results in undefined behavior.

.. function:: case-insensitive-equal

   Compares two strings for equality, ignoring case differences between
   them.

   :signature: case-insensitive-equal *string1* *string2* => *boolean*

   :parameter string1: An instance of :drm:`<string>`.
   :parameter string2: An instance of :drm:`<string>`.
   :value boolean: An instance of :drm:`<boolean>`.

   :description:

     Compares *string1* and *string2* for equality, ignoring any case
     differences between them. Returns true if they are equal and false
     otherwise.

     The function has the same behavior as Dylan's standard method on *=* for
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

   :seealso:

     - :func:`case-insensitive-string-hash`

Hashing
=======

.. class:: <hash-state>

   A hash state.

   :superclasses: :drm:`<object>`

   :description:

     A hash state is :drm:`defined by the DRM <Tables#XREF-1049>`, as "an
     implementation-dependent type that is associated with a hash id and can be used by
     the implementation to determine whether the hash id has been invalidated."  This
     class is such a type, and anything that the DRM describes as a *hash state* is an
     instance of this type.

     Examples of hash states include the second argument and second
     return value of :func:`object-hash`.

.. function:: collection-hash

   Hashes the elements of a collection.

   :signature: collection-hash *key-hash-function* *elt-hash-function* *collection* *initial-state* #key *ordered* => *hash-id* *hash-state*

   :parameter key-hash-function: An instance of :drm:`<function>`.
   :parameter elt-hash-function: An instance of :drm:`<function>`.
   :parameter collection: An instance of :drm:`<collection>`.
   :parameter initial-state: An instance of ``<hash-state>``.
   :parameter #key ordered: An instance of :drm:`<boolean>`. Default value: :drm:`#f`.
   :value hash-id: An instance of :drm:`<integer>`.
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

   :parameter elt-hash-function: An instance of :drm:`<function>`.
   :parameter sequence: An instance of :drm:`<sequence>`.
   :parameter initial-state: An instance of ``<hash-state>``.
   :parameter #key ordered: An instance of :drm:`<boolean>`. Default value: :drm:`#f`.
   :value hash-id: An instance of :drm:`<integer>`.
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

   :parameter elt-hash-function: An instance of :drm:`<function>`.
   :parameter hash-state: An instance of ``<hash-state>``.
   :parameter initial-state: An instance of ``<hash-state>``.
   :parameter #rest arguments: Instances of :drm:`<object>`.
   :value hash-id: An instance of :drm:`<integer>`.
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

   :parameter string: An instance of :drm:`<string>`.
   :parameter initial-state: An instance of ``<hash-state>``.
   :value hash-id: An instance of :drm:`<integer>`.
   :value result-state: An instance of ``<hash-state>``.

   :description:

     Produces a hash code for a string, using the equivalence predicate
     :drm:`=`.

.. function:: case-insensitive-string-hash

   Hashes a string, without considering case information.

   :signature: case-insensitive-string-hash *string* *initial-state* => *hash-id* *result-state*

   :parameter string: An instance of :drm:`<string>`.
   :parameter initial-state: An instance of ``<hash-state>``.
   :value hash-id: An instance of :drm:`<integer>`.
   :value result-state: An instance of ``<hash-state>``.

   :description:

     Produces a hash code for a string using the equivalence predicate
     :func:`case-insensitive-equal`, which does not consider the case of
     the characters in the strings it compares.

   :seealso:

     - :func:`case-insensitive-equal`

Miscellaneous
=============

.. generic-function:: remove-all-keys!
   :open:

   Removes all keys from a collection and leaves it empty.

   :signature: remove-all-keys! *collection* => *collection*

   :parameter collection: An instance of :drm:`<mutable-explicit-key-collection>`.
   :value collection: An instance of :drm:`<mutable-explicit-key-collection>`.

   :description:

     Modifies *collection* by removing all its keys and elements, and leaves
     it empty.

     .. note:: To empty collections that are not instances of :drm:`<mutable-explicit-key-collection>`, use *size-setter*.

.. method:: remove-all-keys!
   :specializer: <mutable-explicit-key-collection>

   Removes all keys from a collection and leaves it empty.

   :signature: remove-all-keys! *collection* => *collection*

   :parameter collection: An instance of :drm:`<mutable-explicit-key-collection>`.
   :value collection: An instance of :drm:`<mutable-explicit-key-collection>`.

   :description:

     Modifies *collection* by removing all its keys and elements, and
     leaves it empty. This method implements the generic function by
     making repeated calls to ``remove-key!``.

     .. note:: To empty collections that are not instances of :drm:`<mutable-explicit-key-collection>`, use *size-setter*.

.. method:: remove-all-keys!
   :specializer: <table>

   Removes all keys from a table and leaves it empty.

   :signature: remove-all-keys! *table* => *table*

   :parameter table: An instance of :drm:`<table>`.
   :parameter table: An instance of :drm:`<table>`.

   :description:

     Modifies *table* by removing all its keys and elements, and leaves
     it empty.

     This method does not use ``remove-key!``.

     .. note:: To empty collections that are not instances of :drm:`<mutable-explicit-key-collection>`, use *size-setter*.

.. macro:: tabling
   :macro-type: Function

   Creates a table and populates it with keys and values.

   :macrocall:
     .. parsed-literal:: 
        tabling( [ `class`, ] `key` => `value`, ...)

   :parameter class:  An instance of :drm:`<class>`. Optional.
   :parameter key:    An expression.
   :parameter value:  An expression.
   :value table:      A new instance of *class*.

   :description:

     Creates a table of type *class* and populates it with *key*/*value*
     pairs. If *class* is omitted, creates a table of type :drm:`<table>`.

   :example:

     .. code-block:: dylan

       let my-table = tabling(#"red" => "stop", #"green" => "go");
       let my-table = tabling(<string-table>, "red" => "stop", "green" => "go");
