***********************
The Collections library
***********************

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

The *table-extensions* module exports the class *<string-table>* ; the
type *<hash-state>* ; the generic function *remove-all-keys!* and two
methods thereon; and the functions *collection-hash*, *sequence-hash*,
*string-hash*, *values-hash*, *case-insensitive-string-hash*, and
*case-insensitive-equal*.

The *<string-table>* class is a class of tables that use strings for
keys.

The *<hash-state>* type implements *hash states*. A hash state is
defined by the DRM, page 123, as “an implementation-dependent type that
is associated with a hash id and can be used by the implementation to
determine whether the hash id has been invalidated.” See pages 122–123
of the DRM for more details.

The various hash functions and the *case-insensitive-equal* equivalence
predicate are convenient building blocks for creating new table classes
and hash functions.

Hash functions
--------------

Different hash functions are not required to return the same hash code
for equal or even identical objects. For instance,

collection-hash(#(), object-hash, object-hash);

is not guaranteed to return the same values as

sequence-hash(#(), object-hash);

Furthermore, *collection-hash* with *ordered: #t* is not guaranteed to
return the same hash code as *collection-hash* with *ordered: #f*. Such
a requirement would render the *ordered:* keyword useless.

Weak tables
-----------

Common Dylan allows all general instances of the built-in class
*<table>* to be *weak*. See :ref:`weak tables <weak-tables>` of
this volume for information about weakness.

You can create weak tables with the *<table>* class’s *weak:*
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

<string-table>
--------------

Sealed class

Summary

A table class that uses strings for keys.

Superclasses

<table>

Init-keywords

See Superclasses.

Description

The *<string-table>* class is the class of tables that use instances of
*<string>* for their keys. It is an error to use a key that is not an
instance of *<string>*.

Keys are compared with the equivalence predicate *\\=*.

The elements of the table are instances of *<object>*.

It is an error to modify a key once it has been used to add an element
to a *<string-table>*. The effects of modification are not defined.

<hash-state>
------------

Type

Summary

A hash state.

Supertypes

<object>

Init-keywords

None.

Description

Anything that the Dylan Reference Manual describes as a *hash state* is
an instance of this type.

Examples of hash states include the second argument and second return
value of *object-hash*.

collection-hash
---------------

Function

Summary

Hashes the elements of a collection.

Signature

collection-hash *key-hash-function* *elt-hash-function* *collection
initial-state* #key *ordered* => *hash-id* *hash-state*

Arguments

-  *key-hash-function* An instance of *<function>*.
-  *elt-hash-function* An instance of *<function>*.
-  *collection* An instance of *<collection>*.
-  *initial-state* An instance of *<hash-state>*.
-  *ordered* An instance of *<boolean>*. Default value: *#f*.

Values

-  *hash-id* An instance of *<integer>*.
-  *result-state* An instance of *<hash-state>*.

Description

Hashes every element of *collection* using *key-hash-function* on the
keys and *elt-hash-function* on the elements, and merges the resulting
hash codes in order.

The *ordered* keyword is passed on to *merge-hash-ids*.

The functions *key-hash-function* and *elt-hash-function* must be
suitable for use as hash functions. See page 123 of the DRM.

sequence-hash
-------------

Function

Summary

Hashes the elements of a sequence.

Signature

sequence-hash *elt-hash-function* *sequence* *initial-state*
 #key *ordered* => *hash-id* *result-state*

Arguments

-  *elt-hash-function* An instance of *<function>*.
-  *sequence* An instance of *<sequence>*.
-  *initial-state* An instance of *<hash-state>*.

Values

-  *hash-id* An instance of *<integer>*.
-  *result-state* An instance of *<hash-state>*.

Description

Hashes every element of *sequence* using *elt-hash-function*, and
merges the resulting hash codes in order.

The function *elt-hash-function* must be suitable for use as a hash
function. See page 123 of the Dylan Reference Manual.

The *ordered* keyword is passed on to *merge-hash-ids*.

values-hash
-----------

Function

Summary

Hashes the values passed to it.

Signature

values-hash *elt-hash-function* *initial-state* #rest *arguments* =>
*hash-id* *result-state*

Arguments

-  *elt-hash-function* An instance of *<function>*.
-  *hash-state* An instance of *<hash-state>*.
-  *arguments* Instances of *<object>*.
-  *initial-state* An instance of *<hash-state>*.

Values

-  *hash-id* An instance of *<integer>*.
-  *result-state* An instance of *<hash-state>*.

Description

Hashes every object in *arguments* using *elt-hash-function*, and
merges the resulting hash codes in order.

The function *elt-hash-function* must be suitable for use as a hash
function. See page 123 of the Dylan Reference Manual.

The *ordered* keyword is passed on to *merge-hash-ids*.

string-hash
-----------

Function

Summary

Hashes a string.

Signature

string-hash *string* *initial-state* => *hash-id* *result-state*

Arguments

-  *string* An instance of *<string>*.
-  *initial-state* An instance of *<hash-state>*.

Values

-  *hash-id* An instance of *<integer>*.
-  *result-state* An instance of *<hash-state>*.

Description

Produces a hash code for a string, using the equivalence predicate *\\=*.

case-insensitive-string-hash
----------------------------

Function

Summary

Hashes a string, without considering case information.

Signature

case-insensitive-string-hash *string* *initial-state* => *hash-id*
*result-state*

Arguments

-  *string* An instance of *<string>*.
-  *initial-state* An instance of *<hash-state>*.

Values

-  *hash-id* An instance of *<integer>*.
-  *result-state* An instance of *<hash-state>*.

Description

Produces a hash code for a string using the equivalence predicate
*case-insensitive-equal*, which does not consider the case of the
characters in the strings it compares.

See also

`case-insensitive-equal`_

case-insensitive-equal
----------------------

Function

Summary

Compares two strings for equality, ignoring case differences between
them.

Signature

case-insensitive-equal *string1* *string2* => *boolean*

Arguments

-  *string1* An instance of *<string>*.
-  *string2* An instance of *<string>*.

Values

-  *boolean* An instance of *<boolean>*.

Description

Compares *string1* and *string2* for equality, ignoring any case
differences between them. Returns true if they are equal and false
otherwise.

The function has the same behavior as Dylan’s standard method on *=* for
sequences, except that when comparing alphabetical characters, it
ignores any case differences.

This function is used as an equivalence predicate by
`case-insensitive-string-hash`_.

This function uses *as-uppercase* or *as-lowercase* to convert the
characters in its string arguments.

Example

The *case-insensitive-equal* function returns true if passed the
following strings:

"The Cat SAT ON the Mat"

"The cat sat on the Mat"

Conversely, the standard method on *=* returns false when passed those
strings.

See also

`case-insensitive-string-hash`_

remove-all-keys!
----------------

Open generic function

Summary

Removes all keys from a collection and leaves it empty.

Signature

remove-all-keys! *collection* => *collection*

Arguments

-  *collection* An instance of *<mutable-explicit-key-collection>*.

Values

-  *collection* An instance of *<mutable-explicit-key-collection>*.

Description

Modifies *collection* by removing all its keys and elements, and leaves
it empty.

.. note:: To empty collections that are not instances of
   *<mutable-explicit-key-collection>*, use *size-setter*.

remove-all-keys!
----------------

G.f. method

Summary

Removes all keys from a collection and leaves it empty.

Signature

remove-all-keys! *collection* => *collection*

Arguments

-  *collection* An instance of *<mutable-explicit-key-collection>*.

Values

-  *collection* An instance of *<mutable-explicit-key-collection>*.

Description

Modifies *collection* by removing all its keys and elements, and leaves
it empty. This method implements the generic function by making repeated
calls to *remove-key!*.

.. note:: To empty collections that are not instances of
   *<mutable-explicit-key-collection>*, use *size-setter*.

remove-all-keys!
----------------

Sealed g.f. method

Summary

Removes all keys from a table and leaves it empty.

Signature

remove-all-keys! *table* => *table*

Arguments

-  *table* An instance of *<table>*.

Values

-  *table* An instance of *<table>*.

Description

Modifies *table* by removing all its keys and elements, and leaves it
empty.

This method does not use *remove-key!*.

.. note:: To empty collections that are not instances of
   *<mutable-explicit-key-collection>*, use *size-setter*.
