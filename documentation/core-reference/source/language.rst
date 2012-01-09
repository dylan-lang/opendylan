*************************
Dylan Language Extensions
*************************

Introduction
============

The Dylan language is described in *The Dylan Reference Manual* by
Andrew Shalit (Addison-Wesley, 1996). We call this book “the DRM”
hereafter.

Open Dylan provides an implementation of the Dylan language
described by the DRM, with a few exceptions that are documented in
`Language differences`_ of this chapter.

Open Dylan provides the Dylan language in the *dylan* module of the
*dylan* library.

As of the 2.0 release, Harlequin Dylan has joined forces with the other
major implementor of Dylan, the Gwydion Dylan Development Cooperative,
to provide a set of common extensions to the Dylan language (as defined
by the DRM). These extensions are portable across implementations and
are either built in to the *dylan* library or are available in a
separate library, *common-extensions*. This chapter is an introduction
to the language extensions.

Using Open Dylan’s language extensions
======================================

There are a number of ways to use Open Dylan’s language extensions
in your applications.

A few extensions have become part of the *dylan* library. You can read
about these extensions in `The core of the common extensions`_.

The majority of the extensions are in the *common-extensions* module of
the *common-extensions* library. That library also exports a number of
smaller modules that contain other basic facilities such as simplified
formatting (*simple-format*), pseudo-random integer generation
(*simple-random*), and object finalization (*finalization*).

Open Dylan provides a convenience library, *common-dylan*, that
combines the *dylan* and *common-extensions* libraries to provide a
convenient “dialect” of Dylan, exported from the module *common-dylan* :

.. code-block:: dylan

    define library common-dylan
      use dylan, export: all;
      use common-extensions, export: all;

      export common-dylan;
    end module;

    define module common-dylan
      use dylan, export: all;
      use common-extensions, export: all;
    end module;

The core of the common extensions
=================================

This section describes the common language extensions, that is,
extensions made to the Dylan library as it is defined in DRM. These
extensions are available to applications in the *dylan* library’s
*dylan* module.

All the other language extensions are described in :doc:`extensions`.

Function Definition
-------------------

The *define function* definition macro provides a convenient way to
define functions that have no generic properties and hence are not
suitable for definition with *define generic* or *define method*.
This extension has been accepted as part of the language since the DRM
was published.

The *define function* macro provides a way of defining a function that
says clearly to other programmers that the function is not part of any
generic operation; furthermore, the function will not be extended as a
generic function, and calling it need not involve any generic dispatch.
Without this macro, programmers who wanted to do so would have to turn
to *define constant*. With *define function*, programmer intent is
more explicit and it relays more information to future maintainers of a
piece of code.

The language definition of *define function* explicitly *does not*
specify what it expands into, so that Dylan implementations have
latitude to support this definer in the best way suited to the
implementation.

define function
---------------

Definition macro

Summary

Defines a constant binding in the current module and initializes it to a
new function.

Macro call

define {*adjective* }\* function *name* *parameter-list*
[ *body* ]
end [ function ] [ *name* ]

Arguments

-  *adjective* A Dylan unreserved-name *bnf*.
-  *name* A Dylan variable-name *bnf*.
-  *parameter-list* A Dylan parameter-list *bnf*.
-  *body* A Dylan body *bnf*.

Description

Creates a constant module binding with the name *name*, and initializes
it to a new function described by *parameter-list*, *options*, and any
adjectives.

The adjectives permitted depend on the implementation.

The *parameter-list* describes the number and types of the function’s
arguments and return values. It is an error to supply *#next* in the
parameter list, and there is no implicit *#next* parameter.

Operations

The following functions return the same values as they would if the
function had been defined as a bare method with the same signature:

function-specializers

function-arguments

function-return-values

Calling some of the following reflective operations on a function
defined with *define function* may be an error:

generic-function-methods

add-method

generic-function-mandatory-keywords

sorted-applicable-methods

find-method

remove-method

applicable-method?

Extensions to the FOR iteration construct
-----------------------------------------

We have also made two extensions to the *for* iteration construct: a
*keyed-by* clause and *in* … *using* clauses.

The *keyed-by* clause allows iteration over table elements:

.. code-block:: dylan

    for (my-element keyed-by my-key in my-table)
      ...
    end;

The *in* … *using* clause allows you to specify a iteration protocol
other than the default (*forward-iteration-protocol*):

.. code-block:: dylan

    for (element in my-sequence using backward-iteration-protocol)
      ...
    end;

.. _weak-tables:

Weak tables
-----------

We have extended *define table* to incorporate *weak references*
through keys and values.

A weak reference is an reference that the garbage collector treats as
irrelevant to establishing whether the object referred to is live. If an
object has only weak references to it, the garbage collector can delete
the reference and recycle the object’s memory. We call a normal
reference a *strong reference*.

Weak references are a useful tool for building data structures where you
do not want the garbage collector to preserve objects in the structure
on account of certain references merely used to build up the structure.

Typically, this level of control is not required in a language like
Dylan, which does not expose memory references to programs. But without
the ability to tell the garbage collector to disregard certain kinds of
reference, data structures such as tables could be bloated unnecessarily
by the garbage collector preserving entries (a key/value pair) solely
because the table object itself has a reference to the entry’s key or
value.

Common Dylan provides weakness options for instances of ``<table>``. A
table can have *weak keys* or *weak values*:

.. code-block:: dylan

    make(<table>, weak: #"key"); // makes a weak-key table

    make(<table>, weak: #"value"); // makes a weak-value table

In a weak-keyed table, if a key is no longer referenced from anywhere
else in the program (apart from weak references, including from the same
table), then the entry (key and value) can be deleted from the table.
After that, the key object will be recycled. The value will also be
recycled unless it has strong references from elsewhere in the program.

Weak-valued tables are much the same, except that the focus is values
and not keys. In a weak-valued table, if a value is no longer referenced
from anywhere else in the program (apart from weak references, including
from the same table), then the entry (value and key) can be deleted from
the table. After that, the value object will be recycled. The key will
also be recycled unless it has strong references from elsewhere in the
program.

Weak tables are useful for implementing many sorts of cache, where the
cached data is recomputable and yet both expensive to compute and also
expensive to keep for a long time. For example, consider something like
a font cache for an X Window System server, or a printer. Fonts might be
looked up by name, so the strings would be the keys of the table. The
values would be the bitmaps for the font. While the X server is using a
font, the cache will be kept alive — so any further requests to select
the font will find the data already present. However, if the font is not
used then you would eventually expect the garbage collector to clean it
out. Any future request would then have to re-load all the bitmaps.

Inlining adjectives for methods, constants, functions, and slots
----------------------------------------------------------------

To *inline* a value is to replace, at compile time, a reference to a
variable with the value of that variable. Such inlining often allows
compile-time evaluation (“constant folding”) or partial evaluation.

The Open Dylan compiler can perform inlining on generic function
methods, constants, class slots, and functions (created with *define
function* —see `Function Definition`_). We have extended the Dylan language
specification of *define method*, *define constant*, and class slots with
inlining definition adjectives and have included those same adjectives in
our language extension *define function*. The adjectives are:

- *not-inline* Never inline this item.
- *default-inline* (default)
  Inline this item within a library, at the compiler’s discretion. Never
  inline a cross-library reference.
- *may-inline* Inline this item within or between libraries, at the
  compiler’s discretion.
- *inline* Inline this item wherever the compiler can do so.

In addition, *define constant* and *define function* permit the
adjective *inline-only*, which forces every reference to the constant
or function to be inlined.

.. note:: If you export from a library any variables created with
   *may-inline*, *inline*, or *inline-only*, and then change the values
   of the variables, client libraries may need to be recompiled.

.. _language-differences:

Language differences
====================

Tables
------

For efficiency, Common Dylan adopts a slightly different table protocol
to that described by the DRM. Hashing functions take an additional
hash-state argument and merge it into the hash-state result. The
function *merge-hash-codes* is replaced by *merge-hash-ids* because
hash-states are merged as part of the hashing process. The constant
*$permanent-hash-state* is no longer required; the same effect can be
achieved by returning the argument *hash-state* unchanged as the result
*hash-state*. Finally, *object-hash* has been altered to use the new
protocol.

This section describes the items that have been changed. We also provide
a Table-extensions module, which you can read about in
:doc:`table-extensions`.

table-protocol
--------------

Open generic function

Summary

Returns functions used to implement the iteration protocol for tables.

Signature

.. code-block:: dylan

    table-protocol *table* => *test-function* *hash-function*

Arguments

-  *table* An instance of ``<table>``.

Values

- *test-function* An instance of ``<function>``.
- *hash-function* An instance of ``<function>``.

Library

dylan

Module

dylan

Description

Returns the functions used to iterate over tables. These functions are
in turn used to implement the other collection operations on ``<table>``.

The *test-function* argument is for the table test function, which is
used to compare table keys. It returns true if, according to the table’s
equivalence predicate, the keys are members of the same equivalence
class. Its signature must be:

test-function *key1* *key2* => *boolean*

The *hash-function* argument is for the table hash function, which
computes the hash code of a key. Its signature must be:

hash-function *key* *initial-state* => *id* *result-state*

In this signature, *initial-state* is an instance of ``<hash-state>``.
The hash function computes the hash code of *key*, using the hash
function that is associated with the table’s equivalence predicate. The
hash code is returned as two values: an integer *id* and a hash-state
*result-state*. This *result-state* is obtained by merging the
*initial-state* with the hash-state that results from hashing *key*.
The *result-state* may or may not be == to *initial-state*. The
*initial-state* could be modified by this operation.

merge-hash-ids
--------------

Function

Summary

Returns a hash ID created by merging two hash IDs.

Signature

.. code-block:: dylan

    merge-hash-ids *id1* *id2* #key *ordered* => *merged-id*

Arguments

- *id1* An instance of ``<integer>``.
- *id2* An instance of ``<integer>``.
- *ordered* An instance of ``<boolean>``. Default value: *#f*.

Values

-  *merged-id* An instance of ``<integer>``.

Description

Computes a new hash ID by merging the argument hash IDs in some
implementation-dependent way. This can be used, for example, to generate
a hash ID for an object by combining hash IDs of some of its parts.

The *id1*, *id2* arguments and the return value *merged-id* are all
integers.

The *ordered* argument is a boolean, and determines whether the
algorithm used to the merge the IDs is permitted to be order-dependent.
If false (the default), the merged result must be independent of the
order in which the arguments are provided. If true, the order of the
arguments matters because the algorithm used need not be either
commutative or associative. It is best to provide a true value for
*ordered* when possible, as this may result in a better distribution of
hash IDs. However, *ordered* must only be true if that will not cause
the hash function to violate the second constraint on hash functions,
described on page 123 of the DRM.

object-hash
-----------

Function

Summary

The hash function for the equivalence predicate ==.

Signature

.. code-block:: dylan

    object-hash *object* *initial-state* => *hash-id* *result-state*

Arguments

- *object* An instance of ``<integer>``.
- *initial-state* An instance of ``<hash-state>``.

Values

- *hash-id* An instance of ``<integer>``.
- *result-state* An instance of ``<hash-state>``.

Description

Returns a hash code for *object* that corresponds to the equivalence
predicate ==.

This function is a useful tool for writing hash functions in which the
object identity of some component of a key is to be used in computing
the hash code.

It returns a hash ID (an integer) and the result of merging the initial
state with the associated hash state for the object, computed in some
implementation-dependent manner.
