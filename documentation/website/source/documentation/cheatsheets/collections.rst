********************
Collections in Dylan
********************

Common Collection Types
=======================

+--------------------------+------------------------------------+----------------------------------------+
| Class                    | Location                           | Description                            |
+==========================+====================================+========================================+
| :drm:`<array>`           | ``dylan:dylan``                    | Multi-dimensional data, much like a    |
|                          |                                    | matrix.                                |
+--------------------------+------------------------------------+----------------------------------------+
| :drm:`<vector>`          | ``dylan:dylan``                    | A one-dimensional array. Vectors       |
|                          |                                    | provide fast random access.            |
+--------------------------+------------------------------------+----------------------------------------+
| :drm:`<stretchy-vector>` | ``dylan:dylan``                    | A vector that can have elements        |
|                          |                                    | directly added and removed without     |
|                          |                                    | requiring a complete copy.             |
+--------------------------+------------------------------------+----------------------------------------+
| :drm:`<list>`            | ``dylan:dylan``                    | A linked list.                         |
+--------------------------+------------------------------------+----------------------------------------+
| :drm:`<deque>`           | ``dylan:dylan``                    | A double-ended queue.                  |
+--------------------------+------------------------------------+----------------------------------------+
| `\<set>`_                | ``collections:set``                | A set is for efficiently tracking      |
|                          |                                    | membership.                            |
+--------------------------+------------------------------------+----------------------------------------+
| `\<bit-set>`_            | ``collections:bit-set``            | A set with bits for members.           |
+--------------------------+------------------------------------+----------------------------------------+
| `\<bit-vector>`_         | ``collections:bit-vector``         | A vector optimized for storing bit     |
|                          |                                    | values.                                |
+--------------------------+------------------------------------+----------------------------------------+
| `\<byte-vector>`_        | ``collections:byte-vector``        | A vector of bytes.                     |
+--------------------------+------------------------------------+----------------------------------------+
| :drm:`<table>`           | ``dylan:dylan``                    | A mapping between keys and values.     |
+--------------------------+------------------------------------+----------------------------------------+
| `\<string-table>`_       | ``collections:table-extensions``   | A ``<table>`` with strings for keys.   |
+--------------------------+------------------------------------+----------------------------------------+

.. note::

   Because of the default comparison function for ``<table>``, it is
   important to use ``<string-table>`` when the keys will be strings.

Mutable vs. Immutable
=====================

Many operations don't modify the collection passed in. The exception
is when you use an operation ending in ``!`` on a *stretchy* collection.

For this reason, if you want to modify a collection frequently, be sure
to pay attention to the type of collection that you're working with.

In many cases, you will need to assign the result of an operation
back to the source collection:

.. code-block:: dylan

    threads := add(threads, thread);

In cases where you are modifying a collection frequently, you may want
to consider using a :drm:`<stretchy-vector>` rather than a `<vector>`
or some other type of collection.

Common Operations
=================

+---------------------+------------------------------+------------------------------------------------------+
| Class               | Operation                    | Summary                                              |
+=====================+==============================+======================================================+
| :drm:`<collection>` | :drm:`any?`                  | Returns the first true value obtained by iterating   |
|                     |                              | over one or more collections.                        |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`do`                    | Iterates over one or more collections for side       |
|                     |                              | effect.                                              |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`empty?`                | Returns true if its argument is empty.               |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`every?`                | Returns true if a predicate returns true when        |
|                     |                              | applied to all corresponding elements of a set of    |
|                     |                              | collections.                                         |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`fill!`                 | Fills a collection with a specified value.           |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`find-key`              | Returns the key in a collection such that the        |
|                     |                              | corresponding collection element satisfies a         |
|                     |                              | predicate.                                           |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`map`                   | Iterates over one or more collections and collects   |
|                     |                              | the results in a freshly allocated collection.       |
|                     |                              | See also :drm:`map-as` and :drm:`map-into`.          |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`member?`               | Returns true if a collection contains a particular   |
|                     |                              | value.                                               |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`reduce`                | Combines the elements of a collection and a seed     |
|                     |                              | value into a single value by repeatedly applying a   |
|                     |                              | binary function. See also :drm:`reduce1`.            |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`replace-elements!`     | Replaces those collection elements that satisfy a    |
|                     |                              | predicate.                                           |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`size`                  | Returns the size of its argument.                    |
+---------------------+------------------------------+------------------------------------------------------+
| :drm:`<sequence>`   | :drm:`add` / :drm:`add!`     | Adds an element to a sequence.                       |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`add-new` /             | Adds a new element to a sequence.                    |
|                     | :drm:`add-new!`              |                                                      |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`choose`                | Returns those elements of a sequence that satisfy a  |
|                     |                              | predicate. See also :drm:`choose-by`.                |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`concatenate`           | Returns the concatenation of one or more sequences   |
|                     |                              | in a sequence of a type determined by the            |
|                     |                              | :drm:`type-for-copy` of its first argument.          |
|                     |                              | See also :drm:`concatenate-as`.                      |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`copy-sequence`         | Returns a freshly allocated copy of some subsequence |
|                     |                              | of a sequence.                                       |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`intersection`          | Returns the intersection of two sequences.           |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`remove` /              | Removes an element from a sequence.                  |
|                     | :drm:`remove!`               |                                                      |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`remove-duplicates`     | Returns a sequence without duplicates.               |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`replace-subsequence!`  | Replaces a portion of a sequence with the elements   |
|                     |                              | of another sequence.                                 |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`reverse` /             | Returns a sequence with elements in the reverse      |
|                     | :drm:`reverse!`              | order of its argument sequence.                      |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`sort` / :drm:`sort!`   | Returns a sequence containing the elements of its    |
|                     |                              | argument sequence, sorted.                           |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`subsequence-position`  | Returns the position where a pattern appears in a    |
|                     |                              | sequence.                                            |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`union`                 | Returns the union of two sequences.                  |
+---------------------+------------------------------+------------------------------------------------------+
| :drm:`<list>`       | :drm:`head`                  | Returns the head of a list.                          |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`tail`                  | Returns the tail of a list.                          |
+---------------------+------------------------------+------------------------------------------------------+
| :drm:`<deque>`      | :drm:`push`                  | Adds an element to the front of a deque.             |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`pop`                   | Removes and returns the first element of a deque.    |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`push-last`             | Adds an element to the end of a deque.               |
|                     +------------------------------+------------------------------------------------------+
|                     | :drm:`pop-last`              | Removes and returns an element from the end of a     |
|                     |                              | deque.                                               |
+---------------------+------------------------------+------------------------------------------------------+
| :drm:`<table>`      | :drm:`remove-key!`           | Modifies an explicit key collection so it no longer  |
|                     |                              | has a particular key.                                |
+---------------------+------------------------------+------------------------------------------------------+

.. _<set>: http://opendylan.org/documentation/library-reference/collections/set.html
.. _<bit-set>: http://opendylan.org/documentation/library-reference/collections/bit-set.html
.. _<bit-vector>: http://opendylan.org/documentation/library-reference/collections/bit-vector.html
.. _<byte-vector>: http://opendylan.org/documentation/library-reference/collections/byte-vector.html
.. _<string-table>: http://opendylan.org/documentation/library-reference/collections/table-extensions.html#collections:table-extensions:[string-table]
