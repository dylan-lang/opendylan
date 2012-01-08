*********************
Library Documentation
*********************

We are working on a tool to automatically generate skeletal documentation from
source code, but until then, we are documenting the Open Dylan libraries
manually using `Sphinx <http://sphinx.pocoo.org>`_ to build the HTML pages.
Sphinx uses reStructuredText markup with some extensions of its own, and we have
created additional extensions to document Dylan language entities.

The documentation — a number of RST files — is in the "documentation" directory
in the "opendylan" repository. Consult the Sphinx web-site for details about
reStructuredText markup and Sphinx extensions to it, and see the
"dylandomain/reference.rst" file in the "sphinx-extensions" repository for
details about the Dylan language extensions. (You may use the ``rst2html`` tool
to generate an HTML page from an .rst file.)


Example documentation
=====================

Here is an example of documentation to get a feel for how it works. Use the
"Show Source" link at the bottom of the page to see it in RST markup form.


Skip Lists
----------

A skip-list is a data type equivalent to a balanced (binary) tree.

.. library:: skip-list

   The skip list library may be found in the `"skip-list" repository
   <http://github.com/dylan-lang/skip-list>`_.
   
.. module:: skip-list

   The skip-list module exports a number of symbols. Two new symbols are of
   interest:

   - :class:`<skip-list>`
   - :gf:`element-sequence`

   The skip-list module also adds to several generic methods. One of the new
   methods is:

   - :meth:`element <element(<skip-list>)>`

.. class:: <skip-list>
   :open:
   :primary:
   
   A skip-list is a data type equivalent to a balanced (binary) tree. All keys
   must be comparable by some kind of ordering function, e.g., :drm:`<`.
   
   :superclasses: <stretchy-collection>, <mutable-explicit-key-collection>
   :keyword key-test:
      The collection's key-test function; should return ``#t`` if two keys
      should be considered equal. Defaults to :drm:`==`.
   :keyword key-order:
      A function that accepts two keys and returns ``#t`` if the first key
      sorts before the second. Defaults to :drm:`<`.
   :keyword size:
      Preallocates enough memory to hold this number of objects. Optional.
   :keyword capacity:
      Sets the maximum capacity of the skip list. Optional.
   :keyword probability:
      The probability to create a new level of the list. Equivalent to the
      fan-out of a tree. Defaults to 0.25.
   :keyword max-level:
      The list will not grow beyond this number of levels. Defaults to a value
      based on the ``size`` and ``capacity`` keywords.
   :keyword level:
      The list starts with this number of levels. Defaults to a value based on
      the ``size`` and ``capacity`` keywords.
      
   In general, a skip list operates like a stretchy mutable key collection. But
   a skip list can also act as an *ordered* stretchy mutable key collection
   where the iteration order is the key order. To take advantage of this, the
   library defines ``forward-by-key-iteration-protocol``,
   :gf:`element-sequence`, and ``element-sequence-setter``.
   
.. generic-function:: element-sequence
   
   :argument list:   A skip list.
   :value sequence:  An instance of :drm:`<sequence>`.
   
   One of the useful features of skip lists is that they can be ordered.
   However, most of the useful operations that can be performed on ordered
   collections, such as sort, are only defined for sequences. To solve this
   problem, I add ``element-sequence`` and ``element-sequence-setter``. The
   client may call the former to obtain a sequence, operate on it, and call the
   latter to fix the results in the skip list. The setter ensures that no
   elements have been added or removed from the skip list, only reordered.

.. method:: element
   :specializer: <skip-list>
   
   A specialization of :drm:`element`.
   
   :argument collection:    An instance of :class:`<skip-list>`.
   :argument key:           The key of an element. An instance of :drm:`<object>`.
   :argument #key default:  A value to return if the element is not found. If
                            omitted and element not found, signals an error.
   :value    object:        The element associated with the key.
