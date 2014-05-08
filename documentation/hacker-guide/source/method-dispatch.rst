***************
Method Dispatch
***************

Method dispatch is a critical area for compile time and runtime optimization
in Dylan. Since nearly everything appears to involve a method dispatch including
slot access, it is very important that the compiler be able to optimize much
of this away.

Method dispatch has support in both the compiler and the runtime. The runtime
portion also has data structures that are defined in the compiler (leading to
dependencies between the compiler and the runtime).

Runtime portions, including compiler support, can be found in these files:

* `sources/dylan/discrimination.dylan`_
* `sources/dylan/dispatch-caches.dylan`_
* `sources/dylan/dispatch-prologue.dylan`_
* `sources/dylan/dispatch.dylan`_
* `sources/dylan/new-dispatch.dylan`_
* `sources/dylan/slot-dispatch.dylan`_
* `sources/dfmc/modeling/functions.dylan`_

Compiler support for method dispatch optimization can be found in:

* `sources/dfmc/optimization/dispatch.dylan`_

Publications
============

One paper has been written about the implementation of dispatch in Open Dylan:

* **Partial Dispatch: Optimizing Dynamically-Dispatched Multimethod Calls with Compile-Time Types and Runtime Feedback** (by Jonathan Bachrach and Glenn Burke - Technical Report 2000 `pdf <http://people.csail.mit.edu/jrb/Projects/pd.pdf>`__)

Generic Function Representation
===============================

In Dylan, a generic function looks like (from
`sources/dfmc/modeling/functions.dylan`_):

.. code-block:: dylan

    define abstract primary &class <generic-function> (<function>)
      lazy &slot function-signature :: false-at-compile-time-or(<signature>),
        init-value: #f,
        init-keyword: signature:;
      &slot %gf-cache, init-value: #f;
      lazy &slot debug-name :: <object>,
        init-value:   #f,
        init-keyword: debug-name:;
      lazy &computed-slot generic-function-methods :: <list>,
        init-value: #();
      // If we start using this it should probably be made lazy, as it would
      // only be used for creating the runtime object, not compilation.
      &slot discriminator, init-value: #f;

      // Compile-time slots.
      slot ^generic-function-properties :: <integer>, init-value: 0;
      lazy slot signature-spec :: <signature-spec>,
        required-init-keyword: signature-spec:;
      lazy slot %generic-function-domains :: <list> = #();
      slot parameters-dynamic-extent,
            init-value: #f,
        init-keyword: dynamic-extent:;
      slot ^generic-function-cache-info = #f;
      metaclass <function-class>;
    end &class <generic-function>;

There are 2 subclasses of :drm:`<generic-function>`: ``<sealed-generic-function>`` and
``<incremental-generic-function>``.  A sealed generic function adds no slots, while
an incremental generic function maintains some extra data to support adding further
methods.

At the moment, we'll focus on sealed generic functions.

And in the generated C, a sealed generic function looks like:

.. code-block:: c

    typedef struct {
      D wrapper;
      D xep_;
      D function_signature_;
      D Pgf_cache_;
      D debug_name_;
      D generic_function_methods_;
      D discriminator_;
    } _KLsealed_generic_functionGVKe;

    _KLsealed_generic_functionGVKe Ksize_in_wordsVKi = {
      &KLsealed_generic_functionGVKeW,
      &gf_xep_1,
      &KDsignature_LobjectG_object_rest_value_1VKi,
      &KPfalseVKi,
      &K342,
      &K632,
      &RSINGULAR_Labsent_engine_nodeG
    };

Incremental generic functions look similar, but contain some additional data
after the discriminator.

The types in the generated C are just ``D`` which is what the Dylan
compiler's C back-end likes to generate. More specific types for some
values are available but not emitted by the C back-end. (Improvements
in this area are worth considering as they would improve the debugging
experience.)

Runtime Dispatch
================

Much of the technical report by Bachrach and Burke remains accurate with
respect to the basics of dispatch.

Discriminators at Runtime
-------------------------

The initial discriminator of a generic function is ``$absent-engine-node``
(or in C, ``RSINGULAR_absent_engine_node``). When this is encountered when
performing a dispatch, ``gf-dispatch-absent`` is invoked, which calls
``handle-missed-dispatch``. The initial dispatch engine state will then
be calculated in ``calculate-dispatch-engine`` and dispatch will proceed.

In this way, dispatch data is built incrementally at runtime as it is
needed and can take advantage of data available at runtime. In fact,
dispatch can start out being monomorphic and grow to linear and then
hash-based discriminators as the number of relevant methods changes
at runtime.

For example, when growing a linear discriminator (``grow-linear-class-keyed-discriminator``),
it can be upgraded to become a hashed discriminator.

The logic for creating a new discriminator starts in ``compute-discriminator-for-arg``
(defined in `sources/dylan/discrimination.dylan`_).

Discriminator Structure
-----------------------

The classes that dictate the in-memory layout of the discriminators are
defined within the compiler in `sources/dfmc/modeling/functions.dylan`_.

Of particular interest are the ``<linear-by-class-discriminator>`` and
``<hashed-by-class-discriminator>``. These, along with some variants
for dealing with singleton dispatch, define a repeated slot for storing
their data:

.. code-block:: dylan

    repeated &slot class-keyed-discriminator-table-element,
      init-value:        #f,
      size-getter:       class-keyed-discriminator-table-size,
      size-init-keyword: size:,
      size-init-value:   0;

For these discriminators, the keys and values are stored in alternating
sequence::

    key1, value1, key2, value2

This allows for a compact representation within memory without extra
allocations for pairs of values, a hash table, etc.

The code for iterating over this data can be found in the functions
``linear-class-key-lookup`` and ``hashed-class-key-lookup`` as found
within `sources/dylan/new-dispatch.dylan`_. That file also contains
the code for adding new methods to the discriminator.


Compile Time Optimization
=========================

*Discuss the impact of sealing and other things here.*

Analysis
========

Performance Highlighting
------------------------

The compiler records dispatch decisions as they're made within
the optimizer. This work is performed within `sources/dfmc/optimization/dispatch.dylan`_
(look for calls to ``color-dispatch``). It is worth noting
that the dispatch decisions are compacted by ``compact-coloring-info``
in `sources/dfmc/management/compilation-driver.dylan`_.

In the IDE, Open Dylan supports performance highlighting to indicate how
much optimization the compiler was able to apply. This is performed
within `sources/environment/deuce/dylanworks-mode.dylan`_ by examining
the results from ``source-record-colorization-info``.

This information is also available in ``.el`` files within the build
directory that can be used with the ``dylan-mode`` in emacs. The
generation of the ``.el`` files is performed by ``project-dump-emacs-dispatch-colors``
in `sources/project-manager/projects/implementation.dylan`_.

The available dispatch decisions that are recorded for highlighting
are:

* ``#"not-all-methods-known"``
* ``#"failed-to-select-where-all-known"``
* ``#"lambda-call"``
* ``#"inlining"``
* ``#"slot-accessor-fixed-offset"``
* ``#"eliminated"``
* ``#"dynamic-extent"``
* ``#"bogus-upgrade"``

*Link to documentation on both of these features, perhaps embed
some screenshots.*

Dispatch Profiler
-----------------

There is a dispatch profiler in `sources/lib/dispatch-profiler`_
but no one knows how to use it.

Future Work
===========

* Learn more about partial dispatch and possibly enable it.
* Look at the effectiveness of call site caching.
* Can the hashing in the megamorphic hashed by-class discriminator
  be tuned better?
* Learn more about and document things mentioned in this document
  but that aren't understood well (like dispatch profiling).
* Much more documentation.

.. _sources/dylan/discrimination.dylan: https://github.com/dylan-lang/opendylan/tree/master/sources/dylan/discrimination.dylan
.. _sources/dylan/dispatch-caches.dylan: https://github.com/dylan-lang/opendylan/tree/master/sources/dylan/dispatch-caches.dylan
.. _sources/dylan/dispatch-prologue.dylan: https://github.com/dylan-lang/opendylan/tree/master/sources/dylan/dispatch-prologue.dylan
.. _sources/dylan/dispatch.dylan: https://github.com/dylan-lang/opendylan/tree/master/sources/dylan/dispatch.dylan
.. _sources/dylan/new-dispatch.dylan: https://github.com/dylan-lang/opendylan/tree/master/sources/dylan/new-dispatch.dylan
.. _sources/dylan/slot-dispatch.dylan: https://github.com/dylan-lang/opendylan/tree/master/sources/dylan/slot-dispatch.dylan
.. _sources/dfmc/modeling/functions.dylan: https://github.com/dylan-lang/opendylan/tree/master/sources/dfmc/modeling/functions.dylan
.. _sources/dfmc/optimization/dispatch.dylan: https://github.com/dylan-lang/opendylan/tree/master/sources/dfmc/optimization/dispatch.dylan
.. _sources/dfmc/management/compilation-driver.dylan: https://github.com/dylan-lang/opendylan/tree/master/sources/dfmc/management/compilation-driver.dylan
.. _sources/environment/deuce/dylanworks-mode.dylan: https://github.com/dylan-lang/opendylan/tree/master/sources/environment/deuce/dylanworks-mode.dylan
.. _sources/project-manager/projects/implementation.dylan: https://github.com/dylan-lang/opendylan/tree/master/sources/project-manager/projects/implementation.dylan
.. _sources/lib/dispatch-profiler: https://github.com/dylan-lang/opendylan/tree/master/sources/lib/dispatch-profiler
