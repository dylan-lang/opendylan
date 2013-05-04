***************
Method Dispatch
***************

Method dispatch is a critical area for compile time and runtime optimization
in Dylan. Since nearly everything appears to involve a method dispatch including
slot access, it is very important that the compiler be able to optimize much
of this away.

Publications
============

One paper has been written about the implementation of dispatch in Open Dylan:

* **Partial Dispatch: Optimizing Dynamically-Dispatched Multimethod Calls with Compile-Time Types and Runtime Feedback** (by Jonathan Bachrach and Glenn Burke - Technical Report 2000 `pdf <http://people.csail.mit.edu/jrb/Projects/pd.pdf>`__)

Generic Function Representation
===============================

In Dylan, a generic function looks like (from
``sources/dfmc/modeling/functions.dylan``):

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

There are 2 subclasses of ``<generic-function>``: ``<sealed-generic-function>`` and
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

Compile Time Optimization
=========================

*Discuss the impact of sealing and other things here.*

Analysis
========

Performance Highlighting
------------------------

In the IDE, Open Dylan supports performance highlighting to indicate how
much optimization the compiler was able to apply.

This information is also available in ``.el`` files within the build
directory that can be used with the ``dylan-mode`` in emacs.

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

.. _sources/lib/dispatch-profiler: https://github.com/dylan-lang/opendylan/tree/master/sources/lib/dispatch-profiler

