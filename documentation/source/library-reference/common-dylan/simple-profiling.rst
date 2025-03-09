***************************
The simple-profiling Module
***************************

.. current-library:: common-dylan
.. current-module:: simple-profiling

.. module:: simple-profiling

This module provides an easy to use interface for measuring application
performance.

Simple Profiling
================

.. macro:: timing
   :statement:

   Returns the time, in seconds and microseconds, spent executing the body
   of code it is wrapped around.

   :macrocall:
     .. parsed-literal:: 

        timing () [ `body` ] end [ timing ]

   :parameter body: A Dylan body *bnf*
   :value seconds: An instance of :drm:`<integer>`.
   :value microseconds: An instance of :drm:`<integer>`.

   :description:

     Returns the time, in seconds and microseconds, spent executing the
     body of code it is wrapped around.

     The first value returned is the number of whole seconds spent in
     *body*. The second value returned is the number of microseconds
     spent in *body* in addition to *seconds*.

   :example:

     .. code-block:: dylan

       timing ()
         for (i from 0 to 200)
           format-to-string("%d %d", i, i + 1)
         end
       end;
       => 1 671000

.. macro:: profiling

   Gives access to the CPU time, in seconds and microseconds, as well as
   some memory allocation statistics, spent executing the body of code
   it is wrapped around.

   :macrocall:
     .. parsed-literal:: 

        profiling ([`profiling-type`, ...])
          `body`
        results
          `results`
        end

   :parameter profiling-type: Any of ``cpu-time-seconds``,
     ``cpu-time-microseconds``, ``allocation`` and
     ``allocation-stats``.
   :parameter body: A Dylan body *bnf*
   :parameter results: A Dylan body *bnf*

   :description:

     This macro takes a set of *profiling-type* parameters, performs
     the *body* of code and then executes the *results*. Within the
     results block, there will be bindings with the names of the profiling
     types which have the corresponding value.

     .. note:: Using ``allocation-stats`` is more involved and not
        as flexible as one might hope. This needs further documentation
        and perhaps an improved implementation.

     .. note:: The memory allocation statistics may not work on all
        run-times and platforms.

   :example:

     .. code-block:: dylan

        profiling (cpu-time-seconds, cpu-time-microseconds, allocation)
          execute-command(command)
        results
          message(context, "Command took %d.%s seconds, and allocated %d bytes",
                  cpu-time-seconds,
                  integer-to-string(floor/(cpu-time-microseconds, 1000), size: 3),
                  allocation)
        end

Internals
=========

These functions don't typically need to be called directly but
may be useful in some scenarios.

.. type:: <profiling-state>

   :equivalent: :drm:`<object-table>`

.. type:: <cpu-profiling-type>

   :equivalent: ``one-of(#"cpu-time-seconds", #"cpu-time-microseconds")``

.. generic-function:: profiling-type-result
   :open:

   :signature: profiling-type-result (state keyword #key #all-keys) => (value)

   :parameter state: An instance of :type:`<profiling-state>`.
   :parameter keyword: An instance of :drm:`<symbol>`.
   :value value: An instance of :drm:`<object>`.

.. method:: profiling-type-result
   :specializer: <profiling-state>, <cpu-profiling-type>
   :no-contents-entry:

.. method:: profiling-type-result
   :specializer: <profiling-state>, singleton(#"allocation")
   :no-contents-entry:

.. method:: profiling-type-result
   :specializer: <profiling-state>, singleton(#"allocation-stats")
   :no-contents-entry:

.. function:: start-profiling

   :signature: start-profiling (profiling-types) => (state)

   :parameter profiling-types: A sequence of any of ``#"cpu-time-seconds"``,
     ``#"cpu-time-microseconds"``, ``#"allocation#`` and
     ``#"allocation-stats#``.
   :value state: An instance of :type:`<profiling-state>`.

   This is useful for when direct control over profiling is needed rather
   than using the :macro:`profiling` macro.

.. generic-function:: start-profiling-type
   :open:

   :signature: start-profiling-type (state keyword) => ()

   :parameter state: An instance of :type:`<profiling-state>`.
   :parameter keyword: An instance of :drm:`<symbol>`.

.. method:: start-profiling-type
   :specializer: <profiling-state>, <cpu-profiling-type>
   :no-contents-entry:

.. method:: start-profiling-type
   :specializer: <profiling-state>, singleton(#"allocation")
   :no-contents-entry:

.. method:: start-profiling-type
   :specializer: <profiling-state>, singleton(#"allocation-stats")
   :no-contents-entry:

.. function:: stop-profiling

   :signature: stop-profiling (state profiling-types) => ()

   :parameter state: An instance of :type:`<profiling-state>`.
   :parameter profiling-types: A sequence of :drm:`<symbol>`. These
     symbols should be the same as those passed to :func:`start-profiling`.

.. generic-function:: stop-profiling-type
   :open:

   :signature: stop-profiling-type (state keyword) => ()

   :parameter state: An instance of :type:`<profiling-state>`.
   :parameter keyword: An instance of :drm:`<symbol>`.

.. method:: stop-profiling-type
   :specializer: <profiling-state>, <cpu-profiling-type>
   :no-contents-entry:

.. method:: stop-profiling-type
   :specializer: <profiling-state>, singleton(#"allocation")
   :no-contents-entry:

.. method:: stop-profiling-type
   :specializer: <profiling-state>, singleton(#"allocation-stats")
   :no-contents-entry:
