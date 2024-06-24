*****************************
The DISPATCH-PROFILER library
*****************************

.. current-library:: dispatch-profiler

The dispatch profiler library exposes the multimethod dispatch
profiling capability built in to the Open Dylan run-time library,
making it possible to identify costly function calls in code. While
*dispatch coloring* can identify calls that could and could not be
optimized at compile time, dispatch profiling shows which calls have
an actual effect on dynamic performance.

While this library can be quite useful, it was originally built as
part of prototyping the dispatch mechanisms in Open Dylan, rather than
as a polished tool. We hope that in the future this functionality can
be incorporated into the debugger, making direct use of this library
unnecessary.

Preparing for Profiling
***********************

Before dispatch profiling can be enabled during program execution, you
will need to request that the compiler generate generic function call
data structures (with slightly higher overhead) that are capable of
collecting profile counts. This can be done by setting the
:envvar:`OPEN_DYLAN_PROFILE_ALL_CALLS` environment variable to a
non-empty string when invoking the compiler:

.. code-block:: console

  $ OPEN_DYLAN_PROFILE_ALL_CALLS=1 dylan-compiler -build write-100mb


Report Output Format
********************

After a profiling run, upon collection of dispatch statistics a
profiling report can be written out. The report, sorted by ``COST``
field, lists generic functions and call sites for which any method
invocations are present in the dispatch cache. The following fields
appear in the report:

``GENERICS``
  The count of generic functions in an application or library
  appearing in the report.

``CLASSES``
  The total number of classes appearing in the application.

``DEP-GFS``
  Not currently supported (based on an analysis currently commented out
  in the compiler).

``CT-S-CALLS``
  Count of static calls (i.e., to a specific known method or slot accessor)
  as determined by the compiler for a library or application.

``CT-D-CALLS``
  Count of dynamic calls (i.e., using full run-time dispatch through the
  :drm:`<generic-function>` object) as determined by the compiler for
  a library or application.

``S/D``
  The ratio of (compile-time) static to dynamic calls.

``RT-S-CALLS``
  The number of call sites in the library or application where a
  method was called but the ``COST`` was 0.

``E-RT-S/D``
  The ratio of the total number of static calls (``RT-S-CALLS`` plus
  ``CT-S-CALLS``) to the total number of call sites identified by the
  compiler (``CT-S-CALLS`` plus ``CT-D-CALLS``).

``POLY``
  The degree of polymorphism. For a given call site, represents the
  number of distinct methods called. For a generic function, library,
  or application, represents the total call site polymorphism divided
  by the number of call sites.

``TOT SIZE``
  The memory storage size (in units of pointer-sized object slots)
  used by method dispatch cache nodes.

``AVG SIZE``
  The average memory storage size (in units of pointer-sized object
  slots) of each associated method dispatch cache node.

``HITS``
  The number of times that the dispatched method was found in the
  method dispatch cache.

``COST``
  The total number of discriminator nodes traversed in order to locate
  the dispatched method in the cache. Useful as an approximation for the
  cost of method dispatch for a given call site or generic function.

``COST/HIT``
  The average number of discriminator nodes that needed to be
  traversed in order to dispatch to the given method.

``C-HITS``
  The total number of discriminator nodes traversed with successful
  discrimination tests.

``C-TRIES``
  The total number of discriminator nodes traversed (identical to
  ``COST``).

``HIT-RATE``
  The ratio of ``C-HITS`` to ``C-TRIES``.

The DISPATCH-PROFILER module
****************************

.. current-module:: dispatch-profiler

.. macro:: with-dispatch-profiling-report
   :statement:

   Performs dispatch profiling over a body of code and prints out a
   profiling report.

   :macrocall:
     .. parsed-literal::
        with-dispatch-profiling-report (#key `keys`)
          `body`
        end

   :param keys: Zero or more of the keywords provided by :gf:`print-dispatch-statistics`.
   :param body: A body of Dylan code.

   :description:

     Executes the *body* with dispatch profiling enabled. Clears
     dispatch profiling counters before beginning execution, disables
     profiling and collects statistics at the end, and then writes out
     a report using :gf:`print-dispatch-statistics`.

   :example:

     .. code-block:: dylan

       define function main()
         with-dispatch-profiling-report (by-library?: #t, profile-base: "write-100mb-")
           let string = make(<string>, size: 100, fill: 'x');
           with-open-file (stream = "/tmp/100mb.dylan.txt", direction: #"output")
             for (i from 1 to 1024 * 1024)
               write(stream, string);
             end;
           end;
         end;
       end function;

.. generic-function:: decache-all-generics

     Restores dispatch caches of generic functions in the given
     library and dependent libraries to their initial states.

   :signature: decache-all-generics (library) => ()

   :parameter library: An instance of :class:`<library>`.

.. generic-function:: clear-dispatch-profiling

     Resets call site profile counts and discriminator hit counts.

   :signature: clear-dispatch-profiling (library) => ()

   :description:

     Resets the call site profile counts and discriminator hit
     counts of all dispatch cache nodes for generic functions in the
     given library and dependent libraries.

   :parameter library: An instance of :class:`<library>`.

.. generic-function:: make-dispatch-statistics

     Instantiates a new object for collecting dispatch statistics in
     preparation for report output.

   :signature: make-dispatch-statistics (shared-generic-caches?) => (#rest results)

   :parameter shared-generic-caches?: An instance of :class:`<object>`.
   :value results: An instance of :class:`<application-profile-results>`.

.. generic-function:: clear-dispatch-statistics!

     Resets a :class:`<application-profile-results>` to its initial state.

   :signature: clear-dispatch-statistics! (profile) => ()

   :parameter profile: An instance of :class:`<application-profile-results>`.

.. generic-function:: collect-dispatch-statistics

     Traverses generic function call sites in the given library and
     collects dispatch statistics into the given profile results
     object.

   :signature: collect-dispatch-statistics (library profile) => ()

   :parameter library: An instance of :class:`<library>`.
   :parameter profile: An instance of :class:`<application-profile-results>`.

.. generic-function:: print-dispatch-statistics

    Prints out a summary of dispatch profiling results.

   :signature: print-dispatch-statistics (app-results #key library profile-base full? by-library? hits-only? app-results-only? uncalled-methods? app-details?) => ()

   :parameter app-results: An instance of :class:`<application-profile-results>`.
   :parameter #key library: An instance of ``false-or(<symbol>)``.
   :parameter #key profile-base: An instance of ``false-or(<string>)``.
   :parameter #key full?: An instance of :class:`<object>`. Defaults to ``#t``.
   :parameter #key by-library?: An instance of :class:`<object>`.
   :parameter #key hits-only?: An instance of :class:`<object>`. Defaults to ``#t``.
   :parameter #key app-results-only?: An instance of :class:`<object>`.
   :parameter #key uncalled-methods?: An instance of :class:`<object>`.
   :parameter #key app-details?: An instance of :class:`<object>`. Defaults to ``#t``.

   :description:

     If a particular ``library`` is requested (by name) and if
     ``by-library?`` is not false, then then ``profile-base`` must be
     provided, and the output is placed in files with names based on
     the profile base name, the library name, and the extension
     ``.prf``. Otherwise, the results summary is written to
     :var:`*standard-output*`.

     If ``hits-only?`` is not false (the default) then call sites that
     never successfully used the method dispatch cache will be omitted
     from the report. If ``uncalled-methods?`` is true, then the
     report will list methods in the dispatch cache that were never
     invoked.

.. generic-function:: enable-generic-caches-only

   :signature: enable-generic-caches-only (library) => ()

     Disable call-site method dispatch caches.

   :parameter library: An instance of :class:`<library>`.

     Configures the dispatch mechanisms in the Open Dylan run-time to
     only use per-:drm:`<generic-function>` caches for method dispatch
     rather than call-site specific caches. This was intended to be
     used for comparison purposes during the development of the
     dispatch mechanisms.

.. generic-function:: enable-call-site-caches-only

     Configures the dispatch mechanisms in the Open Dylan run-time to
     use call-site specific method dispatch caches.

   :signature: enable-call-site-caches-only (library) => ()

   :parameter library: An instance of :class:`<library>`.

