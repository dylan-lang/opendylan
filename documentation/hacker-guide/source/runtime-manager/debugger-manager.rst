****************************
The DEBUGGER-MANAGER library
****************************

.. current-library:: debugger-manager
.. current-module:: debugger-manager

The original version of this document was written by Paul Howard and
Tony Mann in March 1997.

Introduction
============

The Debugger Manager (DM) is quite a high-level component of the debugger
architecture. It sits between the access path interface and the debugger
UI. The DM provides abstractions over the debug operations described by
the access path interface, as well as providing dylan-specific debugging
functionality.

.. glossary::

   DM
      Debugger Manager. The component of the debugger described by
      this document.

   RTM
      Runtime Manager. The DM in conjunction with other internal
      support for the tether/interactivity: the access-path and
      interactive downloader.

   UI
     User Interface (of the debugging tool). Client of the
     DM.

   DevelDBG
     A simple Dylan debugger with a command-line interface.  Some
     implementation details of DevelDBG require specific functionality
     from the Debugger Manager. These have been kept to a minimum and
     are flagged in this document by "DevelDBG only".

For DevelDBG, the debugger UI will probably import most of the access
path functionality as well as debugger manager functionality. It is
doubtful that this model will be adopted for the final
debugger. Certainly _some_ access path definitions will need to be
exposed (stop-reasons being a good example), but exposing application
control functions like "stop" and "continue" potentially allows the UI
to interfere with the DM's management of the running app.

Model of the Debugger Manager
=============================

The general model of the debugging tool can be reduced to two concurrent
loops: a loop processing user input in the UI, and a loop receiving stop
reasons from the running application. The DM takes charge of the latter,
but synchronizes with the UI by operating a callback system, calling on UI
routines when important events are received from the running application.

Callbacks are invoked under the following circumstances:

#. The application stopped because a debug event or exception occurred
   (an "internal" stop), or because some event in the debugger UI
   signaled that the application should stop (an "external" stop).

#. The application hit a breakpoint/watchpoint. At the low level, a
   breakpoint/watchpoint hit is just another variety of internal stop.
   However, the DM provides a separate mechanism of callbacks for
   them, since they are the vehicle for many higher-level debugging
   facilities such as tracing.

#. The application is ready to continue. This means that the
   application had previously stopped for some reason (which may have
   involved the invocation of other callbacks), but the DM is now
   capable of allowing it to resume.

The following sections describe how and when these callbacks are
registered, and the precise form they must take.

Debug Targets
=============

A "debug target" represents whatever is being debugged. In most cases,
this will be a newly created application. Basically, a debug target is
very similar to an access path, and has a one-to-one correspondence
with an :class:`<access-path>` instance. The main difference is that
the :class:`<access-path>` is a sealed description of the connection
to the debugger nub, whereas the :class:`<debug-target>` can be
subclassed to hold any information that clients of the DM wish to
store.

.. class:: <debug-target>
   :open:
   :abstract:

   :superclasses: :drm:`<object>`

   :keyword application-object: An instance of :drm:`<object>`.
   :keyword compilation-context: An instance of :drm:`<object>`.
   :keyword top-level-component-name: An instance of :drm:`<string>`, or ``#f``.

   Describes an application being debugged. Users of the DM are
   allowed to create appropriate concrete subclasses of :class:`<debug-target>`
   with slots specific to their own purposes.

   It is always possible to map between a <debug-target> and its
   corresponding :class:`<access-path>`.

   When a :class:`<debug-target>` is made, an :class:`<access-path>`
   of the appropriate type is made automatically. The DM will also
   install the :class:`<debug-target>` as the
   :func:`access-path-abstract-handle` slot of the access path.

   This class is specified to take the same init-keywords as
   :class:`<access-path>`.  (They will just be passed on when the
   :class:`<access-path>` instance is made.)

   Example:

   .. code-block:: dylan

      define class <knackered-application> (<debug-target>)
      end class;

      define variable my-app =
                    make (<knackered-application>,
                          application: "spam",
                          arguments: "spam");

.. generic-function:: debug-target-access-path

   :signature: debug-target-access-path (object) => (value)

   :parameter object: An instance of :class:`<debug-target>`.
   :value value: An instance of :class:`<access-path>`.

   It's not clear that we need to export this function at all from the
   DM. (Exporting it is advantageous for DevelDBG since it allows us
   to use all the access-path functionality as well, such as
   :func:`access-path-arguments`). See the note in the `Introduction`_.

.. generic-function:: debug-target-symbol-table

   :signature: debug-target-symbol-table (object) => (value)

   :parameter object: An instance of :class:`<debug-target>`.
   :value value: An instance of :class:`<interactive-symbol-table>`.

.. generic-function:: debug-target-compilation-context

   :signature: debug-target-compilation-context (object) => (value)

   :parameter object: An instance of :class:`<debug-target>`.
   :value value: An instance of :drm:`<object>`.

.. generic-function:: debug-target-compilation-context-setter

   :signature: debug-target-compilation-context-setter (value object) => (value)

   :parameter value: An instance of :drm:`<object>`.
   :parameter object: An instance of :class:`<debug-target>`.
   :value value: An instance of :drm:`<object>`.

.. generic-function:: find-library-called

   :signature: find-library-called (application core-name) => (maybe-lib)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter core-name: An instance of :drm:`<string>`.
   :value maybe-lib: An instance of :class:`<remote-library>`.

   Attempts to find a :class:`<remote-library>` whose name matches the
   supplied string. Returns ``#f`` if no matching library is found.

.. generic-function:: obtain-component-name
   :open:

   :signature: obtain-component-name (application libname) => (name)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter libname: An instance of :drm:`<string>`.
   :value name: An instance of :drm:`<string>`.

   A name context contains the name of a dylan library. Often, this
   needs to be mapped to the name of a shared object (or DLL), which
   should be performed via this function.

.. method:: obtain-component-name
   :specializer: <debug-target>, <string>

.. class:: <interactor-return-breakpoint>

   :superclasses: :class:`<dylan-return-breakpoint>`

   :keyword required application-state: An instance of :drm:`<object>`.
   :keyword required result-spec: An instance of :drm:`<symbol>`.

.. generic-function:: interaction-request-application-state
   :open:

   :signature: interaction-request-application-state (interaction-transaction-id) => (application-state)

   :parameter interaction-transaction-id: An instance of :drm:`<object>`.
   :value application-state: An instance of :drm:`<object>`.

.. method:: interaction-request-application-state
   :specializer: <interactor-return-breakpoint>

.. generic-function:: interaction-request-application-state-setter
   :open:

   :signature: interaction-request-application-state-setter (application-state interaction-transaction-id) => (application-state)

   :parameter application-state: An instance of :drm:`<object>`.
   :parameter interaction-transaction-id: An instance of :drm:`<object>`.
   :value application-state: An instance of :drm:`<object>`.

.. method:: interaction-request-application-state-setter
   :specializer: <object>, <interactor-return-breakpoint>

Debugger Transaction Caching Utilities
======================================

.. class:: <page-relative-object-table>
   :open:
   :abstract:

   :superclasses: :drm:`<object>`

   :keyword required debug-target: An instance of :class:`<debug-target>`.

   A class used to store remote dylan objects in fast-lookup form.
   The values in the table can be arbitrary information that needs to
   be obtained from the key.

.. class:: <page-relative-object-table-entry>
   :open:
   :abstract:

   :superclasses: :drm:`<object>`

.. generic-function:: add-object
   :open:

   :signature: add-object (table instance entry) => ()

   :parameter table: An instance of :class:`<page-relative-object-table>`.
   :parameter instance: An instance of :const:`<remote-value>`.
   :parameter entry: An instance of :class:`<page-relative-object-table-entry>`.

.. method:: add-object
   :specializer: <page-relative-object-table>, <remote-value>, <page-relative-object-table-entry>

.. generic-function:: enquire-object
   :open:

   :signature: enquire-object (table instance) => (entry)

   :parameter table: An instance of :class:`<page-relative-object-table>`.
   :parameter instance: An instance of :const:`<remote-value>`.
   :value entry: An instance of :class:`<page-relative-object-table-entry>`, or ``#f``.

   Checks to see whether a dylan object is present in a table. If so,
   returns the description that was supplied to :func:`add-object`
   when the object was put into the table, otherwise returns ``#f``.

.. method:: enquire-object
   :specializer: <page-relative-object-table>, <remote-value>

.. generic-function:: remove-object
   :open:

   :signature: remove-object (table instance) => ()

   :parameter table: An instance of :class:`<page-relative-object-table>`.
   :parameter instance: An instance of :const:`<remote-value>`.

.. method:: remove-object
   :specializer: <page-relative-object-table>, <remote-value>

.. generic-function:: invalidate-page-relative-object-table
   :open:

   :signature: invalidate-page-relative-object-table (table) => ()

   :parameter table: An instance of :class:`<page-relative-object-table>`.

.. method:: invalidate-page-relative-object-table
   :specializer: <page-relative-object-table>

Stop Reasons
============

The :lib:`access-path` library exports the open abstract class
:class:`<external-stop-reason>` (as a subclass of
:class:`<stop-reason>`). The DM expands on this open branch of the
hierarchy.

.. generic-function:: stop-reason-debug-points

   :signature: stop-reason-debug-points (application sr) => (interested-debug-points)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter sr: An instance of :class:`<debug-point-stop-reason>`.
   :value interested-debug-points: An instance of :drm:`<sequence>`.

   Returns the sequence of :class:`<debug-point>` objects that caused
   the stop reason.  The sequence will only contain those debug-points
   whose callbacks returned ``#t`` (see `Registering Debug Points`_).

.. class:: <debugger-generated-stop-reason>
   :open:
   :abstract:

   :superclasses: :class:`<external-stop-reason>`

   :keyword client-data: An instance of :drm:`<object>`.

   A subclass of :class:`<external-stop-reason>`. A stop reason
   indicating that the application was stopped by some action on the
   part of the UI.

.. generic-function:: stop-reason-client-data

   :signature: stop-reason-client-data (object) => (value)

   :parameter object: An instance of :class:`<debugger-generated-stop-reason>`.
   :value value: An instance of :drm:`<object>`.

.. class:: <debugger-stop-application-stop-reason>

   :superclasses: :class:`<debugger-generated-stop-reason>`

   A subclass of :class:`<debugger-generated-stop-reason>`. The stop
   reason generated when the UI calls :func:`stop-application` (see
   `Managing Application Control`_) for some reason.

The access-path also supports an open subclass of
:class:`<internal-stop-reason>` called
:class:`<language-level-stop-reason>`. This is so that the DM can,
having examined the dynamic state of the application, present more
informative stop-reasons. Here are the subclasses defined by the DM,
along with various specific accessors.

.. class:: <dylan-invoke-debugger-stop-reason>

   :superclasses: :class:`<with-stack-protocol-stop-reason>`

   A subclass of :class:`<language-level-stop-reason>`. A stop-reason
   indicating that an unhandled Dylan condition resulted in a
   Dylan-level invocation of the debugger.

.. generic-function:: dylan-error-message-string

   :signature: dylan-error-message-string (sr) => (str)

   :parameter sr: An instance of :class:`<dylan-invoke-debugger-stop-reason>`.
   :value str: An instance of :drm:`<string>`.

   Returns the error message that resulted in the condition.

.. class:: <dylan-debug-message-stop-reason>

   :superclasses: :class:`<with-stack-protocol-stop-reason>`

   A subclass of :class:`<language-level-stop-reason>`. A stop-reason
   indicating that the application called a dylan-level debug utility
   to generate a formatted debugging message.

.. generic-function:: dylan-debug-message-string

   :signature: dylan-debug-message-string (sr) => (str)

   :parameter sr: An instance of :class:`<dylan-debug-message-stop-reason>`.
   :value str: An instance of :drm:`<string>`.

   Builds up and returns the formatted string that was generated by
   the :func:`debug-message` call.

.. class:: <source-code-alignment-stop-reason>

   :superclasses: :class:`<language-level-stop-reason>`


.. class:: <interactor-return-stop-reason>

   :superclasses: :class:`<language-level-stop-reason>`

   :keyword required transaction-id: An instance of :drm:`<object>`.

   A subclass of <language-level-stop-reason>. A stop-reason indicating
   that the execution of an interactive form has just returned, and
   that its results are available.

.. class:: <interactive-thread-initialized-stop-reason>

   :superclasses: :class:`<language-level-stop-reason>`

   :keyword required name: An instance of :drm:`<byte-string>`.

.. generic-function:: interactive-thread-name

   :signature: interactive-thread-name (object) => (value)

   :parameter object: An instance of :class:`<interactive-thread-initialized-stop-reason>`.
   :value value: An instance of :drm:`<byte-string>`.

.. generic-function:: interactor-transaction-id

   :signature: interactor-transaction-id (object) => (value)

   :parameter object: An instance of :class:`<interactor-return-stop-reason>`.
   :value value: An instance of :drm:`<object>`.

   All interactive evaluations have a transaction-id associated with
   them.  This accessor returns the id that was associated with the
   evaluation that just returned, generating the stop reason. This
   transaction-id will be :func:`\==` to the transaction-id that was returned
   when the interactor called execute-source (see gz's CSI document).

.. generic-function:: interactor-return-values

   :signature: interactor-return-values (sr) => (vals)

   :parameter sr: An instance of :class:`<interactor-return-stop-reason>`.
   :value vals: An instance of :drm:`<sequence>`.

   Returns a vector of :type:`<remote-value>`\ s corresponding to the
   sequence of return values generated by the interactive evaluation.

.. generic-function:: setup-interactor

   :signature: setup-interactor (application thread symbolic-c-entry-point symbolic-dll return-spec #rest args) => (transaction-id)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter thread: An instance of :class:`<remote-thread>`.
   :parameter symbolic-c-entry-point: An instance of :drm:`<string>`.
   :parameter symbolic-dll: An instance of :drm:`<string>`, or ``#f``.
   :parameter return-spec: An instance of :drm:`<symbol>`.
   :parameter #rest args: An instance of :drm:`<object>`.
   :value transaction-id: An instance of :drm:`<object>`.

.. generic-function:: handle-interactor-return
   :open:

   :signature: handle-interactor-return (application thread transaction-id #rest return-values) => (stop?)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter thread: An instance of :class:`<remote-thread>`.
   :parameter transaction-id: An instance of :drm:`<object>`.
   :parameter #rest return-values: An instance of :drm:`<object>`.
   :value stop?: An instance of :drm:`<boolean>`.

.. method:: handle-interactor-return
   :specializer: <debug-target>, <remote-thread>, <object>

.. class:: <class-breakpoint-stop-reason>

   :superclasses: :class:`<language-level-stop-reason>`

   :keyword required class: An instance of :const:`<remote-value>`.
   :keyword required size: An instance of :drm:`<integer>`.

.. generic-function:: class-breakpoint-class

   :signature: class-breakpoint-class (object) => (value)

   :parameter object: An instance of :class:`<class-breakpoint-stop-reason>`.
   :value value: An instance of :const:`<remote-value>`.

.. generic-function:: class-breakpoint-size

   :signature: class-breakpoint-size (object) => (value)

   :parameter object: An instance of :class:`<class-breakpoint-stop-reason>`.
   :value value: An instance of :drm:`<integer>`.


Managing Application Control
============================

The DM takes responsibility for most aspects of application
control. There are no explicit functions to wait for stop-reasons or
to continue execution — the DM performs these tasks on demand during
an indefinite loop. Within this loop, the DM invokes various callbacks
that can be registered by the UI.

.. generic-function:: stop-application

   :signature: stop-application (application #key stop-reason) => ()

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter #key stop-reason: An instance of :drm:`<object>`.

   Suspends the application with immediate effect.

.. generic-function:: kill-application

   :signature: kill-application (application) => ()

   :parameter application: An instance of :class:`<debug-target>`.

   Terminates the application regardless of its state.

.. generic-function:: restart-application

   :signature: restart-application (application) => ()

   :parameter application: An instance of :class:`<debug-target>`.

   Restarts the application from the beginning.

.. generic-function:: manage-running-application

   :signature: manage-running-application (application #key stop-reason-callback poll-for-stop-callback ready-to-continue-callback) => ()

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter #key stop-reason-callback: An instance of :drm:`<object>`.
   :parameter #key poll-for-stop-callback: An instance of :drm:`<object>`.
   :parameter #key ready-to-continue-callback: An instance of :drm:`<object>`.

   Starts up the application via its access path and enters an
   indefinite loop. Within this loop, the DM receives stop-reasons
   through the access-path and invokes the appropriate registered
   callback.

   This function does not return until the running application exits
   or is killed.

   A number of callbacks can be specified up-front as keyword
   parameters. The UI will almost certainly want to specify all of
   these, though defaults are provided.

   ``stop-reason-callback``
      A function of two arguments, a :class:`<debug-target>` and a
      :class:`<stop-reason>`, which returns a boolean. The DM invokes
      this callback whenever a stop-reason is received from the
      application. Note that breakpoint exceptions are a special case
      (see `Registering Debug Points`_).

      If the callback returns ``#f``, the application is silently
      resumed. If it returns true, the application remains suspended,
      and the ``ready-to-continue-callback`` is invoked with an instance
      of <internal-stop-reason>. The return value of true, therefore,
      indicates an "interest" in this stop reason, whereas ``#f``
      effectively filters it out.

      The application is known to be frozen during the entire execution
      of this callback.

   ``poll-for-stop-callback``
      A function of one argument, a :class:`<debug-target>`. In a
      single-threaded environment (which DevelDBG is), this function
      is needed to give the UI a chance to stop the application. The
      DevelDBG UI, for instance, has a "stop button" whose status can
      be checked during this periodic callback.

      During execution of this callback, the UI may call
      :func:`stop-application`.  This will result in the
      ``ready-to-continue-callback`` being invoked with an instance of
      :class:`<debugger-stop-application-stop-reason>`. But note that
      this will not happen until the ``poll-for-stop-callback`` returns.

      The DM undertakes to call this function at frequent intervals. The
      default callback does nothing (and hence does *not* call
      stop-application).

      For a multi-threaded GUI debugging tool, this callback will not
      be required.

      The application may be running during the execution of this
      callback.

  ``ready-to-continue-callback``
      A function of two arguments, a :class:`<debug-target>` and a
      :class:`<stop-reason>`.

      This callback is invoked whenever the debugger manager is ready
      to continue the application. If the application stopped of its
      own accord (an internal stop), then other callbacks will already
      have had the chance to do their own processing (such as the
      handling of debug-points).

      Reaching the ``ready-to-continue-callback`` means that *all* of
      this processing has been done, but the internal stop was still
      interesting enough for control to be passed to the UI before
      continuing.

      This callback may call the function :func:`kill-application`. In
      this case, the application will no longer be running, and
      :func:`manage-running-application` will return. This callback
      may also call the function :func:`restart-application`, in which
      case the app will be re-run from the beginning, but will
      continue to be managed in the same loop. (That is,
      :func:`manage-running-application` will not return).

      If the callback returns without having called
      :func:`kill-application` or :func:`restart-application`, the
      application will be resumed from the point where it stopped.

      The default callback does nothing.

      The application is known to be frozen during the entire
      execution of this callback.

      In a multi-threaded UI environment, this callback will probably
      block on some resource which can be freed by a "continue"
      gesture in the GUI.

.. generic-function:: application-stopped?

   :signature: application-stopped? (object) => (value)

   :parameter object: An instance of :class:`<debug-target>`.
   :value value: An instance of :drm:`<boolean>`.

.. generic-function:: application-stopped?-setter

   :signature: application-stopped?-setter (value object) => (value)

   :parameter value: An instance of :drm:`<boolean>`.
   :parameter object: An instance of :class:`<debug-target>`.
   :value value: An instance of :drm:`<boolean>`.

Registering Debug Points
========================

The DM allows any number of debug points to be positioned on any one
address (obviously, the first such debug point results in actually
setting a breakpoint or watchpoint in the application, but the DM
takes care of this implicitly). In the DM, a debug point is described
by an address paired with a callback.

Debug Point callbacks take three arguments, a :class:`<debug-target>`,
a :class:`<debug-point>`, and a :class:`<remote-thread>` (the thread
that signaled the debug point exception), and return a boolean.

A true return value indicates that the application should stop as a
result of this debug point (ie, the UI is interested in it, given the
current context).  ``#f`` implies that the callback has done all of
the necessary processing and the application may continue.

Debug point callbacks have slightly different semantics than those
callbacks described thus far. They are not functions that are
immediately called when a debug-point is encountered. Instead, they
are functions which *can* be called if the context is relevant. This
is further described below.

.. class:: <debug-point>
   :open:
   :abstract:

   :superclasses: :class:`<dm-registered-descriptor>`

   :keyword required address: An instance of :const:`<remote-value>`.
   :keyword required callback: An instance of :drm:`<function>`.

   This is NOT the same as :class:`<debug-point-stop-reason>` as
   defined in the :lib:`access-path` library.

   Requires the following init-keywords:

   ``address:``
      A :type:`<remote-value>` — the location at which this debug
      point is to be registered.

   ``callback:``

      A :drm:`<function>` — the callback to be invoked when the
      debug-point is hit. Should accept a :class:`<debug-target>`, a
      :class:`<debug-point>` and a :class:`<remote-thread>` as
      arguments, and return a boolean (as described above). Note that
      the DM does not *directly* invoke this callback. Instead, it
      calls the open generic function :func:`handle-debug-point-event`
      (see below), which may or may not subsequently invoke the
      callback.

.. class:: <breakpoint>
   :open:
   :abstract:

   :superclasses: :class:`<debug-point>`

.. class:: <watchpoint>
   :open:
   :abstract:

   :superclasses: :class:`<debug-point>`

.. class:: <tracepoint>
   :abstract:
   :instantiable:

   :superclasses: :class:`<debug-point>`

   All debug-points to do with function tracing are a subclass of
   :class:`<tracepoint>`. Calling :drm:`make` on this class returns an
   instance of :class:`<entry-tracepoint>`.

.. class:: <entry-tracepoint>
   :open:
   :abstract:

   :superclasses: :class:`<tracepoint>`

   :keyword required return-callback: An instance of :class:`<function>`.

   This requires a further init-keyword ``return-callback:`` (of the same
   specification as the ``callback:`` keyword argument).

   The default :gf:`handle-debug-point-event` method for
   :class:`<entry-tracepoint>` calls the registered callback as well
   as setting a :class:`<return-tracepoint>` on the return address via
   a protocol described below. This :class:`<return-tracepoint>` will
   have, as its registered callback, the function that was supplied as
   the return-callback.

   Entry tracepoints can only be set at the very start of functions.
   The DM will refuse to register an :class:`<entry-tracepoint>` whose ``address:``
   keyword does not correspond to the first address of a function
   definition. A :class:`<debug-point-error>` will be signaled if this
   condition is not met. (Maybe another error class should be defined
   for this specific case...?)

   Valid addresses are therefore those which are known in advance to
   be the addresses of functions. Addresses obtained via
   :func:`dylan-method-iep`, for example, will be valid. The address
   of a :class:`<remote-symbol>` will only be valid if the symbol
   denotes a function.

.. class:: <return-tracepoint>
   :open:
   :abstract:
   :instantiable:

   :superclasses: :class:`<tracepoint>`

   :keyword required entry: An instance of :class:`<entry-tracepoint>`.
   :keyword required frame: An instance of :const:`<remote-value>`.
   :keyword required thread: An instance of :class:`<remote-thread>`.

   It is not intended that clients of the DM should create and
   register <return-tracepoint> objects except via the special
   mechanism that the DM provides (see the functions below).

   The default :gf:`handle-debug-point-event` method for
   :class:`<return-tracepoint>` will invoke the registered callback if
   (and only if) the thread and stack contexts are the same as when
   the corresponding :class:`<entry-tracepoint>` was encountered. In
   this case, the :class:`<return-tracepoint>` will also deregister
   itself.

.. generic-function:: make-return-tracepoint
   :open:

   :signature: make-return-tracepoint (app bp thr #rest keys #key #all-keys) => (return-point)

   :parameter app: An instance of :class:`<debug-target>`.
   :parameter bp: An instance of :class:`<entry-tracepoint>`.
   :parameter thr: An instance of :class:`<remote-thread>`.
   :parameter #rest keys: An instance of :drm:`<object>`.
   :value return-point: An instance of :class:`<return-tracepoint>`.

   When the DM encounters a debug-point of type
   :class:`<entry-tracepoint>`, there is the need to set a
   :class:`<return-tracepoint>` on the corresponding return
   address. The DM calls this open generic function in order to create
   it. This is required since :class:`<return-tracepoint>` is an open
   class.

   The default method simply returns an instance of
   :class:`<return-tracepoint>`.

.. method:: make-return-tracepoint
   :specializer: <debug-target>, <entry-tracepoint>, <remote-thread>

.. method:: make-return-tracepoint
   :specializer: <debug-target>, <starting-dynamic-initialization>, <remote-thread>

.. generic-function:: initialize-return-tracepoint
   :open:

   :signature: initialize-return-tracepoint (app bp thr #key #all-keys) => ()

   :parameter app: An instance of :class:`<debug-target>`.
   :parameter bp: An instance of :class:`<return-tracepoint>`.
   :parameter thr: An instance of :class:`<remote-thread>`.

   After calling :gf:`make-return-tracepoint`, the DM also calls this
   open generic function with the newly created
   return-tracepoint. This allows the client to perform any other
   specialized initialization behaviour.

   The default method actually registers the return-tracepoint, so
   clients defining methods on this function should make sure that they
   call ``next-method()`` at some point.

.. method:: initialize-return-tracepoint
   :specializer: <debug-target>, <return-tracepoint>, <remote-thread>

.. generic-function:: corresponding-entry-tracepoint

   :signature: corresponding-entry-tracepoint (object) => (value)

   :parameter object: An instance of :class:`<return-tracepoint>`.
   :value value: An instance of :class:`<entry-tracepoint>`.

   Given a return-tracepoint, returns the registered entry-tracepoint
   that created it.

.. generic-function:: dylan-trace-entry-arguments

   :signature: dylan-trace-entry-arguments (application thread function-signature) => (required-arguments rest-vector keyword-arguments)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter thread: An instance of :class:`<remote-thread>`.
   :parameter function-signature: An instance of :const:`<remote-value>`.
   :value required-arguments: An instance of :drm:`<sequence>`.
   :value rest-vector: An instance of :class:`<remote-value>`, or ``#f``.
   :value keyword-arguments: An instance of :drm:`<sequence>`, or ``#f``.

.. generic-function:: dylan-trace-return-values

   :signature: dylan-trace-return-values (application thread) => (return-vals)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter thread: An instance of :class:`<remote-thread>`.
   :value return-vals: An instance of :drm:`<sequence>`.

.. generic-function:: handle-debug-point-event
   :open:

   :signature: handle-debug-point-event (application debug-point thr) => (register-interest?)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter debug-point: An instance of :class:`<debug-point>`.
   :parameter thr: An instance of :class:`<remote-thread>`.
   :value register-interest?: An instance of :drm:`<boolean>`.

   When a debug-point is encountered at address X, the DM selects all
   debug-points that were registered at X and calls this GF with each
   debug-point in turn.

   If any one of these calls returns true, the application will remain
   suspended, and the ``ready-to-continue-callback`` will be invoked with
   a :class:`<debug-point-stop-reason>`. If *all* calls return ``#f``,
   the application will be silently allowed to continue. Note that a
   true return value does not short-circuit this process — all
   selected debug-points still get handled.

   Clients of the DM can add methods to this function to specialize
   the behaviour of their own debug-point subclasses.

   The default method on this GF just invokes the registered callback
   for this debug-point, passing it the same arguments, and returning
   its return value. This default behaviour can be reached by calls to
   ``next-method()``.

   Example:

   .. code-block:: dylan

      define class <ph-breakpoint> (<breakpoint>)
      end class;

          ...

      define method handle-debug-point-event 
                      (app :: <debug-target>,
                       x :: <ph-breakpoint>,
                       t :: <remote-thread>) => (_ :: <boolean>)

           if (wind-blowing-in-the-right-direction())
               #f  // Don't invoke the registered callback, and
                   // don't signal any interest in this breakpoint.
           else
               next-method()
           end if
       end method;

.. generic-function:: register-debug-point

   :signature: register-debug-point (application debug-point) => ()

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter debug-point: An instance of :class:`<debug-point>`.

   Registers the :class:`<debug-point>` with the DM. It is added to
   any others that have already been registered at the same
   address. (If there are no others, then this call will actually set
   a low-level breakpoint).

.. generic-function:: deregister-debug-point

   :signature: deregister-debug-point (application debug-point) => ()

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter debug-point: An instance of :class:`<debug-point>`.

   De-registers the :class:`<debug-point>`.

.. class:: <debug-point-error>

   :superclasses: :drm:`<error>`

   A condition of this type will be signaled by
   :func:`register-debug-point` or :func:`deregister-debug-point` if
   either operation fails for some reason. This might occur, for
   example, if an address in the application's code was not mapped and
   so a breakpoint instruction could not be poked in.

.. generic-function:: application-running-on-code-entry?

   :signature: application-running-on-code-entry? (object) => (value)

   :parameter object: An instance of :class:`<debug-target>`.
   :value value: An instance of :drm:`<boolean>`.

.. generic-function:: application-running-on-code-entry?-setter

   :signature: application-running-on-code-entry?-setter (value object) => (value)

   :parameter value: An instance of :drm:`<boolean>`.
   :parameter object: An instance of :class:`<debug-target>`.
   :value value: An instance of :drm:`<boolean>`.

.. generic-function:: application-just-interacted?

   :signature: application-just-interacted? (object) => (value)

   :parameter object: An instance of :class:`<debug-target>`.
   :value value: An instance of :drm:`<boolean>`.

.. generic-function:: application-just-interacted?-setter

   :signature: application-just-interacted?-setter (value object) => (value)

   :parameter value: An instance of :drm:`<boolean>`.
   :parameter object: An instance of :class:`<debug-target>`.
   :value value: An instance of :drm:`<boolean>`.

.. generic-function:: interactor-deferred-id-table

   :signature: interactor-deferred-id-table (object) => (value)

   :parameter object: An instance of :class:`<debug-target>`.
   :value value: An instance of :class:`<table>`.


Example - simple function tracing
---------------------------------

.. code-block:: dylan

  define class <my-entry-trace> (<entry-tracepoint>)
         slot traced-function :: <remote-symbol>,
              required-init-keyword: symbol:;
  end class;

  define class <my-return-trace> (<return-tracepoint>)
         slot traced-function :: <remote-symbol>,
              required-init-keyword: symbol:;
  end class;

  define method print-entry-data
                 (app :: <debug-target>, bp :: <my-entry-trace>,
                  thr :: <remote-thread>) => (_ :: <boolean>)
         format-out ("Entered %s in thread %s\n",
                     bp.traced-function.remote-symbol-name,
                     thr.thread-name);
         #f;
  end method;

  define method print-return-data
                 (app :: <debug-target>, bp :: <my-return-trace>,
                  thr :: <remote-thread>) => (_ :: <boolean>)
         format-out ("Returned from %s in thread %s\n",
                     bp.traced-function.remote-symbol-name,
                     thr.thread-name);
         #f;
  end method;

  define method make-return-tracepoint
                 (app :: <debug-target>, bp :: <my-entry-trace>,
                  thr :: <remote-thread>, #rest keys, #key, #all-keys)
                  => (_ :: <my-return-trace>)
         apply (make, <my-return-trace>, symbol: bp.traced-function, keys)
  end method;

  // Just for completeness of the example...

  define method initialize-return-tracepoint
                 (app :: <debug-target>, bp :: <my-return-trace>,
                  thr :: <remote-thread>, #key) => ()
         next-method ()
  end method;
  .
  .
  .
      register-debug-point (my-application,
                            make (<my-entry-trace>,
                                  address: some-remote-value,
                                  symbol: some-remote-symbol,
                                  callback: print-entry-data,
                                  return-callback: print-return-data))
  .
  .


Dylan Name Context
==================

In order to resolve Dylan names, they need to be mangled by the
debugger in the same way that they were mangled by the compiler. In
order to do this, the debugger must know the "context" (the library
name and module name) to mangle into. For example, if the context
specifies the dylan library and the internal module, then `*pants*`
mangles to ``TpantsTYinternalVdylan``.

.. class:: <dylan-name-context>

   :superclasses: :drm:`<object>`

   :keyword library: An instance of :drm:`<byte-string>`.
   :keyword module: An instance of :drm:`<byte-string>`.

   Specifies a context for name mangling. Calling make on this class
   returns a context specifying the dylan library and the internal
   module, but these can be overridden by supplying keyword arguments
   ``library:`` and ``module:``, both with strings.

.. generic-function:: context-library

   :signature: context-library (object) => (value)

   :parameter object: An instance of :class:`<dylan-name-context>`.
   :value value: An instance of :drm:`<byte-string>`.

   Returns the name of the library in this context as a string.

.. generic-function:: context-library-setter

   :signature: context-library-setter (value object) => (value)

   :parameter value: An instance of :drm:`<byte-string>`.
   :parameter object: An instance of :class:`<dylan-name-context>`.
   :value value: An instance of :drm:`<byte-string>`.

   Sets the name of the library for this context.

.. generic-function:: context-module

   :signature: context-module (object) => (value)

   :parameter object: An instance of :class:`<dylan-name-context>`.
   :value value: An instance of :drm:`<byte-string>`.

   Returns the name of the module in this context.

.. generic-function:: context-module-setter

   :signature: context-module-setter (value object) => (value)

   :parameter value: An instance of :drm:`<byte-string>`.
   :parameter object: An instance of :class:`<dylan-name-context>`.
   :value value: An instance of :drm:`<byte-string>`.

   Sets the name of the module for this context.

.. generic-function:: demangle-dylan-name

   :signature: demangle-dylan-name (full-mangled-name) => (name-part module-part library-part method-name? method-iep? method-library-part method-number-part)

   :parameter full-mangled-name: An instance of :drm:`<byte-string>`.
   :value name-part: An instance of :drm:`<byte-string>`.
   :value module-part: An instance of :drm:`<byte-string>`.
   :value library-part: An instance of :drm:`<byte-string>`.
   :value method-name?: An instance of :drm:`<boolean>`.
   :value method-iep?: An instance of :drm:`<boolean>`.
   :value method-library-part: An instance of :drm:`<byte-string>`, or ``#f``.
   :value method-number-part: An instance of :drm:`<byte-string>`, or ``#f``.

.. generic-function:: demangle-local-dylan-name

   :signature: demangle-local-dylan-name (full-mangled-name) => (demang)

   :parameter full-mangled-name: An instance of :drm:`<byte-string>`.
   :value demang: An instance of :drm:`<byte-string>`.

.. generic-function:: mangle-local-dylan-name

   :signature: mangle-local-dylan-name (s) => (mangled)

   :parameter s: An instance of :drm:`<byte-string>`.
   :value mangled: An instance of :drm:`<byte-string>`.

.. generic-function:: mangle-in-context

   :signature: mangle-in-context (s cxt #key as-wrapper? as-static-object? as-entry-point?) => (mangled)

   :parameter s: An instance of :drm:`<byte-string>`.
   :parameter cxt: An instance of :class:`<dylan-name-context>`.
   :parameter #key as-wrapper?: An instance of :drm:`<object>`.
   :parameter #key as-static-object?: An instance of :drm:`<object>`.
   :parameter #key as-entry-point?: An instance of :drm:`<object>`.
   :value mangled: An instance of :drm:`<byte-string>`.

Transactions on dylan values
============================

.. generic-function:: read-dylan-value

   :signature: read-dylan-value (ap address) => (v ok)

   :parameter ap: An instance of :class:`<debug-target>`.
   :parameter address: An instance of :const:`<remote-location>`.
   :value v: An instance of :const:`<remote-value>`.
   :value ok: An instance of :drm:`<boolean>`.

.. generic-function:: write-dylan-value

   :signature: write-dylan-value (ap address value) => (v ok)

   :parameter ap: An instance of :class:`<debug-target>`.
   :parameter address: An instance of :const:`<remote-location>`.
   :parameter value: An instance of :const:`<remote-value>`.
   :value v: An instance of :const:`<remote-value>`.
   :value ok: An instance of :drm:`<boolean>`.

.. generic-function:: read-instance-slot-element

   :signature: read-instance-slot-element (ap object i) => (v ok)

   :parameter ap: An instance of :class:`<debug-target>`.
   :parameter object: An instance of :const:`<remote-value>`.
   :parameter i: An instance of :drm:`<integer>`.
   :value v: An instance of :const:`<remote-value>`.
   :value ok: An instance of :drm:`<boolean>`.

Printing and Inspecting Dylan Objects
=====================================

.. generic-function:: print-dylan-object

   :signature: print-dylan-object (application instance #key length level decorate? format) => (rep)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter instance: An instance of :const:`<remote-value>`.
   :parameter #key length: An instance of :drm:`<object>`.
   :parameter #key level: An instance of :drm:`<object>`.
   :parameter #key decorate?: An instance of :drm:`<boolean>`.
   :parameter #key format: An instance of :drm:`<symbol>`, or ``#f``.
   :value rep: An instance of :drm:`<string>`.

   Attempts to interpret the remote-value as a dylan object and
   generates a string representation of it. Immediates and "standard"
   objects (such as strings and simple-object-vectors) will be given
   direct representations.  General instances might just be
   represented as `[<SPAM>]`. The DM will have special knowledge of
   (possibly a subset of) condition objects, and will print their
   formatted messages if possible.

   Printable representations of collection objects (and other
   instances whose representations are structured) can expand to an
   inappropriately large size unless some limitations are
   specified. If ``length:`` is supplied, it should be an integer
   specifying the maximum number of components that should be printed
   for structured instances. (For example, the maximum number of
   elements of a collection. Collections with more than 'length'
   elements would be printed with the excess elements replaced by an
   ellipsis).

   If ``level:`` is supplied, it should be an integer specifying the
   level of depth to which structured printing should proceed. Again,
   deeper levels are abbreviated with an ellipsis. For example,
   ``#[1, 2, 3, #[1, 2], 4, 5]`` would be printed as such if ``level:``
   were greater than 0. With ``level:`` equal to 0, the representation
   would be ``#[1, 2, 3, #[...], 4, 5]``.

   If the :type:`<remote-value>` corresponds to a condition object in
   the runtime, the DM will attempt to generate the genuine formatted
   string by interpreting the condition's format-string and
   format-args. The DM only guarantees this for instances of
   :drm:`<simple-warning>`, :drm:`<simple-error>` and
   :drm:`<simple-restart>`.

.. generic-function:: describe-dylan-object

   :signature: describe-dylan-object (application instance) => (class-name slots slot-values repeats repeated-slot-name repeated-slot-values)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter instance: An instance of :const:`<remote-value>`.
   :value class-name: An instance of :drm:`<string>`.
   :value slots: An instance of :drm:`<sequence>`.
   :value slot-values: An instance of :drm:`<sequence>`.
   :value repeats: An instance of :drm:`<integer>`, or ``#f``.
   :value repeated-slot-name: An instance of :drm:`<string>`, or ``#f``.
   :value repeated-slot-values: An instance of :drm:`<sequence>`, or ``#f``.

.. generic-function:: get-inspector-values

   :signature: get-inspector-values (application instance) => (instance-class instance-slots slot-getters slot-setters repeats rept-slot rept-getter rept-setter nonword-repeats nonword-repeat-vector)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter instance: An instance of :const:`<remote-value>`.
   :value instance-class: An instance of :const:`<remote-value>`.
   :value instance-slots: An instance of :drm:`<sequence>`.
   :value slot-getters: An instance of :drm:`<sequence>`.
   :value slot-setters: An instance of :drm:`<sequence>`.
   :value repeats: An instance of :drm:`<integer>`, or ``#f``.
   :value rept-slot: An instance of :class:`<remote-value>`, or ``#f``.
   :value rept-getter: An instance of :drm:`<function>`, or ``#f``.
   :value rept-setter: An instance of :drm:`<function>`, or ``#f``.
   :value nonword-repeats: An instance of :drm:`<integer>`, or ``#f``.
   :value nonword-repeat-vector: An instance of :drm:`<vector>`, or ``#f``.

   A lower-level function similar in behaviour to
   describe-dylan-object, but with return values as follows:

   *class*
      A :type:`<remote-value>` representing the :drm:`<class>` that
      describes this Dylan object.

   *slots*
      A sequence of :type:`<remote-value>`\ s representing the slot descriptors
      for the class.

   *getters*
      A sequence of functions (closures) capable of reading the value
      from the corresponding slot. Each function takes no arguments
      and returns a :type:`<remote-value>`.

   *setters*
      Another sequence of functions capable of setting the value of
      the corresponding slot. Each function takes a single
      :class:`<remote-value>` (and also returns it), with the side-effect of
      inserting that <remote-value> into the slot.

      The three sequences are parallel, so ``getters[0]`` returns the value of
      ``slots[0]``, and ``setters[0]`` sets that slot, and so forth.

   *repeats*
      If the instance has no repeated slot, this value will be
      ``#f``. Otherwise, it will be an integer specifying the number of
      repeated elements. (Note that this integer can still be zero,
      thus distinguishing between an instance that has no repeated
      slot, and an instance that does have one except there are
      currently no elements).

   *repeated-slot*

      Unless *repeats* is ``#f``, this will be a
      :type:`<remote-value>` giving the slot descriptor for the
      repeated slot.

   *repeated-getter*

      Unless *repeats* is ``#f``, this will be a function of one
      integer argument (*i*) that returns a
      :type:`<remote-value>`. When called with some value *i* (where
      ``0 <= i <`` *repeats*), it returns the *i*\ th repeated element in
      the instance.  If *i* is not in the specified range, this function
      will return ``#f``.

   *repeated-setter*
      Unless *repeats* is ``#f``, this will be a function of two
      arguments, an :drm:`<integer>` and a
      :type:`<remote-value>`. When called with some integer *i* (where
      ``0 <= i <`` *repeats*), and some remote-value *v*, sets the
      *i*\ th repeated element in the instance to *v*.  If *i* is not
      in the specified range, this function will return ``#f``.

.. generic-function:: dylan-class-browser-information

   :signature: dylan-class-browser-information (application class-instance #key use-incarnation) => (slots navigation repeat count-offset element-size element-offset class-slot-count)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter class-instance: An instance of :const:`<remote-value>`.
   :parameter #key use-incarnation: An instance of :drm:`<object>`.
   :value slots: An instance of :drm:`<sequence>`.
   :value navigation: An instance of :class:`<string-table>`.
   :value repeat: An instance of :drm:`<string>`, or ``#f``.
   :value count-offset: An instance of :drm:`<integer>`, or ``#f``.
   :value element-size: An instance of :drm:`<integer>`, or ``#f``.
   :value element-offset: An instance of :drm:`<integer>`, or ``#f``.
   :value class-slot-count: An instance of :drm:`<integer>`.

.. generic-function:: dylan-class-slot-storage

   :signature: dylan-class-slot-storage (application class-instance #key use-incarnation) => (basic-names descriptors vals)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter class-instance: An instance of :const:`<remote-value>`.
   :parameter #key use-incarnation: An instance of :drm:`<object>`.
   :value basic-names: An instance of :drm:`<sequence>`.
   :value descriptors: An instance of :drm:`<sequence>`.
   :value vals: An instance of :drm:`<sequence>`.

.. generic-function:: dylan-object?

   :signature: dylan-object? (ap instance #key address?) => (val)

   :parameter ap: An instance of :class:`<debug-target>`.
   :parameter instance: An instance of :const:`<remote-value>`.
   :parameter #key address?: An instance of :drm:`<boolean>`.
   :value val: An instance of :drm:`<boolean>`.

   Probes the object represented by remote-value and tries to deduce
   whether it is a valid Dylan object. Returns true if the
   remote-value is (apparently) a Dylan object, otherwise returns
   ``#f``.

.. generic-function:: dylan-object-size

   :signature: dylan-object-size (application instance) => (byte-size-of-whole-object number-of-fixed-fields number-of-repeated-elements)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter instance: An instance of :const:`<remote-value>`.
   :value byte-size-of-whole-object: An instance of :drm:`<integer>`.
   :value number-of-fixed-fields: An instance of :drm:`<integer>`.
   :value number-of-repeated-elements: An instance of :drm:`<integer>`.

Mapping Between Symbolic Names and Objects
==========================================

.. generic-function:: resolve-dylan-name

   :signature: resolve-dylan-name (application name context #key indirect? library) => (val address)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter name: An instance of :drm:`<string>`.
   :parameter context: An instance of :class:`<dylan-name-context>`.
   :parameter #key indirect?: An instance of :drm:`<object>`.
   :parameter #key library: An instance of :drm:`<object>`.
   :value val: An instance of :class:`<remote-value>`, or ``#f``.
   :value address: An instance of :class:`<remote-value>`, or ``#f``.

   Mangles *string* according to the supplied name context and performs
   a symbol search for the mangled name.

   Two values are returned: the value associated with the name, and
   the address associated with the name. If the lookup fails, both
   return values will be ``#f``. Otherwise, the second return value is
   guaranteed to be a valid :type:`<remote-value>`, though the first
   may still be ``#f``.

   If ``indirect?:`` is true, the DM will attempt to resolve the dylan
   name to a symbol. If the symbol is found, the DM will read a value
   from the symbol's address. The value and the address that was
   indirected through are both returned. Note that the if the DM, for
   whatever reason, cannot read the value from the address, it will
   return ``#f`` as the value (although it will still return the
   address).

   If ``indirect?:`` is ``#f``, the DM will attempt to resolve the
   dylan name to an address. It also assumes this address to be the
   value, and does not perform an indirection. If the symbol is found,
   it just returns the address twice.

   If you are not interested in the address of a symbol, you can
   ignore the second return value.

   .. note::
      The console debugger needs the second 'address' return value in
      order to set variables with values.


.. generic-function:: resolve-dylan-keyword

   :signature: resolve-dylan-keyword (application sym) => (addr)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter sym: An instance of :drm:`<string>`.
   :value addr: An instance of :class:`<remote-value>`, or ``#f``.

   Finds the address of a keyword (:drm:`<symbol>` object) in the
   runtime. The string is the name of the keyword, without any
   syntactic baggage (ie, ``spam`` rather than ``spam:`` or
   ``#"spam"``). The DM will canonicalize any mixture of character
   case in the string, and search for the address of the keyword given
   the current state of the runtime's symbol dictionary.

   This function will return ``#f`` if the keyword is not found.

.. generic-function:: dylan-keyword-name

   :signature: dylan-keyword-name (application sym) => (str)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter sym: An instance of :const:`<remote-value>`.
   :value str: An instance of :drm:`<string>`.

.. generic-function:: find-dylan-name

   :signature: find-dylan-name (application address #key disambiguate-methods?) => (name context precise? constant?)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter address: An instance of :const:`<remote-value>`.
   :parameter #key disambiguate-methods?: An instance of :drm:`<object>`.
   :value name: An instance of :drm:`<string>`.
   :value context: An instance of :class:`<dylan-name-context>`.
   :value precise?: An instance of :drm:`<boolean>`.
   :value constant?: An instance of :drm:`<boolean>`.

   Searches for a symbol whose definition is at (or close to) the
   given :type:`<remote-value>`.

   Return values:

   *name*
      The demangled name of the symbol that was found.

   *context*
      A :class:`<dylan-name-context>` giving the name's library and
      module. (This will have been generated from the stripped-off
      qualifiers during demangling).

   *precise?*
      Will be true if the symbol's address exactly matched
      the remote-value, otherwise #f.

   *constant?*
      Will be true if the name represents a constant value.

.. generic-function:: find-closest-symbolic-name

   :signature: find-closest-symbolic-name (application instance) => (maybe-name precise?)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter instance: An instance of :const:`<remote-value>`.
   :value maybe-name: An instance of :class:`<remote-symbol>`, or ``#f``.
   :value precise?: An instance of :drm:`<boolean>`.

   Given a :type:`<remote-value>` instance, attempts to locate the
   :type:`<remote-symbol>` object whose definition is at or close to that
   address. The first return value will be that symbol, or ``#f`` if no
   symbol could be found.

   The second return value will be true if the symbol's definition
   precisely matches the supplied address, and ``#f`` otherwise.

   Note the use of the term *symbolic* rather than *mangled* in this
   function. There is no guarantee that the remote-value supplied is
   even a Dylan object, or that the symbol that defines it is a
   Dylan-emitted symbol. If any symbolic definition can be found, it
   will be returned.

   The source of symbolic information will be the access-path, in
   combination with whatever mechanism we implement for describing
   interactively (re-)defined symbols.

.. generic-function:: resolve-symbolic-name

   :signature: resolve-symbolic-name (application symbolic-name #key library) => (definition-address)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter symbolic-name: An instance of :drm:`<string>`.
   :parameter #key library: An instance of :drm:`<object>`.
   :value definition-address: An instance of :class:`<remote-value>`, or ``#f``.

   Attempts to match the supplied name against symbolic information in
   the runtime. If a match is made, the address of the name's
   definition is returned as the result.

   ``#f`` will be returned if no matching symbol can be found.

   Again, the source of information will be the access-path, in
   combination with whatever mechanism we implement for describing
   interactively (re-)defined symbols.

Convenience Interfaces for Dylan Objects
========================================

The general inspector interface can be used to look up all attributes
of any arbitrary Dylan object. However, the DM has special knowledge
of the Dylan runtime, and is able to deduce information about certain
"standard" objects, saving clients the trouble of calculating with the
inspector values. Function and class objects are good examples, though
we may introduce more and more of these convenience accessors as time
goes on.


.. generic-function:: dylan-generic-function-methods

   :signature: dylan-generic-function-methods (application gf-object) => (methods)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter gf-object: An instance of :const:`<remote-value>`.
   :value methods: An instance of :drm:`<sequence>`.

   Given that *gf-object* is a remote instance of a generic function,
   returns a sequence of :type:`<remote-value>`\ s that are remote instances of
   the methods of that generic function.

.. generic-function:: dylan-method-iep

   :signature: dylan-method-iep (application method-object) => (meth-iep)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter method-object: An instance of :const:`<remote-value>`.
   :value meth-iep: An instance of :const:`<remote-value>`.

   Given that *method-object* is a remote instance of a method object
   (perhaps obtained by a call to
   :func:`dylan-generic-function-methods`), returns a
   :type:`<remote-value>` that holds the IEP of that method.


.. generic-function:: dylan-method-specializers

   :signature: dylan-method-specializers (application method-object) => (specializers)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter method-object: An instance of :const:`<remote-value>`.
   :value specializers: An instance of :drm:`<sequence>`.

   Given that remote-value is a remote instance of a method object,
   returns a sequence of :type:`<remote-value>`\ s that are remote
   instances of :drm:`<type>`.

.. generic-function:: dylan-slot-descriptor-getter

   :signature: dylan-slot-descriptor-getter (application descriptor) => (getter)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter descriptor: An instance of :const:`<remote-value>`.
   :value getter: An instance of :const:`<remote-value>`.

   Given that *descriptor* is any remote instance of a slot
   descriptor, this function returns the getter (a remote instance of
   :drm:`<function>`) as a :type:`<remote-value>`. (Slot descriptors
   are returned from a call to :type:`get-inspector-values`. The
   "name" of a slot is effectively the name of its getter, obtained
   via this function. The getter can be passed to :func:`find-dylan-name`
   for a name).

.. generic-function:: remote-instance?

   :signature: remote-instance? (application instance class-instance) => (answer)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter instance: An instance of :const:`<remote-value>`.
   :parameter class-instance: An instance of :const:`<remote-value>`.
   :value answer: An instance of :drm:`<boolean>`.

   Returns true if both :type:`<remote-value>`\ s are Dylan objects,
   and the first is an instance of the second. Any
   :type:`<remote-value>`\ s corresponding to statically-built objects
   can be used as valid arguments to this function. However, if a
   :type:`<remote-value>` is *not* known to be statically built, the
   DM client is required to have that object registered, and to lookup
   the tracked value before calling this function.

   The implementation of this function may involve the execution of
   code in the runtime. However, calling it does not end a debugger
   transaction.

.. generic-function:: dylan-value-unbound?

   :signature: dylan-value-unbound? (application instance) => (answer)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter instance: An instance of :const:`<remote-value>`.
   :value answer: An instance of :drm:`<boolean>`.

   Returns true if the supplied :type:`<remote-value>` corresponds to
   the runtime's canonical UNBOUND marker. Otherwise, returns ``#f``.

.. generic-function:: dylan-object-immediate-value

   :signature: dylan-object-immediate-value (application instance) => (replica success?)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter instance: An instance of :const:`<remote-value>`.
   :value replica: An instance of :drm:`<object>`.
   :value success?: An instance of :drm:`<boolean>`.

   If the supplied :type:`<remote-value>` is an "uploadable
   immediate", such as a tagged integer or character, this function
   returns an environment-side replica of the runtime value.

   Note the second :drm:`<boolean>` return value. We cannot just use
   ``#f`` as a failure value in this case, since it's conceivable that
   ``#f`` could be an uploaded immediate, if booleans were tagged. Hence,
   the second return value flags whether the replica was successfully
   generated.

   Note that, even if the object can be uploaded to a replica, there
   is no guarantee that the precise type of the uploaded object will
   correspond to the type of the runtime object. For example, an
   :drm:`<integer>` in the runtime might get uploaded to an
   :class:`<extended-integer>` in the environment.

Debugger Transactions and Remote Object Registration
====================================================

More could be said about debugger transactions and remote object
registration. For now, suffice it to say that a debugger transaction
is in effect from the instant the application stops (for any reason)
until the instant that it resumes. During this period, any
:type:`<remote-value>`\ s collected from the application remain valid.
Between debugger transactions, objects can be relocated, so there is
no guarantee that a :type:`<remote-value>` that pointed to object X before
still points to object X.

In terms of the DM interface described thus far,
:type:`<remote-value>`\ s remain valid up until an activation of the
``ready-to-continue-callback`` terminates. This callback is therefore a
good place to register them as remote objects via the mechanism
described below.

In order for a handle on an object to persist between debugger
transactions, it must be "registered". This is a facility that is
provided by the Spy, with an interface to it being provided by the DM.

.. class:: <remote-object>
   :abstract:

   :superclasses: :class:`<runtime-registered-handle>`

   :keyword required debug-target: An instance of :class:`<debug-target>`.

   Represents a persistent handle on an object within the running
   application.  Unlike instances of :type:`<remote-value>`, instances
   of :class:`<remote-object>` remain valid between debugger
   transactions.


.. generic-function:: register-remote-object

   :signature: register-remote-object (application value #key finalize weak thread) => (robj)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter value: An instance of :const:`<remote-value>`.
   :parameter #key finalize: An instance of :drm:`<object>`.
   :parameter #key weak: An instance of :drm:`<object>`.
   :parameter #key thread: An instance of :drm:`<object>`.
   :value robj: An instance of :class:`<remote-object>`.

   Informs the Spy that the debugger now requires the object specified
   by *value* is to be tracked. The return value is an instance of
   :type:`<remote-object>` which can be used to obtain the object's
   value even if it is relocated.

   This mechanism is implemented by creating a new reference to the
   object within the application (so that it will be kept current by
   the garbage collector) using functionality provided by the Spy.

   If ``finalize`` is true (the default), then the reference will be
   implicitly freed by finalization when the development environment
   reclaims the remote object handle. If ``finalize`` is ``#f``, the
   overhead for registration may be lower, but a memory leak will
   result unless the UI explicitly frees the handle (via
   :func:`free-remote-object`).

   Normally, while a remote value is registered, the remote garbage
   collector will be prevented from condemning the remote value in any
   way that causes the object to be "lost". However, if ``weak`` is
   true, then the implementation is permitted to reference the remote
   value weakly, and to garbage collect it if there are no references
   within the running application itself. If the weakly registered
   object does get collected, subsequent calls to
   :func:`remote-object-value` will return ``#f``.


.. generic-function:: free-remote-object

   :signature: free-remote-object (application robj) => (#rest results)

   :parameter application: An instance of :drm:`<object>`.
   :parameter robj: An instance of :drm:`<object>`.
   :value #rest results: An instance of :drm:`<object>`.

   Informs the Spy that the debugger no longer needs this
   :class:`<remote-object>` to be tracked. Instances of
   :class:`<remote-object>` become invalid once passed to this
   function.

.. generic-function:: remote-object-value

   :signature: remote-object-value (application robj) => (#rest results)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter robj: An instance of :drm:`<object>`.
   :value value: An instance of :class:`<remote-value>`, or ``#f``.

   Maps a remote-object onto its remote-value.

.. class:: <object-registration-error>

   :superclasses: :drm:`<error>`

   An instance of this may be signaled by
   :func:`register-remote-object` if, for instance, the Spy does not
   support remote object registration.

.. generic-function:: object-requires-registration?

   :signature: object-requires-registration? (application instance) => (answer)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter instance: An instance of :const:`<remote-value>`.
   :value answer: An instance of :drm:`<boolean>`.

.. generic-function:: call-debugger-function
   :open:

   :signature: call-debugger-function (application function #rest arguments) => (#rest vals)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter function: An instance of :class:`<function>`.
   :parameter #rest arguments: An instance of :drm:`<object>`.
   :value #rest vals: An instance of :drm:`<object>`.

.. method:: call-debugger-function
   :specializer: <debug-target>, <function>


Stack Backtracing
=================

Provides functionality for modeling the stack in a running Dylan
application.


.. class:: <application-stack-frame>
   :abstract:

   :superclasses: :drm:`<object>`

   :keyword required generic-fp: An instance of :const:`<remote-value>`.
   :keyword newer: An instance of :class:`<application-stack-frame>`, or ``#f``.
   :keyword older: An instance of :class:`<application-stack-frame>`, or ``#f``.
   :keyword required thread: An instance of :class:`<remote-thread>`.

   Represents a stack frame of any kind within the application. (In
   the future, we might want to consider making this an open class so
   that clients of the DM can describe their own weird and wonderful
   stack frames.)

.. generic-function:: first-stack-frame

   :signature: first-stack-frame (application thread) => (top-frame)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter thread: An instance of :class:`<remote-thread>`.
   :value top-frame: An instance of :class:`<application-stack-frame>`.

   Returns the frame at the top of the stack in the running
   application, regardless of its type.


.. generic-function:: next-stack-frame

   :signature: next-stack-frame (application f) => (maybe-frame)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter f: An instance of :class:`<application-stack-frame>`.
   :value maybe-frame: An instance of :class:`<application-stack-frame>`, or ``#f``.

   Given a stack frame, returns the next most recent stack frame,
   regardless of its type. Returns ``#f`` if there is no next frame.

.. generic-function:: previous-stack-frame

   :signature: previous-stack-frame (application f) => (maybe-frame)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter f: An instance of :class:`<application-stack-frame>`.
   :value maybe-frame: An instance of :class:`<application-stack-frame>`, or ``#f``.

   Given a stack frame, returns the next oldest stack frame, regardless of
   its type. Returns ``#f`` if there is no previous frame.

.. class:: <dylan-stack-frame-mixin>
   :abstract:

   :superclasses: :drm:`<object>`

   This is a superclass of any Dylan stack frame.

   .. note::
      Possibly obsolescent.

.. class:: <call-frame>

   :superclasses: :class:`<access-path-stack-frame>`

   :keyword required fp: An instance of :const:`<remote-value>`.
   :keyword function-symbol: An instance of :class:`<remote-symbol>`, or ``#f``.
   :keyword required ip: An instance of :const:`<remote-value>`.
   :keyword linked-to: An instance of :class:`<call-frame>`, or ``#f``.
   :keyword required ret: An instance of :const:`<remote-value>`.

   This is a stack frame that corresponds to a function call.

.. generic-function:: call-frame-description

   :signature: call-frame-description (application frame) => (ap-frame)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter frame: An instance of :class:`<call-frame>`.
   :value ap-frame: An instance of :class:`<function-frame>`.

.. generic-function:: call-frame-return-address

   :signature: call-frame-return-address (application f) => (top-frame)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter f: An instance of :class:`<call-frame>`.
   :value return-address: An instance of :type:`<remote-value>`.

   Returns the call-frame's return address as a
   :type:`<remote-value>`.

.. generic-function:: call-frame-frame-pointer

   :signature: call-frame-frame-pointer (application f) => (top-frame)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter f: An instance of :class:`<call-frame>`.
   :value fp: An instance of :type:`<remote-value>`.

   Returns the call-frame's frame pointer as a :type:`<remote-value>`.

.. generic-function:: call-frame-frame-pointer

   :signature: call-frame-frame-pointer (application f) => (top-frame)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter f: An instance of :class:`<call-frame>`.
   :value ip: An instance of :type:`<remote-value>`.

   Returns the call-frame's instruction pointer (PC) as a
   :type:`<remote-value>`.

.. generic-function:: call-frame-nearest-source-locator

   :signature: call-frame-nearest-source-locator (application call-frame) => (maybe-locator)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter call-frame: An instance of :class:`<call-frame>`.
   :value maybe-locator: An instance of :class:`<source-locator>`, or ``#f``.

   If possible, returns the nearest known source location to the
   instruction pointer for the given call frame. If the frame is not
   precisely aligned at a source locator, this function can be used by
   the UI to find the nearest relevant piece of source to indicate as
   being "current".

.. generic-function:: call-frame-aligned-at-source-locator?

   :signature: call-frame-aligned-at-source-locator? (application call-frame) => (maybe-locator)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter call-frame: An instance of :class:`<call-frame>`.
   :value maybe-locator: An instance of :class:`<source-locator>`, or ``#f``.

   If the program counter for this call frame corresponds exactly to a
   known source code location, then this function returns the locator.

   If this function returns #f, then the instruction pointer is not at
   a known source code location. The function
   :func:`align-thread-to-source-location` can be used to attempt
   alignment. If alignment succeeds, it is not guaranteed that the
   destination source locator will be the same as that returned
   beforehand by :func:`call-frame-nearest-source-locator`.

.. generic-function:: call-frame-function

   :signature: call-frame-function (application frame) => (func-sym func-obj gf-obj)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter frame: An instance of :class:`<call-frame>`.
   :value func-sym: An instance of :class:`<remote-symbol>`, or ``#f``.
   :value func-obj: An instance of :class:`<remote-value>`, or ``#f``.
   :value gf-obj: An instance of :class:`<remote-value>`, or ``#f``.

   Returns a :class:`<remote-symbol>` that describes the function
   being called in this frame. This function might return ``#f`` if,
   for example, there is not sufficient symbolic debugging
   information.

   The second return value will be ``#f`` for any non-Dylan frame, and
   also for Dylan frames where the called function has no current
   model in the compiler (e.g., it's a closure, or a function from
   another project). Otherwise, the second return value will be a
   compiler model for the lambda being called in this frame. This
   model will have stored the correct (non-mangled) name for the
   function, as well as further information such as specializers.

.. generic-function:: number-of-lexical-variables

   :signature: number-of-lexical-variables (application dm-frame #key arguments-only?) => (i)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter dm-frame: An instance of :class:`<call-frame>`.
   :parameter #key arguments-only?: An instance of :drm:`<object>`.
   :value i: An instance of :drm:`<integer>`.

   Returns the number of lexical variables active inside the call
   frame. Performance note: this function scans for the existence of
   live lexical variables without reading in names/addresses over the
   tether. If it turns out that performance is not affected too much
   by reading names and addresses straight away, we can ditch this
   function.

   .. note::
      This has nothing to do with Dylan. The integer returned will be
      the same as the number of elements in the sequence(s) returned
      by :func:`live-frame-lexical-variables` NOT
      :func:`active-dylan-lexical-variables`.

.. generic-function:: active-dylan-lexical-variables

   :signature: active-dylan-lexical-variables (application dm-frame #key arguments-only?) => (names types models vals locations)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter dm-frame: An instance of :class:`<call-frame>`.
   :parameter #key arguments-only?: An instance of :drm:`<object>`.
   :value names: An instance of :drm:`<sequence>`.
   :value types: An instance of :drm:`<sequence>`.
   :value models: An instance of :drm:`<sequence>`.
   :value vals: An instance of :drm:`<sequence>`.
   :value locations: An instance of :drm:`<sequence>`.

   Presents the compiler's view of the set of live lexicals for the
   given :type:`<call-frame>`.

   The first return value is a sequence of lexical variable model
   objects.

   The fourth return value is a sequence parallel to the first. Each
   element either contains a :type:`<remote-value>` — the value of the
   corresponding variable (in the first sequence), or ``#f`` - meaning
   that this variable is not live. By "not live" we mean not live
   according to the debug information dump. It's entirely possible
   that dylan variables may be lexically in-scope, while not having
   obtainable values in the runtime.  (For example, their stack space
   may have been optimized away, or used for something else).

.. generic-function:: live-frame-lexical-variables

   :signature: live-frame-lexical-variables (application dm-frame #key arguments-only?) => (vars vals)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter dm-frame: An instance of :class:`<call-frame>`.
   :parameter #key arguments-only?: An instance of :drm:`<object>`.
   :value vars: An instance of :drm:`<sequence>`.
   :value vals: An instance of :drm:`<sequence>`.

   Presents the runtime's view of the set of live lexicals for the
   given :class:`<call-frame>`.

   The first return value is a sequence of :class:`<lexical-variable>`
   objects representing the lexical variables that are "live" in the
   given call frame. This information comes ultimately from the dumped
   debugging information.

   The second return value is a parallel sequence of :class:`<remote-value>`
   objects giving the values of the variables.

   This is basically just a lower-level API than
   :func:`active-dylan-lexical-variables`, assumed to be useful for
   frames running foreign code, and also frames running dylan code
   outside of the current project.


.. class:: <dylan-call-frame>

   :superclasses: :class:`<call-frame>`, :class:`<dylan-stack-frame-mixin>`

   Represents a frame corresponding to the activation of a Dylan
   function.

   .. note::
      Possibly obsolescent.

.. generic-function:: dylan-call-frame?

   :signature: dylan-call-frame? (application f) => (answer)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter f: An instance of :class:`<call-frame>`.
   :value answer: An instance of :drm:`<boolean>`.

.. class:: <implementation-stack-frame>
   :abstract:

   :superclasses: :class:`<application-stack-frame>`, :class:`<dylan-stack-frame-mixin>`

   Represents a "special" Dylan stack frame (not a call frame).

.. class:: <bind-exit-frame>

   :superclasses: :class:`<implementation-stack-frame>`

   A subclass of <implementation-stack-frame>. A frame corresponding to
   the Dylan ``block (exit) ... end`` construct.

.. class:: <unwind-protect-frame>

   :superclasses: :class:`<implementation-stack-frame>`

   :keyword required call-frame-pointer: An instance of :const:`<remote-value>`.
   :keyword required cleanup-address: An instance of :const:`<remote-value>`.

   Corresponds to a block construct with a cleanup.

Restarts
========

This section documents the APIs by which the debugger can determine
which restarts are available, and also signal a restart on a thread.


.. class:: <remote-restart>

   :superclasses: :drm:`<object>`

   :keyword abort?: An instance of :drm:`<boolean>`.
   :keyword description: An instance of :drm:`<string>`.
   :keyword format-args: An instance of :drm:`<vector>`, or ``#f``.
   :keyword format-string: An instance of :class:`<remote-value>`, or ``#f``.
   :keyword formatted?: An instance of :drm:`<boolean>`.
   :keyword required function: An instance of :const:`<remote-value>`.
   :keyword required index: An instance of :drm:`<integer>`.
   :keyword required init-args: An instance of :drm:`<vector>`.
   :keyword required target: An instance of :class:`<debug-target>`.
   :keyword required test: An instance of :const:`<remote-value>`.
   :keyword required type: An instance of :const:`<remote-value>`.

  A debugger-level abstract handle onto a restart. This is the object
  used to model restarts in the runtime.

.. generic-function:: remote-restart-description

   :signature: remote-restart-description (remote-restart) => (str)

   :parameter remote-restart: An instance of :class:`<remote-restart>`.
   :value str: An instance of :drm:`<string>`.

   Returns a string describing the expected behaviour of the restart.

.. generic-function:: available-restarts-for-thread

   :signature: available-restarts-for-thread (application thread) => (restarts)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter thread: An instance of :class:`<remote-thread>`.
   :value restarts: An instance of :drm:`<sequence>`.

   Examines the dynamic environment of the thread in order to generate
   a sequence of available restarts. Each member of the returned
   sequence is an instance of :class:`<remote-restart>`.

.. generic-function:: signal-restart-on-thread

   :signature: signal-restart-on-thread (application thread rst) => ()

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter thread: An instance of :class:`<remote-thread>`.
   :parameter rst: An instance of :class:`<remote-restart>`.

   Instructs the DM that the specified thread should, upon completion
   of the current debugger transaction, continue by signalling the
   specified restart (a :class:`<remote-restart>`).

Source-Level Stepping
=====================

These functions may only be called during a debugger transaction, and
they do not cause the application to immediately run. In each case,
the step operation occurs on the specified thread as soon as that
thread resumes.

All of these functions register specialized breakpoints. They may also
do some substantial low-level examination of the runtime, even down to
machine code instructions. The breakpoints will be signalled later, when the
thread reaches the destination source location.

Upon calling one of these functions, the DM attempts to calculate the
destination :class:`<source-locator>` for the step. There may be
several possibilities (for example, the entry points of all methods
when a generic function is being called). If the DM fails to calculate
any possible destination, it will return ``#f`` as the success code
from these functions. Otherwise, true is returned.  (The most likely
cause of failure would be a lack of line number information in the
debug info format). Clients of DM should not expect any specialized
breakpoints to be signalled following a ``#f`` result from one of
these functions.

However, these functions may work even in the absence of debug
information - just don't rely on obtaining a :class:`<source-locator>` for
the destination when the thread arrives there! (Step-out, for
example, can break the frame's return address regardless of whether
that address is a known source location).

These functions should be considered analogous to register-debug-point
(since that is what they end up doing!), hence the need for the
callback argument. The callbacks should be of the same signature as
those passed to :func:`register-debug-point`.

.. generic-function:: instruct-thread-to-step-over

   :signature: instruct-thread-to-step-over (application thread #key call-frame) => (success?)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter thread: An instance of :class:`<remote-thread>`.
   :parameter #key call-frame: An instance of :drm:`<object>`.
   :value success?: An instance of :drm:`<boolean>`.

   Arranges for the thread to step to the next source location within
   the execution of this function frame (e.g., stepping over function
   invocations).

   If the thread does not reach another source location within the
   execution of this function, then this will behave like a step-out
   operation instead.

.. generic-function:: instruct-thread-to-step-into

   :signature: instruct-thread-to-step-into (application thread #key call-frame precomputed-addresses) => (success?)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter thread: An instance of :class:`<remote-thread>`.
   :parameter #key call-frame: An instance of :drm:`<object>`.
   :parameter #key precomputed-addresses: An instance of :drm:`<object>`.
   :value success?: An instance of :drm:`<boolean>`.

   Arranges for the thread to step into a function.

   If the thread is not positioned exactly at a function call, this
   will behave like a step-over operation instead.

.. generic-function:: instruct-thread-to-step-out

   :signature: instruct-thread-to-step-out (application thread #key call-frame) => (success?)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter thread: An instance of :class:`<remote-thread>`.
   :parameter #key call-frame: An instance of :drm:`<object>`.
   :value success?: An instance of :drm:`<boolean>`.

   Arranges for the thread to step out of its current function frame.

.. generic-function:: align-thread-to-source-location

   :signature: align-thread-to-source-location (application thread #key interactive?) => (success?)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter thread: An instance of :class:`<remote-thread>`.
   :parameter #key interactive?: An instance of :drm:`<object>`.
   :value success?: An instance of :drm:`<boolean>`.

   Attempts to align the program counter of this thread to a known
   source location.

These functions should work in foreign code as well, provided that
sufficient debugging information is available for the source locators.

FIXME dropped a section here (about the relationship between tracing
and stepping) which might not be valid anymore.

Mappings Between Addresses and Source Locators
==============================================

This is a high-level abstraction for mapping locations in source code
to instruction addresses in the runtime, for the purpose of inspecting
local variables, or setting breakpoints etc. These functions are defined
to work from whatever information is available to the DM, either from
the Dylan compiler (if applicable), or from source location information
within the runtime (if available). These two APIs represent a unified
interface to these two sources of information.

.. generic-function:: remote-address-source-location

   :signature: remote-address-source-location (application address #key line-only? interactive-only? exact-only?) => (source-location exact?)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter address: An instance of :const:`<remote-value>`.
   :parameter #key line-only?: An instance of :drm:`<object>`.
   :parameter #key interactive-only?: An instance of :drm:`<object>`.
   :parameter #key exact-only?: An instance of :drm:`<object>`.
   :value source-location: An instance of :class:`<source-location>`, or ``#f``.
   :value exact?: An instance of :drm:`<boolean>`.

   Attempts to map an instruction address in the runtime to a location
   in source code.

.. generic-function:: source-location-remote-address

   :signature: source-location-remote-address (application source-location #key line-only? interactive-only? entry-point-only? compilation-context) => (address)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter source-location: An instance of :class:`<source-location>`.
   :parameter #key line-only?: An instance of :drm:`<object>`.
   :parameter #key interactive-only?: An instance of :drm:`<object>`.
   :parameter #key entry-point-only?: An instance of :drm:`<object>`.
   :parameter #key compilation-context: An instance of :drm:`<object>`.
   :value address: An instance of :class:`<remote-value>`, or ``#f``.

   Attempts to map a location in source code to an instruction address
   in the runtime.


The keyword arguments are interpreted as follows for both mappings:

``line-only?``
   Only use the max-one-per-line set of recorded source location
   points for the mapping.

``interactive-only?:``
   Only use the set of recorded "interactive" source location points
   for the mapping.

All source-locators are described using the canonical class in the
:lib:`source-records` library.

Note that :class:`<source-location>` objects may be stored
persistently. If the environment wishes to remember the locations of
breakpoints between runs of the application, then it should keep a set
of :class:`<source-location>` objects.  These can be mapped as
breakpoints via the DM when the application is started.

The DM makes the following guarantees:

#. It will map source locations via the compiler in preference to via
   the runtime.

#. The mapping via the compiler uses the same set of source-locations
   as is available via the API between the compiler and the
   environment.

The implication of these two guarantees is that the DM will always find
an exact mapping for any source-location retrieved via
:func:`definition-code-locations`, given the same values of 'line-only?' and
'interactive-only?'

.. note::
   This doesn't appear to be correct, and
   :func:`definition-code-locations` is an unimplemented stub.

Further Points:

#. Source locations obtained from runtime data will be standard
   :class:`<source-location>`\ s in standard source records. This will require
   extending the source record protocol to handle random (foreign)
   files.

#. Source locations obtained from runtime data are necessarily not
   considered to be interactive.


Foreign Code Debugging
======================

This chapter will be fleshed out when we have a solid story for
debugging foreign code. At the moment:

.. generic-function:: foreign-object-type

   :signature: foreign-object-type (application foreign-instance) => (remote-type-description)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter foreign-instance: An instance of :const:`<remote-value>`.
   :value remote-type-description: An instance of :drm:`<object>`.

   Returns, if possible, a :type:`<remote-type>` (defined in
   access-path) that describes the supplied object. This function
   cannot guarantee to return a :type:`<remote-type>`, and will return
   ``#f`` if no type description can be obtained.

   .. note::
     This is not implemented yet, and will just return #f.

Runtime Context
===============

.. class:: <runtime-context>
   :abstract:

   :superclasses: :drm:`<object>`

   :keyword scoped-variables: An instance of :drm:`<sequence>`.
   :keyword stack-frame: An instance of :class:`<application-stack-frame>`, or ``#f``.
   :keyword required target: An instance of :class:`<debug-target>`.
   :keyword required thread: An instance of :class:`<remote-thread>`.

   The DM is capable of allocating this object for any specified
   thread during a debugger transaction. Only the DM and the
   interactive downloader need to unpick this object.

.. generic-function:: runtime-context-debug-target

   :signature: runtime-context-debug-target (context) => (value)

   :parameter context: An instance of :class:`<runtime-context>`.
   :value value: An instance of :class:`<debug-target>`.

.. generic-function:: runtime-context-frame

   :signature: runtime-context-frame (context) => (value)

   :parameter context: An instance of :class:`<runtime-context>`.
   :value value: An instance of :class:`<application-stack-frame>`, or ``#f``.

.. generic-function:: runtime-context-thread

   :signature: runtime-context-thread (context) => (value)

   :parameter context: An instance of :class:`<runtime-context>`.
   :value value: An instance of :class:`<remote-thread>`.

.. method:: active-lexical-variables
   :specializer: <runtime-context>

   A method on the open GF exported by :mod:`dfmc-interactive-execution` in the
   :lib:`dfmc-browser-support` library.

.. generic-function:: runtime-context-lexical-variable-value

   :signature: runtime-context-lexical-variable-value (context, index) => (value)

   :parameter context: An instance of :class:`<runtime-context>`.
   :parameter index: An instance of :drm:`<integer>`.
   :value value: An instance of :class:`<remote-value>`.

   Returns the actual value of a lexical variable whose index was
   provided by active-lexical-variables.

.. generic-function:: current-runtime-context

   :signature: current-runtime-context (application thread #key stack-frame) => (context)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter thread: An instance of :class:`<remote-thread>`.
   :parameter #key stack-frame: An instance of :class:`<call-frame>`, or ``#f``.
   :value context: An instance of :class:`<runtime-context>`.

   Creates and returns the runtime-context for a thread in an application.


Profiling
=========

The Profiler Manager is used to collect profiling data from a
running application. It allows a client to control when profiling
information is to be collected and some limited control over what data
is collected. The profiler manager API is closely associated with the
Debugger Manager and is exported from the debugger-manager module in
the debugger-manager library.

Controlling the Profiler Manager
--------------------------------

Profiling is started by a call to :func:`start-profiling` and is
stopped by a call to :func:`stop-profiling`, or when the target
application exits. Profiling may be turned on and off any number of
times and does not have to be in the same state when the application
exits as it was when the application started.

The Profiler Manager gathers data by stopping the application at
regular intervals and taking a "snapshot" of each thread's stack. It's
possible to specify the interval between application stops and which
threads you are interested in profiling. The volume of data collected
can be further reduced by specifying a maximum stack depth to which
stacks are examined during snapshots.

The profiler manager maintains a complete history of the snapshots it
has collected, but allows this to be reset by a client library. The
profiler manager returns all the data in its history to the client on
request.

.. generic-function:: application-profiling?

   :signature: application-profiling? (application) => (profiling?)

   :parameter application: An instance of :class:`<debug-target>`.
   :value profiling?: An instance of :drm:`<boolean>`.

.. generic-function:: control-profiling

   :signature: control-profiling (application #key reset? snapshot-limit interval class-profiling? stack-depth threads) => ()

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter #key reset?: An instance of :drm:`<boolean>`. If
      supplied with a boolean value of ``#t`` tells the profiler
      manager to reset its history i.e. throw away all the data it has
      collected so far. The default value is ``#f``.
   :parameter #key snapshot-limit: An instance of :drm:`<object>`.
   :parameter #key interval: An instance of
      :drm:`<integer>`, or ``#f``. Specifies the regular interval in
      millisecs after which the application will be stopped and data
      collected. There is no guarantee that the application will be
      stopped precisely on this interval, but it will not be stopped
      before the interval is up. Not all threads will necessarily have
      had the same amount of cpu-time during the interval, so the
      profiler provides a weight for each thread based on the amount
      of cpu-time it has had.
   :parameter #key class-profiling?: An instance of :drm:`<boolean>`.
   :parameter #key stack-depth: An instance of :drm:`<object>`. The
      maximum depth to which the profiler should trace stack
      frames. Stacks deeper than this are still traced, but only to
      the depth specified. The limit applies for each snapshot taken
      and to all threads which are being profiled. ``#f`` (the default)
      indicates no limit and the entire stack is traced.
   :parameter #key threads: An instance of :drm:`<object>`.  A
      collection of :class:`<remote-thread>` objects which restricts
      the profiler to collecting data from the specified
      thread(s). ``#f`` (the default) indicates that data is to be
      collected from all threads. If one of the threads being profiled
      exits, the profiler manager stops collecting data for the
      thread, but keeps whatever data it has collected for it since
      the last reset in its history.

   Can be called at any time during a debugger transaction
   irrespective of whether profiling is on or not. It may also be
   called before the target application is running.

.. generic-function:: start-profiling

   :signature: start-profiling (application #key reset? snapshot-limit interval class-profiling? stack-depth threads) => ()

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter #key reset?: An instance of :drm:`<boolean>`.
   :parameter #key snapshot-limit: An instance of :drm:`<object>`.
   :parameter #key interval: An instance of :drm:`<integer>`, or ``#f``.
   :parameter #key class-profiling?: An instance of :drm:`<boolean>`.
   :parameter #key stack-depth: An instance of :drm:`<object>`.
   :parameter #key threads: An instance of :drm:`<object>`.

   Turns on profiling for the target application. If supplied, the
   keyword arguments override the properties set by any earlier
   :func:`control-profiling` call. In effect :func:`control-profiling`
   is called with the keyword arguments before profiling is switched
   on.

.. generic-function:: stop-profiling

   :signature: stop-profiling (application) => ()

   :parameter application: An instance of :class:`<debug-target>`.

   Stops the profiling of the target application. This may only be
   called during a debugger transaction.

.. generic-function:: profile-data

   :signature: profile-data (object) => (#rest results)

   :parameter object: An instance of :drm:`<object>`.
   :value #rest results: An instance of :drm:`<object>`.

   Returns all the profiling data collected since the last reset. See
   the next section for a description of the structure of the returned
   data.

.. method:: profile-data
   :specializer: <profile-state>

.. method:: profile-data
   :specializer: <debug-target>

.. generic-function:: reset-profile-data

   :signature: reset-profile-data (application) => ()

   :parameter application: An instance of :class:`<debug-target>`.


Extracting the data
-------------------

The data returned by the profiler comprises a series of snapshots for
each thread that was profiled. During a snapshot, the profiler manager
steps through the function call frames on the stack (to a maximum
depth if one has been specified) collecting an instruction pointer for
each frame. The instruction pointer is the address of the next
instruction to execute for the frame.

.. class:: <application-profile>

   :superclasses: :drm:`<object>`

   :keyword application-snapshots: An instance of :class:`<stretchy-object-vector>`.
   :keyword profile-threads: An instance of :class:`<stretchy-object-vector>`.

.. generic-function:: application-snapshot-skip

   :signature: application-snapshot-skip (object) => (value)

   :parameter object: An instance of :class:`<application-profile>`.
   :value value: An instance of :drm:`<integer>`.

.. generic-function:: application-snapshots

   :signature: application-snapshots (object) => (value)

   :parameter object: An instance of :class:`<application-profile>`.
   :value value: An instance of :class:`<stretchy-object-vector>`.

.. generic-function:: application-profile-threads

   :signature: application-profile-threads (object) => (value)

   :parameter object: An instance of :class:`<application-profile>`.
   :value value: An instance of :class:`<stretchy-object-vector>`.


.. class:: <application-snapshot>

   :superclasses: :drm:`<object>`

   :keyword required page-faults-increment: An instance of :drm:`<integer>`.
   :keyword required thread-snapshots: An instance of :drm:`<sequence>`.
   :keyword required wall-time-increment: An instance of :drm:`<integer>`.

.. generic-function:: application-thread-snapshot

   :signature: application-thread-snapshot (snapshot thread) => (thread-snapshot)

   :parameter snapshot: An instance of :class:`<application-snapshot>`.
   :parameter thread: An instance of :class:`<remote-thread>`.
   :value thread-snapshot: An instance of :class:`<thread-snapshot>`, or ``#f``.

.. generic-function:: wall-time-increment

   :signature: wall-time-increment (object) => (value)

   :parameter object: An instance of :class:`<application-snapshot>`.
   :value value: An instance of :drm:`<integer>`.

.. generic-function:: page-faults-increment

   :signature: page-faults-increment (object) => (value)

   :parameter object: An instance of :class:`<application-snapshot>`.
   :value value: An instance of :drm:`<integer>`.

.. generic-function:: thread-snapshots

   :signature: thread-snapshots (object) => (value)

   :parameter object: An instance of :class:`<application-snapshot>`.
   :value value: An instance of :drm:`<sequence>`.

.. class:: <thread-snapshot>

   :superclasses: :drm:`<object>`

   :keyword required allocated-class: An instance of :class:`<remote-value>`, or ``#f``.
   :keyword required allocation-increment: An instance of :drm:`<integer>`.
   :keyword required cpu-time-increment: An instance of :drm:`<integer>`.
   :keyword required instruction-pointers: An instance of :const:`<instruction-pointers>`.
   :keyword required thread: An instance of :class:`<remote-thread>`.

   Includes a <snapshot-sequence> for the thread and an indication of
   which thread the data was collected from.

   This describes the data collected from a thread during a
   "snapshot". It includes the sequence of instruction pointers
   associated with the stack frames on the thread's stack at the time
   of the snapshot (to a maximum depth, if one was specified) and a
   weight for this data.

.. type:: <instruction-pointers>

.. generic-function:: profile-thread

   :signature: profile-thread (object) => (value)

   :parameter object: An instance of :class:`<thread-snapshot>`.
   :value value: An instance of :class:`<remote-thread>`.

   Returns the :class:`<remote-thread>` object associated with the
   thread from which the data was collected.

.. generic-function:: cpu-time-increment

   :signature: cpu-time-increment (object) => (value)

   :parameter object: An instance of :class:`<thread-snapshot>`.
   :value value: An instance of :drm:`<integer>`.

.. generic-function:: allocation-increment

   :signature: allocation-increment (object) => (value)

   :parameter object: An instance of :class:`<thread-snapshot>`.
   :value value: An instance of :drm:`<integer>`.

.. generic-function:: allocated-class

   :signature: allocated-class (object) => (value)

   :parameter object: An instance of :class:`<thread-snapshot>`.
   :value value: An instance of :class:`<remote-value>`, or ``#f``.

.. generic-function:: instruction-pointers

   :signature: instruction-pointers (object) => (value)

   :parameter object: An instance of :class:`<thread-snapshot>`.
   :value value: An instance of :const:`<instruction-pointers>`.

   Returns a sequence of instruction pointers associated with each
   stack frame on the thread's stack. Each instruction pointer is the
   address of the next instruction to execute for the frame as a
   :type:`<remote-value>`. The instruction pointers are ordered with
   those from the most recently created stack frames (top of stack)
   appearing first.

.. generic-function:: set-application-class-breakpoint

   :signature: set-application-class-breakpoint (application thread class) => (transaction)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter thread: An instance of :class:`<remote-thread>`.
   :parameter class: An instance of :class:`<remote-value>`, or ``#f``.
   :value transaction: An instance of :drm:`<object>`.

.. generic-function:: clear-application-class-breakpoint

   :signature: clear-application-class-breakpoint (application thread class #key stop-profile?) => (transaction)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter thread: An instance of :class:`<remote-thread>`.
   :parameter class: An instance of :class:`<remote-value>`, or ``#f``.
   :parameter #key stop-profile?: An instance of :drm:`<object>`.
   :value transaction: An instance of :drm:`<object>`.

   Clears a remote class breakpoint.

.. generic-function:: clear-application-class-breakpoints

   :signature: clear-application-class-breakpoints (application thread) => (transaction)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter thread: An instance of :class:`<remote-thread>`.
   :value transaction: An instance of :drm:`<object>`.

   Clears all remote class breakpoints.


Extension Interfaces
====================

.. generic-function:: load-runtime-component

   :signature: load-runtime-component (application name) => (success?)

   :parameter application: An instance of :class:`<debug-target>`.
   :parameter name: An instance of :drm:`<byte-string>`.
   :value success?: An instance of :drm:`<boolean>`.

   Attempts to dynamically load a runtime component (DLL).

.. macro:: spy-function-definer

.. class:: <c-spy-function-descriptor>

   :superclasses: :class:`<spy-function-descriptor>`

   :keyword entry-point: An instance of :class:`<remote-value>`, or ``#f``.

   Describes a spy function that must be called with C calling
   conventions.

.. generic-function:: spy-function-runtime-name

   :signature: spy-function-runtime-name (sf) => (name)

   :parameter sf: An instance of :class:`<spy-function-descriptor>`.
   :value name: An instance of :drm:`<string>`.

   Returns the name of the spy function.

.. generic-function:: spy-function-runtime-component

   :signature: spy-function-runtime-component (sf) => (name)

   :parameter sf: An instance of :class:`<spy-function-descriptor>`.
   :value name: An instance of :drm:`<string>`.

   Returns the component name of the spy function

.. generic-function:: call-spy

   :signature: call-spy (spy-function application #rest arguments) => (result)

   :parameter spy-function: An instance of :class:`<c-spy-function-descriptor>`.
   :parameter application: An instance of :class:`<debug-target>`.
   :parameter #rest arguments: An instance of :drm:`<object>`.
   :value result: An instance of :const:`<remote-value>`.

.. generic-function:: call-spy-on-thread

   :signature: call-spy-on-thread (spy-function application thread #rest arguments) => (result)

   :parameter spy-function: An instance of :class:`<c-spy-function-descriptor>`.
   :parameter application: An instance of :class:`<debug-target>`.
   :parameter thread: An instance of :class:`<remote-thread>`.
   :parameter #rest arguments: An instance of :drm:`<object>`.
   :value result: An instance of :const:`<remote-value>`.

.. class:: <spy-call-error>
   :abstract:

   :superclasses: :drm:`<error>`

   :keyword required arguments: An instance of :drm:`<sequence>`.
   :keyword required debug-target: An instance of :class:`<debug-target>`.
   :keyword required function-descriptor: An instance of :class:`<spy-function-descriptor>`.

   The common superclass of all exceptions that can occur when
   attempting to call a spy function.

.. generic-function:: spy-call-function-descriptor

   :signature: spy-call-function-descriptor (object) => (value)

   :parameter object: An instance of :class:`<spy-call-error>`.
   :value value: An instance of :class:`<spy-function-descriptor>`.

.. generic-function:: spy-call-debug-target

   :signature: spy-call-debug-target (object) => (value)

   :parameter object: An instance of :class:`<spy-call-error>`.
   :value value: An instance of :class:`<debug-target>`.

.. generic-function:: spy-call-arguments

   :signature: spy-call-arguments (object) => (value)

   :parameter object: An instance of :class:`<spy-call-error>`.
   :value value: An instance of :drm:`<sequence>`.

.. class:: <spy-function-not-located>

   :superclasses: :class:`<spy-call-error>`


.. class:: <spy-call-aborted>

   :superclasses: :class:`<spy-call-error>`


.. class:: <spy-call-no-available-thread>

   :superclasses: :class:`<spy-call-error>`


.. class:: <spy-call-cannot-use-thread>

   :superclasses: :class:`<spy-call-error>`

   :keyword required selected-thread: An instance of :class:`<remote-thread>`.

   An attempt was made to call a spy function on a specific thread,
   but the thread could not be used.

.. generic-function:: spy-call-selected-thread

   :signature: spy-call-selected-thread (object) => (value)

   :parameter object: An instance of :class:`<spy-call-cannot-use-thread>`.
   :value value: An instance of :class:`<remote-thread>`.



