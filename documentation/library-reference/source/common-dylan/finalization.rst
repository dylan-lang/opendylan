***********************
The finalization Module
***********************

.. current-library:: common-dylan
.. current-module:: finalization

.. contents::
   :local:

Open Dylan provides a finalization interface in the *finalization*
module of *common-dylan*. This section explains finalization, the
finalization interface provided, and how to use the interface in
applications. Note that you *must* ``use finalization`` to be able
to use the interface described in this documentation.

What is finalization?
=====================

The `Memory Management Reference <http://www.memorymanagement.org>`_ defines
finalization as follows:

    In garbage-collected languages, it is often necessary to perform actions
    on some objects after they are no longer in use and before their memory
    can be recycled. These actions are known as finalization or termination.

    A common use of finalization is to release a resource when the
    corresponding "proxy" object dies. For example, an open file might be
    represented by a stream object. When the stream object has no references
    and can be collected, it is certain that the file is no longer in use by
    the [application] and can be closed.

Finalization is also commonly required when interfacing Dylan code with
foreign code that does not have automatic memory management. If an
interface involves a Dylan object that references a foreign object, it
may be necessary to free the memory resources of the foreign object when
the Dylan object is reclaimed.

How the finalization interface works
====================================

The following sections give a broad overview of how finalization works
and how to use the interface.

Registering objects for finalization
------------------------------------

Finalization works through cooperation with the garbage collector.
Objects that are no longer referenced by the application that created
them will eventually be discovered by Dylan’s garbage collector and are
then available to be reclaimed.

By default, the garbage collector reclaims such objects without
notifying your application. If it is necessary to finalize an object
before it is reclaimed, your application must inform the garbage
collector.

The garbage collector maintains a register of objects requiring
finalization before being reclaimed. To add an object to the register,
call the function :func:`finalize-when-unreachable` on the object.
Objects on the register are said to be *finalizable*.

If the garbage collector discovers that a finalizable object is no
longer referenced by the application, it does not reclaim it
immediately. Instead, it takes the object off its finalization register,
and adds it to the *finalization queue*.

The finalization queue contains all the objects awaiting finalization.
The garbage collector will not reclaim the objects until they have been
finalized.

A simple example of registering a finalizer:

.. code-block:: dylan

    define method initialize (lock :: <recursive-lock>, #key) => ()
      drain-finalization-queue();
      next-method();
      let res = primitive-make-recursive-lock(lock,
                                              lock.synchronization-name);
      check-synchronization-creation(lock, res);
      finalize-when-unreachable(lock);
    end method;

The reasons for calling :func:`drain-finalization-queue` are discussed below.

.. note:: The library containing this code must have ``use finalization;``
   in its module definition.

Draining the finalization queue
-------------------------------

Objects in the finalization queue wait there until the application
drains it by calling the function :func:`drain-finalization-queue`. This
function finalizes every object in the queue.

The finalization queue is not normally drained automatically. See
`How can my application drain the finalization queue automatically?`_
for details of how you can set up a thread to do so.

.. note:: The order in which objects in the finalization queue are
   finalized is not defined. Applications should not make any assumptions
   about finalization ordering.

Finalizers
----------

The :func:`drain-finalization-queue` function
finalizes each object in the finalization queue by calling the generic
function :gf:`finalize` on it. You should define
methods for :gf:`finalize` on those classes
whose instances may require finalization. These methods are called
*finalizers*.

The recommended interface to finalization is through
:func:`finalize-when-unreachable` and :func:`drain-finalization-queue`, but
calling :gf:`finalize` on an object directly is also
permitted. If you are certain you are finished with an object, it may be
desirable to do so. For example, you might want to finalize an object
created in a local binding before it goes out of scope.

.. note:: Finalizable objects are only removed from the register if the
   garbage collector discovers that they are unreachable and moves them
   into the finalization queue. Calling *finalize* on an object directly
   does not affect its registration status.

The :func:`drain-finalization-queue` function
makes each call to :gf:`finalize` inside
whatever dynamic handler environment is present when
:gf:`drain-finalization-queue` is called. If the call to
:gf:`drain-finalization-queue` is aborted via a non-local exit during a call
to :gf:`finalize`, the finalization queue retains all the objects that had
been added to it but which had not been passed to :gf:`finalize`.

There is a default method for :gf:`finalize` on
:drm:`<object>`. The method does nothing. It is available so that it is safe
for all finalizers to call :drm:`next-method`, a practice that we strongly
encourage. See `Writing finalizers`_.

After finalization
------------------

Once an object in the finalization queue has been finalized, it
typically becomes available for reclamation by the garbage collector.
Because it has been taken off the garbage collector’s finalization
register, it will not be queued up for finalization again.

.. note:: There are exceptions to this rule; see `The effects of
   multiple registrations`_ and `The effects of
   resurrecting objects`_.

Upon application exit
---------------------

There are no guarantees that objects which are registered for
finalization will actually be finalized before the application exits.
This is not a problem on many operating systems, which free any
resources held by a process when it exits.

Where it is necessary to guarantee an action at the time the application
exits, you should use a more explicit mechanism.

The effects of multiple registrations
-------------------------------------

Sometimes objects are registered for finalization more than once. The
effects of multiple registration are defined as follows:

Calling :func:`finalize-when-unreachable` on an
object *n* times causes that object to be added to the finalization
queue up to *n* times, where *n* is greater than or equal to zero. There
is no guarantee that the object will be added exactly *n* times.

Note that this definition so general that it does not guarantee that any
object will ever be added to be finalization queue. In practice, Common
Dylan’s implementation guarantees that an object is added to the queue
at least once whenever an object has ben determined to be unreachable by
the garbage collector.

To remain robust under multiple registration, finalizers should be
idempotent: that is, the effect of multiple :gf:`finalize` calls on an
object should is the same as the effect of a single call.

The effects of resurrecting objects
-----------------------------------

If a finalizer makes an object reachable again, by storing a reference
to the object in a variable, slot, or collection, we say it has
*resurrected* it. An object may also be resurrected if it becomes
reachable again when some other object is resurrected (because it is
directly or indirectly referenced by that other object).

Resurrecting objects has pitfalls, and must be done with great care.
Since finalizers typically destructively modify objects when freeing
their resources, it is common for finalization to render objects
unusable. We do not recommend resurrection if there is any possibility
of the object being left in an unusable state, or if the object
references any other objects whose transitive closure might include an
object left in such a state by another call to :gf:`finalize`.

If you do resurrect objects, note that they will not be finalized again
unless you re-register them.

The effects of finalizing objects directly
------------------------------------------

Any object that has been finalized directly, through the application
itself calling :gf:`finalize` on it, may not yet be unreachable. Like any
normal object it only becomes eligible for reclamation when it is
unreachable. If such an object was also registered for finalization
using :gf:`finalize-when-unreachable`, it can end up being finalized again
via the queue mechanism.

Finalization and weak tables
----------------------------

If an object is both registered for finalization and is weakly referred
to from a weak table, finalization occurs *first*, with weak references
being removed afterwards. That is, reachability is defined in terms of
strong references only, as far as finalization is concerned. Weak
references die only when an object’s storage is finally reclaimed.

For more on weak tables, see :doc:`Weak tables <../dylan/weak-tables>`.

Writing finalizers
==================

Because the default :gf:`finalize` method, on
:drm:`<object>`, does nothing, you must define your own
:gf:`finalize` methods to get results from the
finalization interface. This section contains useful information about
writing finalizers.

Class-based finalization
------------------------

If your application defines a class for which all instances require
finalization, call :func:`finalize-when-unreachable` in its ``initialize``
method.

Parallels with INITIALIZE methods
---------------------------------

The default method on :drm:`<object>` is provided to make it safe to call
:drm:`next-method` in all finalizers. This situation is parallel to that for
class :drm:`initialize` methods, which call ``next-method`` before performing
their own initializations. By doing so, ``initialize`` methods guarantee
that the most specific initializations occur last.

By contrast, finalizers should call ``next-method`` last, in case they
depend on the superclass finalizer not being run.

Simplicity and robustness
-------------------------

Write finalizers that are simple and robust. They might be called in any
context, including within other threads; with careful design, your
finalizers will work in most or all possible situations.

A finalizer might be called on the same object more than once. This
could occur if the object was registered for finalization more than
once, or if your application registered the object for finalization and
also called ``finalize`` on it directly. To account for this, write
finalizers that are idempotent: that is, the effect of multiple calls is
the same as the effect of a single call. See `The effects of
multiple registrations`_ for more on the effects
of multiple registrations.

Remember that the order in which the finalization queue is processed is
not defined. Finalizers cannot make assumptions about ordering.

This is particularly important to note when writing finalizers for
classes that are typically used to form circular or otherwise
interestingly connected graphs of objects. If guarantees about
finalization in graphs of objects are important, we suggest registering
a root object for finalization and making its finalizer traverse the
graph (in some graph-specific well-ordered fashion) and call the
``finalize`` method for each object in the graph requiring finalization.

Singleton finalizers
--------------------

Do not write singleton methods on :gf:`finalize`. The singleton method
itself would refer to the object, and hence prevent it from becoming
unreachable.

Using finalization in applications
==================================

This section answers questions about using finalization in an
application.

How can my application drain the finalization queue automatically?
------------------------------------------------------------------

If you would prefer the queue to be drained asynchronously, use the
automatic finalization interface. For more details, see
:func:`automatic-finalization-enabled?` and
:func:`automatic-finalization-enabled?-setter`.

Libraries that do not wish to depend on automatic finalization should
not use those functions. They should call
:func:`drain-finalization-queue` synchronously at
useful times, such as whenever they call ``finalize-when-unreachable``.

Libraries that are not written to depend on automatic finalization
should always behave correctly if they are used in an application that
does use it.

When should my application drain the finalization queue?
--------------------------------------------------------

If you do not use automatic finalization, drain the queue synchronously
at useful points in your application, such as whenever you call
:func:`finalize-when-unreachable` on an object.

This section contains a reference description for each item in the
finalization interface. These items are exported from the
*common-dylan* library in a module called *finalization*.

.. function:: automatic-finalization-enabled?

   Returns true if automatic finalization is enabled, and false otherwise.

   :signature: automatic-finalization-enabled? () => *enabled?*

   :value enabled?: An instance of :drm:`<boolean>`. Default value: ``#f``.

   :description:

     Returns true if automatic finalization is enabled, and false otherwise.

   See also

   - :func:`automatic-finalization-enabled?-setter`
   - :func:`drain-finalization-queue`
   - :func:`finalize-when-unreachable`
   - :gf:`finalize`

.. function:: automatic-finalization-enabled?-setter

   Sets the automatic finalization system state.

   :signature: automatic-finalization-enabled?-setter *newval* => ()

   :parameter newval: An instance of :drm:`<boolean>`.

   :description:

     Sets the automatic finalization system state to *newval*.

     The initial state is ``#f``. If the state changes from ``#f`` to
     ``#t``, a new thread is created which regularly calls
     :func:`drain-finalization-queue` inside an empty dynamic
     environment (that is, no dynamic condition handlers). If the state
     changes from ``#t`` to ``#f``, the thread exits.

   See also

   - :func:`automatic-finalization-enabled?`
   - :func:`drain-finalization-queue`
   - :func:`finalize-when-unreachable`
   - :gf:`finalize`

.. function:: drain-finalization-queue

   Calls :gf:`finalize` on every object in the finalization queue.

   :signature: drain-finalization-queue () => ()

   :description:

     Calls :gf:`finalize` on each object that is awaiting finalization.

     Each call to :gf:`finalize` is made inside whatever dynamic handler
     environment is present when ``drain-finalization-queue`` is called.
     If the call to ``drain-finalization-queue`` is aborted via a
     non-local exit during a call to ``finalize``, the finalization
     queue retains all the objects that had been added to it but which
     had not been passed to ``finalize``.

     The order in which objects in the finalization queue will be
     finalized is not defined. Applications should not make any
     assumptions about finalization ordering.

   See also

   - :func:`finalize-when-unreachable`
   - :gf:`finalize`
   - :func:`automatic-finalization-enabled?`
   - :func:`automatic-finalization-enabled?-setter`

.. function:: finalize-when-unreachable

   Registers an object for finalization.

   :signature: finalize-when-unreachable *object* => *object*

   :parameter object: An instance of :drm:`<object>`.
   :value object: An instance of :drm:`<object>`.

   :description:

     Registers *object* for finalization. If *object* becomes
     unreachable, it is added to the finalization queue rather than
     being immediately reclaimed.

     *Object* waits in the finalization queue until the application
     calls :func:`drain-finalization-queue`, which processes each object
     in the queue by calling the generic function :gf:`finalize` on it.

     The function returns its argument.

   See also

   - :gf:`finalize`
   - :func:`drain-finalization-queue`
   - :func:`automatic-finalization-enabled?`
   - :func:`automatic-finalization-enabled?-setter`

.. generic-function:: finalize

   Finalizes an object.

   :signature: finalize *object* => ()

   :parameter object: An instance of :drm:`<object>`.

   :description:

     Finalizes *object*.

     You can define methods on ``finalize`` to perform class-specific
     finalization procedures. These methods are called *finalizers*.

     A default :meth:`finalize <finalize(<object>)>` method on
     :drm:`<object>` is provided.

     The main interface to finalization is the function
     :func:`drain-finalization-queue`, which calls ``finalize`` on each
     object awaiting finalization. Objects join the finalization queue
     if they become unreachable after being registered for finalization
     with :func:`finalize-when-unreachable`. However, you can call
     ``finalize`` directly if you wish.

     Once finalized, *object* is available for reclamation by the
     garbage collector, unless finalization made it reachable again.
     (This is called *resurrection* ; see `The effects of resurrecting
     objects`_.) Because the object has been taken off the garbage
     collector’s finalization register, it will not be added to the
     finalization queue again, unless it is resurrected. However, it
     might still appear in the queue if it was registered more than
     once.

     Do not write singleton methods on :gf:`finalize`. A singleton
     method would itself reference the object, and hence prevent it from
     becoming unreachable.

   See also

   - :meth:`finalize <finalize(<object>)>`
   - :func:`finalize-when-unreachable`
   - :func:`drain-finalization-queue`
   - :func:`automatic-finalization-enabled?`
   - :func:`automatic-finalization-enabled?-setter`

.. method:: finalize
   :specializer: <object>

   Finalizes an object.

   :signature: finalize *object* => ()

   :parameter object: An instance of :drm:`<object>`.

   :description:

     This method is a default finalizer for all objects. It does nothing, and
     is provided only to make ``next-method`` calls safe for all methods on
     :gf:`finalize`.

   See also

   - :func:`finalize-when-unreachable`
   - :gf:`finalize`
   - :func:`drain-finalization-queue`
   - :func:`automatic-finalization-enabled?`
   - :func:`automatic-finalization-enabled?-setter`
