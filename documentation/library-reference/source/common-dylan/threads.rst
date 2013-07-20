*******************
The threads Module
*******************

Introduction
============

.. current-library:: dylan

.. module:: threads
   :library: dylan

   The Threads module provides a portable threads interface for Dylan. The
   Threads module is designed to map easily and efficiently onto the
   threads facilities provided by all common operating systems.

   All documented bindings are exported from the module *threads* in the
   *dylan* and *common-dylan* libraries.

Multi-thread semantics
======================

The Threads module provides multiple threads of control within a single
space of objects and module variables. Each thread runs in its own
independent stack. The mechanism by which the threads are scheduled is
not specified, and it is not possible to determine how the execution of
instructions by different threads will be interleaved. No mechanism is
provided to call a function on an existing thread other than the current
thread. Neither is there a mechanism to signal an exception on a thread
other than the current thread.

Atomicity
---------

In general, the Threads module guarantees that assignments to slots and
variables are atomic. That is, after an assignment, but before
synchronization, another thread will see either the old value or the new
value of the location. There is no possibility of seeing a half-way
state.

In some circumstances, when a slot or a variable is specialized to be of
a particularly constrained type, the Threads module does not guarantee
atomicity of assignments. Such a type may include a subtype of
:drm:`<double-float>` or a subtype of :drm:`<extended-float>`. It may not include
any other type that is either defined in the current specification of
the Dylan language, or that could be created from standard facilities
provided by the current specification of the language. This restriction
of the atomicity guarantee is intended to permit implementations to
represent the values of such slots or variables in a form which uses
more space than a normal Dylan value, for optimal efficiency.

For those cases where the implementation does not provide the atomicity
guarantee, the results of accessing a normal variable are undefined if:

- The read could proceed in parallel with some write of the same
  location
- Two writes of the same location could have proceeded in parallel
  since the last non-parallel write

Two memory references *proceed in parallel* if they are not explicitly
sequentialized, either by being in a single thread, or by explicit
inter-thread synchronization.

Programmers should guard against the possibility of undefined values by
using explicit inter-thread synchronization.

Ordering
--------

The ordering of visibility of side effects performed in other threads is
undefined, unless explicit synchronization is used. Implementations of
the module may guarantee that the visibility of side-effects performed
by another thread is ordered according to the control flow of the other
thread (*strong ordering*), but multi-processor implementations might
not be strongly ordered. Portable code should not assume strong
ordering, and should use explicit synchronization where the order of
side effects is important. There is currently no module introspection
facility to determine if the implementation is strongly or weakly
ordered.

Because of the possibility of weak ordering, the compiler is free to
assume that the effects of other threads may be ignored between explicit
synchronization points, and it may perform any optimizations which
preserve the semantics of a single-thread model regardless of their
effects on other threads — for example, common sub-expression
elimination, or changing the order of evaluation.

Explicit synchronization
------------------------

The Threads module provides low-level synchronization functions which
control the ordering of operations with respect to other threads, and
control when the side effects that have been performed within one thread
become visible within other threads.

At a higher level, the Threads module provides a variety of
synchronization facilities, described below. These facilities include
mutual-exclusion locks, semaphores and notifications. Each facility
guarantees that when synchronization has been achieved, all the side
effects of another thread are visible, at least up to the point where
that other thread last released the synchronization facility.

An appropriate synchronization must be used to guard side-effects on
state if there is any possibility of those side-effects either being
corrupted by another thread or corrupting another thread. For example, a
function which assigns to two slots of an object may require the use of
a lock to guarantee that other threads never observe the object in a
partly updated state.

It is up to module designers to document when synchronization is not
performed internally, and when synchronization protocols must be used by
clients. The implications for the Dylan library, and some other
low-level libraries, are discussed in `Thread safety in client
libraries`_.

Conditional update
------------------

In addition to the synchronization primitives, the module provides a
conditional update mechanism which is not synchronized, but which tests
whether the value in a variable or slot has changed and atomically
updates it if not.

By using conditional updates, a thread can confirm (or deny) that there
has been no interference from other threads, without any need for a
blocking operation. This is more efficient for those circumstances where
interference is not disastrous and it is possible to recompute the
update.

For example, a function which increments the value of a variable might
use a conditional update to store the new value into place, in order to
guarantee a numeric sequence for the variable. In this example, the
function might loop until the conditional update has succeeded.

It is possible to achieve synchronization by looping until a conditional
update is successful, and then synchronizing side effects. This is not
recommended, because the busy-waiting state during the loop may disallow
other threads from running. Normally, conditional update should be used
only when it is expected to succeed. If it is likely that the
conditional update might fail multiple times around the loop, then
either the number of times around the loop should be limited, or a
blocking function from the Threads module should be used within the
loop.

The dynamic environment
-----------------------

Dylan has an implicit notion of a *dynamic environment*, corresponding
to language constructs with *dynamic extent*. For example, the *block*
construct can introduce *cleanup-clauses*, and the *body* of the block
is executed in a dynamic environment in which those cleanup-clauses are
active. *Handlers* and *exit procedures* are other examples of language
features related to the dynamic environment.

The dynamic environment is defined to be thread-local. When a new thread
is created, it starts with a fresh dynamic environment. It is an error
to attempt to use a handler or a non-local exit function belonging to
another thread. It is impossible to use an unwind-protect cleanup from
another thread.

Although the binding of condition handlers only affects the dynamic
environment of the current thread, unhandled conditions are passed to
the global generic function *default-handler*. This function might
*call the debugger*. The Threads module does not define what calling
the debugger means.

Note that in Dylan, unlike in C and C++, *lexical* variables (that is
local, or *let* -bound variables) have indefinite extent — that is, have
a lifetime independent of the function or block in which they were
created — and are not bound in the dynamic environment. Because those
variables are in general potentially global, you may need to explicitly
synchronize accesses to them.

Thread variables
----------------

The Threads module provides a new type of variable: a *thread*
variable, also known as a *thread-local* variable. These variables are
similar to normal module variables in the sense that they are visible
according to the same scoping rules and have the same semantics in a
single-threaded program. However, in contrast to a normal variable,
assignments to a thread variable in one thread are not visible when
evaluating the variable in another thread.

Whenever a thread is created, the value of each thread variable is
initialized to a thread-independent value resulting from a once-only
evaluation of the initialization expression of the thread variable
definition.

See page :macro:`thread` for details of the ``thread`` adjective to
``define variable``.

Dynamic binding
---------------

The Threads module exports a macro for dynamic binding. A *binding* is a
mapping between a variable and a*value-cell* which holds the variable’s
value. A *dynamic* binding is a binding which has dynamic extent, and
shadows any outermost bindings. Dynamic bindings can be considered to be
a property of the dynamic environment.

Thread variables can have new dynamic bindings created for them with the
macro :macro:`dynamic-bind`. Thread variables inherently have
thread-local bindings, so it is possible to re-bind a thread variable
dynamically using the Dylan construct ``*block* … *cleanup*``. The
:macro:`dynamic-bind` macro can be implemented in this way.

The thread-local nature of dynamically bindable variables may not be
optimal for all problem domains. For instance a shared, global,
outermost binding may be desirable, or alternatively, a thread may want
to inherit current bindings from the parent thread at creation time,
giving a "fork"-type model of state inheritance. These alternatives are
not pursued in this module, but they might be an interesting area for
future research.

Thread safety in client libraries
=================================

If an application uses multiple threads, then there may be thread safety
requirements for any library that can be called simultaneously by
multiple threads, even if the called library does not use the Threads
library directly.

This section is about thread safety in any library that is designed to
be used in a multi-threaded application.

General requirements
--------------------

A library’s designer is responsible for documenting which features of
the library offer built-in synchronization and which do not. While there
is no definitive rule that can assist designers in this documentation,
the following guidelines may be useful.

If a client of the library forgets to use a synchronization feature when
one is necessary, the library designer should ensure that the effect of
the lack of synchronization is limited to a small unit — probably a
single object. In cases where the designer cannot guarantee that the
effect will be limited, the library should either implement the
synchronization internally, or provide a macro for clients to use
instead.

Library implementors must ensure that the library provides implicit
synchronization for any hidden global state which is maintained by the
library. Library designers may choose whether the library should offer
implicit synchronization of the state of objects managed by the library.
The interface is more convenient if the synchronization is implicit, but
it may be more efficient to rely on explicit synchronization by the
client. Library designers should always document the choice they make.

Effects on the Dylan library
----------------------------

The definition of the Dylan library is not changed with the addition of
the Threads module. The implementation ensures that all hidden global
state (such as the symbol table and any generic function caches) is
implicitly synchronized. Those functions in the Dylan library which are
defined to modify the state of objects are not defined to provide
implicit synchronization. However, implementations are expected to
ensure that synchronization bugs in Dylan programs will not cause
obscure errors that cannot be explained in terms of the semantics of
Dylan language constructs.

The library guarantees that *element* and *element-setter* will be
atomic for all of Dylan’s non-stretchy built-in collection classes, and
for :drm:`<table>`, except for subclasses of :drm:`<string>`, and limited
collections where the elements are constrained to be either of a type
for which slots and variables do not guarantee atomicity (see
`Atomicity`_) or a subtype of :drm:`<character>`, or of a proper subtype of
:drm:`<integer>`. This design is intended to permit implementations to use
efficient representations for element values, which use either more or
less space than a normal Dylan value. It is undefined whether any of
the other standard Dylan functions are atomic. Where atomicity is not
guaranteed, clients should guard against unexpected behavior by using
explicit synchronization, as appropriate.

The Threads class hierarchy
===========================

.. figure:: ../images/threads.png
   :align: center

   Threads class hierarchy.

* s - sealed  | o - open
* p - primary | f - free
* c - concrete | a - abstract
* u - uninstantiable | i - instantiable

Basic features
==============

This section documents basic features of the Threads module: operations
on threads and low-level synchronization.

Low-level synchronization
-------------------------

.. dylan:function:: sequence-point

   Tells the compiler that it must consider the possibility of visible side
   effects from other threads at the point of the call.

   :signature: sequence-point () => ()

   :description:

     Tells the compiler that it must consider the possibility of visible
     side effects from other threads at the point of the call.

     Normally, the compiler is not obliged to consider this possibility,
     and is free to rearrange program order provided that the reordering
     cannot be detected within a thread.

     Calling this function effectively prohibits the compiler from
     rearranging the order of reads or writes from or to global data,
     relative to the call. This function may disallow compiler
     optimizations, leading to less efficient code — even for strongly
     ordered machines.

.. function:: synchronize-side-effects

   As :func:`sequence-point`, with the addition that all side effects
   that have been performed within the calling thread are made visible
   within all other threads.

   :signature: synchronize-side-effects () => ()

   :description:

     A call to this function implies all the constraints to the compiler
     of a call to :func:`sequence-point`. In addition it ensures that
     all side effects that have been performed within the calling thread
     are made visible within all other threads. Hence, no side effect
     performed after the call can be visible to other threads before
     side effects performed before the call. On a strongly ordered
     machine, this function might legitimately be performed as a null
     operation.

     Some of the standard synchronization functions in the Threads
     module also ensure the visibility of side effects and act as
     sequence points, as if by a call to this function. This is defined
     to happen as follows:

     - Immediately before a thread exits and becomes available for
       joining with :func:`join-thread`
     - Before :func:`thread-yield` yields control
     - After :gf:`wait-for` achieves synchronization (for all methods
       provided by the Threads module)
     - Upon entry to :gf:`release` (for all methods provided by the
       Threads module)
     - Upon entry to :func:`release-all`

   :example:

     This example uses low-level synchronization to implement a class
     for performing lazy evaluation in a thread-safe manner, without the
     need for locks.

     The class guarantees that the value will not be computed until it
     is needed, although it does not guarantee that it will not be
     computed more than once concurrently. This might be useful for
     memorization purposes.

     The class uses 3 slots: one for a function which may be used to
     compute the value, one for a boolean indicating whether the value
     is already known, and one for the value itself, if known.

     It is essential that no instance can ever be observed in a state
     where the boolean indicates a known value before the value is
     present. The low-level synchronization functions ensure this cannot
     happen.

     .. code-block:: dylan

       define class <lazy-value> (<object>)
         slot thunk :: <function>,
           required-init-keyword: thunk:;
         slot internal-guard :: <boolean> = #t;
         slot computed-value;
       end class;

       define method lazy-value (lv :: <lazy-value>)
        => (value)
         if (lv.internal-guard)
           // Don’t yet have a value == so compute it now;
           let value = lv.thunk();
           // Store the value in place
           lv.computed-value := value;
           // Before dropping the guard, synchronize side
           // effects to ensure there is no possibility that
           // other threads might see the lowered guard
           // before seeing the value
           synchronize-side-effects();
           // Now we can drop the guard to permit other
           // threads to use this value
           lv.internal-guard := #f;
           // Finally, return the computed value
           value
         else // The value has already been computed and
           // stored, so use it
           // First, need a sequence-point to force the
           // compiler not to move the read of the
           // computed-value so that it is performed BEFORE
           // the read of the guard.
           sequence-point();
           lv.computed-value;
         end if;
       end method;

Operations on threads
---------------------

.. class:: <thread>
   :sealed:
   :instantiable:

   The class of threads.

   :superclasses: <object>
   :keyword function: An instance of :drm:`<function>`. Required.
   :keyword priority: A signed integer.
   :keyword name: An instance of :drm:`<string>`.

   :description:

     The class representing a thread of control executing *function*.

     The *function* is called with no arguments in the empty dynamic
     environment of the new thread. The thread terminates when the
     function returns.

     The function is executable immediately. You can suspend a new
     thread (almost) immediately on creation by arranging for it to
     synchronize on an unavailable resource upon entry to the function.

     The optional *priority* keyword provides a scheduling priority for
     the thread. The higher the value, the greater the priority. The
     default value is zero, which is also the value of the constant
     ``$normal-priority``, one of several constants that correspond to
     useful priority levels. The module offers no way to change the
     priority of a thread dynamically.

     The following constants, listed in order of increasing value, may
     be useful as values for the optional *priority* keyword.

     - $low-priority
     - $background-priority
     - $normal-priority
     - $interactive-priority
     - $high-priority

     The *name* keyword is a string that is used as the function’s name for
     convenience purposes, such as debugging.

   :operations:

     The class :class:`<thread>` provides the following
     operations:

     - :func:`thread-name` Returns the name of a thread, or ``#f`` if no name was
       supplied.
     - :func:`join-thread` Blocks until one of the specified threads has terminated,
       and returns the values of its function.

.. function:: thread-name

   Returns the name of a thread.

   :signature: thread-name *thread* => *name-or-false*

   :parameter thread: An instance of :class:`<thread>`.
   :value name-or-false: An instance of ``type-union(<string>, singleton(#f))``.

   :description:

     Returns the name of *thread* as a string. If *thread* does not have
     a name, this function returns ``#f``.

.. function:: join-thread

   Waits for another, existing, thread to terminate, and then returns the
   values of its function.

   :signature: join-thread *thread* #rest *threads* => *thread-joined* #rest *results*

   :parameter thread: An instance of :class:`<thread>`. A thread to
     join.
   :parameter #rest threads: Instances of :class:`<thread>`. More
     threads to join.

   :value thread-joined: An instance of :class:`<thread>`. The thread
     that was joined.
   :value #rest results: Zero or more instances of :drm:`<object>`. The
     values returned from the thread that was joined.

   :conditions:

     An implementation of *join-thread* is permitted to signal the following
     condition:

     ``<duplicate-join-error>``

     - A condition of this class (a subclass of :drm:`<error>`) may be
       signalled when a thread is passed to *join-thread*, if that
       thread has already been joined by an earlier call to
       *join-thread*, or if that thread is currently active in another
       call to *join-thread*.

   :description:

     Waits for another, existing, thread to terminate, by blocking if
     necessary, and then returns the values of its function. The
     function returns the thread object that was joined, along with any
     values its function returns.

     If more than one thread is passed to *join-thread*, the current
     thread blocks until the first of those threads terminates. The
     values returned are those of the first thread to terminate.

     If one or more of the multiple threads has already terminated at
     the time of the call, then one of those terminated threads is
     joined. When more than one thread has already terminated, it is
     undefined which of those threads the implementation will join.

     It is an error to pass a thread to *join-thread* if it has already
     been joined in a previous call to *join-thread*. It is an error to
     pass a thread to *join-thread* if that thread is also being
     processed by another simultaneous call to *join-thread* from
     another thread.

.. function:: thread-yield

   Force the current thread to yield control to the part of the
   implementation responsible for scheduling threads.

   :signature: thread-yield () => ()

   :description:

     Forces the current thread to yield control to the part of the
     implementation responsible for scheduling threads. Doing so may
     have the effect of allowing other threads to run, and may be
     essential to avoid deadlock in a co-operative scheduling
     environment.

.. function:: current-thread

   Returns the current thread.

   :signature: current-thread () => *thread*

   :value thread: An instance of :class:`<thread>`.

   :description:

     Returns the current thread.

Synchronization protocol
========================

Basic features
--------------

.. class:: <synchronization>
   :open:
   :abstract:

   The class of objects that are used for inter-thread synchronization.

   :superclasses: <object>

   :keyword name: An instance of :drm:`<string>`.

   :description:

     The class of objects that are used for inter-thread synchronization.

     There is no explicit mechanism in the module to block on a number
     of synchronization objects simultaneously, until synchronization
     can be achieved with one of them. This mechanism can be implemented
     by creating a new thread to wait for each synchronization object,
     and arranging for each thread to release a notification once
     synchronization has been achieved.

     The *name* keyword is a string that is used as the synchronization
     object’s name for convenience purposes, such as debugging.

   :operations:

     The class ``<synchronization>`` provides the following operations:

     - :gf:`wait-for` Block until synchronization can be achieved.
     - :gf:`release` Release the object to make it available for
       synchronization.
     - :gf:`synchronization-name` Returns the name of the
       synchronization object.

.. generic-function:: wait-for
   :open:

   Blocks until a synchronization object is available.

   :signature: wait-for *object* #key *timeout* => *success*

   :parameter object: An instance of :class:`<synchronization\>`.
   :parameter timeout: Time-out interval. If the value is ``#f``
      (the default), the time-out interval never elapses. Otherwise
      the value should be a :drm:`<real>`, corresponding to the desired
      interval in seconds.
   :value success: An instance of :drm:`<boolean>`.

   :description:

     Blocks until a synchronization object is available.

     This function is the basic blocking primitive of the Threads
     module. It blocks until *object* is available and synchronization
     can be achieved, or the *timeout* interval has expired. A
     non-blocking synchronization may be attempted by specifying a
     *timeout* of zero. Individual methods may adjust the state of the
     synchronization object on synchronization. The function returns
     ``#t`` if synchronization is achieved before the timeout interval
     elapses; otherwise it returns ``#f``.

.. generic-function:: release
   :open:

   Releases a synchronization object.

   :signature: release *object* #key => ()

   :parameter object: An instance of :class:`<synchronization>`.

   :description:

     Releases the supplied synchronization object, *object*, potentially
     making it available to other threads. Individual methods describe
     what this means for each class of synchronization. This function
     does not block for any of the subclasses of
     :class:`<synchronization>` provided by the module.

.. generic-function:: synchronization-name
   :open:

   Returns the name of a synchronization object.

   :signature: synchronization-name *object* => *name-or-false*

   :parameter object: An instance of :class:`<synchronization>`.
   :value name-or-false: An instance of
      ``type-union(<string>, singleton(#f))``.

   :description:

     Returns the name of the synchronization object, *object*, if it was
     created with the *name* init-keyword. Otherwise ``#f`` is returned.

Locks
-----

.. class:: <lock>
   :open:
   :abstract:
   :instantiable:

   The class of locks.

   :superclasses: :class:`<synchronization>`

   :description:

     Locks are synchronization objects which change state when they are
     *claimed* (using :gf:`wait-for`), and revert state when *released*
     (using :gf:`release`).

     It is normally necessary for programs to ensure that locks are
     released, otherwise there is the possibility of *deadlock*. Locks
     may be used to restrict the access of other threads to shared
     resources between the synchronization and the release. It is common
     for a protected operation to be performed by a body of code which
     is evaluated in a single thread between synchronization and
     release. A macro *`with-lock`_* is provided for this purpose. When
     a thread uses a lock for *mutual-exclusion* in this way, the thread
     is said to *own the lock*.

     ``<lock>`` has no direct instances; calling *make* on ``<lock>``
     returns an instance of :class:`<simple-lock>`.

   :operations:

     The class ``<lock>`` provides the following operations:

     - :macro:`with-lock` Execute a body of code between :gf:`wait-for` and
       :gf:`release` operations.

.. macro:: with-lock
   :statement:

   Holds a lock while executing a body of code.

   :macrocall:
     .. code-block:: dylan

       with-lock (*lock*, #key *keys*)
         *body*
       [failure *failure-expr* ]
       end

   :param lock: An instance of :class:`<lock>`.
   :param keys: Zero or more of the keywords provided by :gf:`wait-for`.
   :param body: A body of Dylan code.
   :value values: Zero or more instances of :drm:`<object>`.

   :conditions:

     *with-lock* may signal a condition of the following class (a
     subclass of :drm:`<serious-condition>`):

     ``<timeout-expired>``

     - This is signalled when *with-lock* did not succeed in claiming
       the lock within the timeout period.

   :description:

     Execute the *body* with *lock* held. If a *failure* clause is
     supplied, then it will be evaluated and its values returned from
     *with-lock* if the lock cannot be claimed (because a timeout
     occurred). The default, if no *failure* clause is supplied, is
     to signal an exception of class ``<timeout-expired>``. If there
     is no failure, *with-lock* returns the results of evaluating the
     body.

   :example:

     If no *failure* clause is supplied, the macro expands into code
     equivalent to the following:

     .. code-block:: dylan

       let the-lock = *lock*;
       if (wait-for(the-lock, *keys ...*))
         block ()
           *body*...
         cleanup
           release(the-lock)
         end block
       else
         signal(make(<timeout-expired>,
                     synchronization: the-lock)
       end if

Semaphores
----------

.. class:: <semaphore>
   :open:
   :instantiable:
   :primary:

   The class of traditional counting semaphores.

   :superclasses: :class:`<lock>`

   :keyword initial-count: A non-negative integer, corresponding to the
      initial state of the internal counter. The default value is 0.
   :keyword maximum-count: A non-negative integer corresponding to the
      maximum permitted value of the internal counter. The default value
      is the largest value supported by the implementation, which is the
      value of the constant ``$semaphore-maximum-count-limit``. This
      constant will not be smaller than 10000.

   :description:

     The ``<semaphore>`` class is a class representing a traditional
     counting semaphore. An instance of ``<semaphore>`` contains a
     counter in its internal state. Calling :meth:`release
     <release(<semaphore>)>` on a semaphore increments the internal
     count. Calling :meth:`wait-for <wait-for(<semaphore>)>` on a
     semaphore decrements the internal count, unless it is zero, in
     which case the thread blocks until another thread releases the
     semaphore.

     Semaphores are less efficient than exclusive locks, but they have
     asynchronous properties which may be useful (for example for
     managing queues or pools of shared resources). Semaphores may be
     released by any thread, so there is no built-in concept of a thread
     owning a semaphore. It is not necessary for a thread to release a
     semaphore after waiting for it — although semaphores may be used as
     locks if they do.

.. method:: wait-for
   :specializer: <semaphore>
   :sealed:

   Claims a semaphore object.

   :signature: wait-for *object* #key *timeout* => *success*

   :parameter object: An instance of :class:`<semaphore>`. The
      semaphore object to wait for.
   :parameter #key timeout: Time-out interval. If the value is ``#f``
      (the default), the time-out interval never elapses.
      Otherwise the value should be a :drm:`<real>`, corresponding
      to the desired interval in seconds.
   :value success: An instance of :drm:`<boolean>`.

   :description:

     Decrements the internal count of the semaphore object, blocking if
     the count is zero.

   See also

   :gf:`wait-for`.

.. method:: release
   :specializer: <semaphore>
   :sealed:

   Releases a semaphore object.

   :signature: release *object* #key => ()
   :parameter object: An instance of :class:`<semaphore>`.

   :conditions:

     An implementation of this *release* method is permitted to signal a
     condition of the following class, which is a subclass of
     :drm:`<error>`:

     ``<count-exceeded-error>``

     - This may be signalled when an attempt is made to release a
       :class:`<semaphore>` when the internal counter is already at its
       maximum count.

   :description:

     Releases a semaphore object, by incrementing its internal count.

   See also

   - :gf:`release`.

Exclusive locks
---------------

.. class:: <exclusive-lock>
   :open:
   :abstract:
   :instantiable:

   The class of locks which prohibit unlocking by threads that do not own
   the lock.

   :superclasses: :class:`<lock>`

   :description:

     The class of locks which prohibit unlocking by threads that do not
     own the lock.

     The notion of ownership is directly supported by the class, and a
     thread can test whether an ``<exclusive-lock>`` is currently owned.
     An instance of ``<exclusive-lock>`` can only be owned by one thread
     at a time, by calling *wait-for* on the lock.

     Once owned, any attempt by any other thread to wait for the lock
     will cause that thread to block. It is an error for a thread to
     release an ``<exclusive-lock>`` if another thread owns it.

     ``<exclusive-lock>`` has no direct instances; calling *make* on
     ``<exclusive-lock>`` returns an instance of :class:`<simple-lock>`.

   :operations:

     The class ``<exclusive-lock>`` provides the following operations:

     - :gf:`owned?` Tests to see if the lock has been claimed by the
       current thread.

.. method:: release
   :specializer: <exclusive-lock>
   :sealed:

   Releases an exclusive lock.

   :signature: release *object* #key => ()

   :parameter object: An instance of :class:`<exclusive-lock>`.

   :conditions:

     Implementations of *release* methods for subclasses of
     :class:`<exclusive-lock>` are permitted to signal a condition
     of the following class, which is a subclass of :drm:`<error>`:

     ``<not-owned-error>``

     - This may be signalled when an attempt is made to release an
       :class:`<exclusive-lock>` when the lock is not owned by the
       current thread.

   :description:

     Releases a lock that is owned by the calling thread. It is an error
     if the lock is not owned.

     The Threads module does not provide a method on *release* for
     :class:`<exclusive-lock>`, which is an open abstract class. Each
     concrete subclass will have an applicable method which may signal
     errors according to the protocol described above.

.. generic-function:: owned?
   :open:

   Tests whether an exclusive lock has been claimed by the current thread.

   :signature: owned? *object* => *owned?*

   :parameter object: An instance of :class:`<exclusive-lock>`.
   :value owned?: An instance of :drm:`<boolean>`.

   :description:

     Tests whether the exclusive lock has been claimed by the current
     thread.

Recursive locks
---------------

.. class:: <recursive-lock>
   :open:
   :instantiable:
   :primary:

   The class of locks that can be locked recursively.

   :superclasses: :class:`<exclusive-lock>`

   :description:

     A thread can lock a ``<recursive-lock>`` multiple times,
     recursively, but the lock must later be released the same number of
     times. The lock will be freed on the last of these releases.

.. method:: wait-for
   :specializer: <recursive-lock>
   :sealed:

   :summary: Claims a recursive lock.

   :signature: wait-for *object* #key *timeout* => *success*

   :parameter object: An instance of :class:`<recursive-lock>`.
   :parameter #key timeout: Time-out interval. If the value is ``#f``
      (the default), the time-out interval never elapses. Otherwise
      the value should be a :drm:`<real>`, corresponding to the desired
      interval in seconds.
   :value success: An instance of :drm:`<boolean>`.

   :description:

     Claims a recursive lock, blocking if it is owned by another thread.

   See also

   :gf:`wait-for`.

.. method:: release
   :specializer: <recursive-lock>
   :sealed:

   Releases a recursive lock.

   :signature: release *object* #key => ()

   :parameter object: An instance of :class:`<recursive-lock>`.

   :description:

     Releases a recursive lock, and makes it available if it has been
     released as many times as it was claimed with :meth:`wait-for
     <wait-for(<recursive-lock>)>`.

.. method:: owned?
   :specializer: <recursive-lock>
   :sealed:

   Tests whether a recursive lock has been claimed by the current thread.

   :signature: owned? *object* => *owned?*

   :parameter object: An instance of `<recursive-lock>`.
   :value owned?: An instance of :drm:`<boolean>`.

   :description:

     Tests whether a recursive lock has been claimed by the current
     thread.

Simple locks
------------

.. class:: <simple-lock>
   :open:
   :instantiable:
   :primary:

   A simple and efficient lock.

   :superclasses: :class:`<exclusive-lock>`

   :description:

     The ``<simple-lock>`` class represents the most simple and
     efficient mutual exclusion synchronization primitive. It is an
     error to lock a ``<simple-lock>`` recursively. An attempt to do so
     might result in an error being signalled, or deadlock occurring.

.. method:: wait-for
   :specializer: <simple-lock>
   :sealed:

   Claims a simple lock.

   :signature: wait-for *object* #key *timeout* => *success*

   :parameter object: An instance of :class:`<simple-lock>`.
   :parameter #key timeout: Time-out interval. If the value is ``#f``
     (the default), the time-out interval never elapses. Otherwise the
     value should be a :drm:`<real>`, corresponding to the desired interval
     in seconds.
   :value success: An instance of :drm:`<boolean>`.

   :description:

     Claims a simple lock, blocking if it is owned by another thread.

   See also

   :gf:`wait-for`.

.. method:: release
   :specializer: <simple-lock>
   :sealed:

   Releases a simple lock.

   :signature: release *object* #key => ()

   :parameter object: An instance of :class:`<simple-lock>`.

   :description:

     Releases a simple lock.

   See also

   :gf:`release`.

.. method:: owned?
   :specializer: <simple-lock>
   :sealed:

   Tests whether a simple lock has been claimed by the current thread.

   :signature: owned? *object* => *owned?*

   :parameter object: An instance of :class:`<simple-lock>`.
   :value owned?: An instance of :drm:`<boolean>`.

   :description:

     Tests whether a simple lock has been claimed by the current thread.

Multiple reader / single writer locks
-------------------------------------

.. class:: <read-write-lock>
   :open:
   :instantiable:
   :primary:

   The class of locks that can have multiple readers but only one writer.

   :superclasses: :class:`<exclusive-lock>`

   :description:

     The class of locks that can have multiple readers but only one
     writer.

     The ``<read-write-lock>`` class can be locked in either of two
     modes, *read* and *write*. A write lock is exclusive, and implies
     ownership of the lock. However, a read lock is non-exclusive, and
     an instance can be locked multiple times in read mode, whether by
     multiple threads, recursively by a single thread, or a combination
     of both.

     A ``<read-write-lock>`` can only be locked in write mode if the
     lock is free, and the operation will block if necessary. It can
     only be freed by the thread that owns it.

     A ``<read-write-lock>`` can be locked in read mode provided that
     it is not owned with a write lock. The operation will block while
     the lock is owned. Each time it is locked in read mode, an internal
     counter is incremented. This counter is decremented each time a
     read-mode lock is released. The lock is freed when the counter
     becomes zero.

     The ``<read-write-lock>`` class is less efficient than the other
     lock classes defined in the Threads module. However, it provides an
     efficient and convenient means to protect data that is frequently
     read and may occasionally be written by multiple concurrent
     threads.

.. method:: wait-for
   :specializer: <read-write-lock>
   :sealed:

   Claims a read-write lock.

   :signature: wait-for *object* #key *timeout* *mode*

   :parameter object: An instance of :class:`<read-write-lock>`.
   :parameter #key timeout: Time-out interval. If the value is ``#f``
     (the default), the time-out interval never elapses. Otherwise the
     value should be a :drm:`<real>`, corresponding to the desired interval
     in seconds.
   :parameter #key mode: The mode of the lock to wait for. Valid values
     are ``#"read"`` (the default) and ``#"write"``, which wait for locks
     in read mode and write mode respectively.
   :value success: An instance of :drm:`<boolean>`.

   :description:

     Claims a read-write lock, blocking if necessary. The behavior
     depends on the value of *mode*:

     - ``#"read"`` If there is a write lock, blocks until the lock
       becomes free. Then claims the lock by incrementing its internal
       read-lock counter.
     - ``#"write"`` First waits until the lock becomes free, by blocking
       if necessary. Then claims exclusive ownership of the lock in
       write mode.

     If the claim is successful, this method returns true; otherwise it
     returns false.

.. method:: release
   :specializer: <read-write-lock>
   :sealed:

   Releases a read-write-lock.

   :signature: release object #key => ()

   :parameter object: An instance of :class:`<read-write-lock>`.

   :description:

     Releases a read-write lock.

     If the lock is owned by the calling thread, it is freed. If the
     lock is locked in read mode, the count of the number of locks held
     is decremented; the lock is freed if the count becomes zero.
     Otherwise it is an error to release the lock, and an implementation
     is permitted to signal a ``<not-owned-error>`` condition.

.. method:: owned?
   :specializer: <read-write-lock>
   :sealed:

   Tests whether a read-write lock is owned — that is, has been locked
   in write mode — by the current thread.

   :signature: owned? *object* => *owned?*

   :parameter object: An instance of :class:`<read-write-lock>`.
   :value owned?: An instance of :drm:`<boolean>`.

   :description:

     Tests whether a read-write lock is owned — that is, has been locked
     in write mode — by the current thread.

Notifications
-------------

.. class:: <notification>
   :sealed:
   :instantiable:

   The class of objects that can be used to notify threads of a change
   of state elsewhere in the program.

   :superclasses: :class:`<synchronization>`

   :keyword lock: An instance of :class:`<simple-lock>`.
     Required.

   :description:

     The class of objects that can be used to notify threads of a change
     of state elsewhere in the program. Notifications are used in
     association with locks, and are sometimes called *condition
     variables*. They may be used to support the sharing of data between
     threads using *monitors*. Each ``<notification>`` is permanently
     associated with a :class:`<simple-lock>`, although the same lock
     may be associated with many notifications.

     The required *lock* is associated with the notification, and it is
     only possible to wait for, or release, the notification if the lock
     is owned.

     Threads wait for the change of state to be notified by calling
     :meth:`wait-for <wait-for(<notification>)>`. Threads notify other
     threads of the change of state by calling :meth:`release
     <release(<notification>)>`.

   :operations:

     The class ``<notification>`` provides the following operations:

     - :func:`associated-lock` Returns the lock associated with the
       notification object.
     - :gf:`wait-for` Wait for the notification of the change in state.
       The associated lock must be owned, and is atomically released
       before synchronization, and reclaimed after.
     - :gf:`release` Notify the change of state to a single waiting
       thread. This has no effect on the associated lock, which must be
       owned.
     - :func:`release-all` Notify the change of state to all waiting
       threads. This has no effect on the associated lock, which must be
       owned.

   :example:

     This example shows how to use a notification and an associated lock to
     implement a queue. The variable *\*queue\** is the actual queue object
     (a :drm:`<deque>`). Queue access is performed by interlocking pushes and
     pops on the :drm:`<deque>`. The *\*queue\** variable can be a constant,
     since it is the :drm:`<deque>` which is mutated and not the value of
     *\*queue\**.

     .. code-block:: dylan

       define constant *queue* = make(<deque>);

     The variable *\*lock\** is used to isolate access to the queue

     .. code-block:: dylan

       define constant *lock* = make(<lock>);

     The variable *\*something-queued\** is a notification which is used to
     notify other threads that an object is being put onto an empty queue.

     .. code-block:: dylan

       define constant *something-queued* =
         make(<notification>, lock: *lock*);

     The function *put-on-queue* pushes an object onto the queue. If the
     queue was initially empty, then all threads which are waiting for the
     queue to fill are notified that there is a new entry.

     .. code-block:: dylan

       define method put-on-queue (object) => ()
         with-lock (*lock*)
           if (*queue*.empty?)
             release-all(*something-queued*)
           end;
           push(*queue*, object)
         end with-lock
       end method;

     The *get-from-queue* function returns an object from the queue. If no
     object is immediately available, then it blocks until it receives a
     notification that the queue is no longer empty. After receiving the
     notification it tests again to see if an object is present, in case it
     was popped by another thread.

     .. code-block:: dylan

       define method get-from-queue () => (object)
         with-lock (*lock*)
           while (*queue*.empty?)
             wait-for(*something-queued*)
           end;
           pop(*queue*)
         end with-lock
       end method;

.. function:: associated-lock

   Returns the lock associated with the notification object supplied.

   :signature: associated-lock *notification* => *lock*

   :parameter notification: An instance of :class:`<notification>`.
   :value lock: An instance of :class:`<simple-lock>`.

   :description:

     Returns the lock associated with the notification object *notification*.

.. method:: wait-for
   :specializer: <notification>
   :sealed:

   Wait for another thread to release a notification.

   :signature: wait-for *notification* #key *timeout* => *success*

   :parameter notification: An instance of :class:`<notification>`.
   :parameter #key timeout: Time-out interval. If the value is ``#f``
     (the default), the time-out interval never elapses. Otherwise the
     value should be a :drm:`<real>`, corresponding to the desired interval
     in seconds.
   :value success: An instance of :drm:`<boolean>`.

   :description:

     Wait for another thread to release *notification*. The lock
     associated with the notification must be owned. Atomically, the
     lock is released and the current thread starts blocking, waiting
     for another thread to release the notification. The current thread
     reclaims the lock once it has received the notification.

     Note that the state should be tested again once *wait-for* has
     returned, because there may have been a delay between the
     :meth:`release <release(<notification>)>` of the notification and
     the claiming of the lock, and the state may have been changed
     during that time. If a timeout is supplied, then this is used for
     waiting for the release of the notification only. The *wait-for*
     function always waits for the lock with no timeout, and it is
     guaranteed that the lock will be owned on return. The *wait-for*
     function returns ``#f`` if the notification wait times out.

   :conditions:

     Implementations of this *wait-for* method are permitted to signal a
     condition of the following class, which is a subclass of :drm:`<error>`:

     ``<not-owned-error>``

     - Implementations can signal this error if the application attempts
       to wait for a notification when the associated lock is not owned by
       the current thread.

.. method:: release
   :specializer: <notification>
   :sealed:

   Releases a notification to one of the threads that are blocked and
   waiting for it.

   :signature: release *notification* #key => ()

   :parameter notification: An instance of :class:`<notification>`.

   :conditions:

     Implementations of this *release* method are permitted to signal a
     condition of the following class, which is a subclass of
     :drm:`<error>`:

     ``<not-owned-error>``

     - Implementations can signal this error if the application attempts
       to release a notification when the associated lock is not owned
       by the current thread.

   :description:

     Releases *notification*, announcing the change of state to one of
     the threads which are blocked and waiting for it. The choice of
     which thread receives the notification is undefined. The receiving
     thread may not be unblocked immediately, because it must first
     claim ownership of the notification’s associated lock.

.. function:: release-all

   Release a notification to all the threads that are blocked and waiting
   for it.

   :signature: release-all *notification* => ()

   :parameter notification: An instance of :class:`<notification>`.

   :conditions:

     Implementations of the *release-all* function are permitted to signal a
     condition of the following class, which is a subclass of :drm:`<error>`:

     ``<not-owned-error>``

     - This may be signalled when an attempt is made to release a
       notification when the associated lock is not owned by the current
       thread.

   :description:

     Releases *notification*, announcing the change of state to all
     threads which are blocked and waiting for it. Those threads will
     then necessarily have to compete for the lock associated with the
     notification.

Timers
======

.. function:: sleep

   Blocks the current thread for a specified number of seconds.

   :signature: sleep *interval* => ()

   :parameter interval: An instance of :drm:`<real>`.

   :description:

     Blocks the current thread for the number of seconds specified in
     *interval*.

Thread variables
================

.. macro:: thread
   :macro-type: variable definition adjective

   An adjective to *define variable* for defining thread variables.

   :macrocall:
     .. code-block:: dylan

       define thread variable *bindings* = *init* ;

   :description:

     An adjective to ``define variable``. The construct ``define thread
     variable`` defines module variables in the current module which
     have thread-local bindings. The initialization expression is
     evaluated once, and is used to provide the initial values for the
     variables in each thread. The value of a thread variable binding
     may be changed with the normal assignment operator ``:=``. This
     assignment is not visible in other threads.

   :example:

     .. code-block:: dylan

       define thread variable *standard-output*
         = make(<standard-output-stream>);

Dynamic binding
===============

.. macro:: dynamic-bind
   :statement:

   Executes a body of code in a context in which variables are dynamically
   rebound.

   :macrocall:
     .. code-block:: dylan

       dynamic-bind (*place1* = *init1*, *place2* = *init2*, ...) *body* end;

   :description:

     Executes *body* with the specified *places* rebound in the dynamic
     environment, each place being initialized to the results of
     evaluating the initialization expressions. In other words, the
     places are initialized to new values on entry to the body but
     restored to their old values once the body has finished executing,
     whether because it finishes normally, or because of a non-local
     transfer of control. Typically, each *place* is a thread variable.

     If the *place* is a *name*, it must be the name of a thread
     variable in the module scope.

   :example:

     The following example shows the dynamic binding of a single variable.

     .. code-block:: dylan

       dynamic-bind (*standard-output* = new-val())
         top-level-loop ()
       end;

     This expands into code equivalent to the following:

     .. code-block:: dylan

       begin
         let old-value = *standard-output*;
         block ()
           *standard-output* := new-val();
           top-level-loop()
         cleanup
           *standard-output* := old-value
         end
       end

An extended form of dynamic-bind
--------------------------------

Some implementations of the Threads module may provide an extended form
of *dynamic-bind* for binding places other than variables. The
implementation of this extended form requires the use of non-standard
features in the Dylan macro system, and hence cannot be written as a
portable macro. These non-standard extensions are subject to discussion
amongst the Dylan language designers, and may eventually become standard
features. Until such time as standardization occurs, implementations are
not mandated to implement the extended form of *dynamic-bind*, and
portable code should not depend upon this feature.

The extended form is described below.

dynamic-bind
------------

Statement macro
'''''''''''''''

Summary

Executes a body of code in a context in which variables or other places
are dynamically rebound.

Macro call

dynamic-bind (*place1* = *init1*, *place2* = *init2*, ...) body end;

(This is the same as the simple form.)

Description

If *place* is not a name, then it may have the syntax of a call to a
function. This permits an extended form for *dynamic-bind*, by analogy
with the extended form for *:=*. In this case, if the place appears
syntactically as *name(* *arg1* *,*... *argn* *)*, then the macro
expands into a call to the function

name-dynamic-binder(*init*, *body-method*, *arg1*, ... *argn*)

where *init* is the initial value for the binding, and *body-method* is
function with no parameters whose body is the *body of* the
*dynamic-bind*. The extended form also permits the other "*.* " and
"*[]* "syntaxes for function calls.

There are no features in the current version of the Threads module
which make use of the extended form of *dynamic-bind*.

Example

The following example shows the extended form of *dynamic-bind*.

.. code-block:: dylan

    dynamic-bind (object.a-slot = new-slot-val())
      inner-body(object)
    end;

This expands into code equivalent to the following:

.. code-block:: dylan

    a-slot-dynamic-binder(new-slot-val(),
                          method () inner-body(object) end,
                          object)

Locked variables
================

.. macro:: locked
   :macro-type: variable definition adjective

   Defines a locked variable.

   :macrocall:
     .. code-block:: dylan

       define locked variable *bindings* = *init* ;

   :description:

     An adjective to ``define variable``. The construct ``define locked
     variable`` defines module variables in the current module that can
     be tested and updated with :macro:`conditional-update!`,
     :macro:`atomic-increment!`, or :macro:`atomic-decrement!`.

     Other threads are prevented from modifying the locked variable
     during the conditional update operation by means of a low-level
     locking mechanism, which is expected to be extremely efficient.

   :operations:

     - `conditional-update!` Atomically compare and conditionally assign
       to the variable.
     - :macro:`atomic-increment!` Atomically increment the variable.
     - :macro:`atomic-decrement!`  Atomically decrement the variable.

   :example:

     .. code-block:: dylan

       define locked variable *number-detected* = 0;

Conditional update
==================

.. macro:: conditional-update!
   :statement:

   Performs an atomic test-and-set operation.

   :macrocall:

     .. code-block:: dylan

       conditional-update!(*local-name* = *place*)
         *body*
         [success *success-expr* ]
         [failure *failure-expr* ]
       end

   :parameter local-name: A Dylan variable-name*bnf*.
   :parameter place: A Dylan variable-namebnf,
     If the implementation provides the extended form of
     :macro:`conditional-update!`, *place* can also be a
     function call.
   :parameter body: A Dylan body *bnf*.
   :value value: See description.

   :description:

     Performs an atomic test-and-set operation. Where appropriate, it should
     be implemented using dedicated processor instructions, and is expected
     to be extremely efficient on most platforms.

     The value of the *place* is evaluated once to determine the initial
     value, which is then bound to the *local-name* as a lexical variable.
     The *body* is then evaluated to determine the new value for the place.
     The place is then conditionally updated — which means that the following
     steps are performed atomically:

     #. The place is evaluated again, and a test is made to see if it has
        been updated since the initial evaluation. This may involve a
        comparison with the old value using ``==``, though implementations
        might use a more direct test for there having been an assignment to
        the place. It is undefined whether the test will succeed or fail in
        the case where the place was updated with a value that is identical
        to the old value when compared using ``\\==``.
     #. If the value was found not to have been updated since the initial
        evaluation, the new value is stored by assignment. Otherwise the
        conditional update fails.

     If the update was successful, then ``conditional-update!`` returns the
     result of the *success* expression, or returns the new value of the
     place if no *success* clause was supplied.

     If the update failed, then *conditional-update!* signals a condition,
     unless a *failure* clause was given, in which case the value is
     returned.

     If the *place* is a *name*, it must be the name of a *locked variable*
     in the current module scope. See `Locked variables`_.

   :conditions:

     ``conditional-update!`` may signal a condition of the following class
     (which is a subclass of :drm:`<error>`), unless a *failure* clause is
     supplied.

     ``<conditional-update-error>``

   :example:

     The following example does an atomic increment of ``*number-detected*``.

     .. code-block:: dylan

       until (conditional-update!
                (current-val = *number-detected*)
                current-val + 1
                failure #f
              end conditional-update!)
       end until

.. macro:: atomic-increment!
   :function:

   Atomically increments a place containing a numeric value.

   :macrocall:
     .. code-block:: dylan

       atomic-increment!(*place*);

       atomic-increment!(*place*, *by*);

   :parameter place: A Dylan variable-namebnf.
     If the implementation provides the extended form of
     :macro:`conditional-update!`, *place* can also be a
     function call.
   :parameter by: An instance of :drm:`<object>`. Default value: 1.
   :value new-value: An instance of :drm:`<object>`.

   :description:

     Atomically increments a place containing a numeric value.

     The value of the *place* is evaluated one or more times to
     determine the initial value. A new value is computed from this
     value and *by*, by applying ``+`` from the Dylan module. The new
     value is atomically stored back into *place*.

     The macro returns the new value of *place*.

     The *place* must be a suitable place for
     :macro:`conditional-update!`.

     Implementations of :macro:`atomic-increment!` are permitted to use
     :macro:`conditional-update!` (as in the described example), and
     hence can involve a loop and can cause *place* to be evaluated more
     than once. However, an atomic increment of a locked variable might
     be implemented by a more efficient non-looping mechanism on some
     platforms.

   :example:

     The following example atomically increments ``*number-detected*``
     by 2, and returns the incremented value.

     .. code-block:: dylan

       atomic-increment!(*number-detected*, 2);

.. macro:: atomic-decrement!
   :function:

   Atomically decrements a place containing a numeric value.

   :macrocall:
     .. code-block:: dylan

       atomic-decrement!(*place*)

       atomic-decrement!(*place, by*)

   :parameter place: A Dylan variable-namebnf.
     If the implementation provides the extended form of
     :macro:`conditional-update!`, *place* can also be a
     function call.
   :parameter by: An instance of :drm:`<object>`. Default value: 1.
   :value new-value: An instance of :drm:`<object>`.

   :description:

     Atomically decrements a place containing a numeric value. It has
     the same semantics as :macro:`atomic-increment!` with the exception
     that the *place* is decremented.

An extended form of conditional-update!
---------------------------------------

Some implementations of the Threads module may provide an extended form
of *conditional-update!* for updating places other than locked
variables. The implementation of this extended form requires the use of
non-standard features in the Dylan macro system, and hence cannot be
written as a portable macro. These non-standard extensions are subject
to discussion amongst the Dylan language designers, and may eventually
become features. Until such time as standardization occurs,
implementations are not mandated to implement the extended form of
*conditional-update!*, and portable code should not depend upon the
feature.

conditional-update!
-------------------

Statement macro
'''''''''''''''

Summary

Performs an atomic test-and-set operation.

Macro call

::

    conditional-update!(*local-name* = *place*)
      *body*
      [success *success-expr* ]
      [failure *failure-expr* ]
    end

Arguments

-  *local-name* A Dylan variable-name*bnf*.
-  *place* A Dylan variable-namebnf or a function call.
-  *body* A Dylan body*bnf*.

Values

-  See Description.

Description

This extended form of *conditional-update!* additionally accepts a
*place* that has the syntax of a call to a function. This extended form
for *conditional-update!* is analogous to that for *:=*. In this case,
if the *place* appears syntactically as

*name* (*arg* 1, … *arg* n)

The macro expands into this call:

*name* -conditional-updater(*new-value*, *local-name*, *arg* 1, …
*arg* n)

If the result of this function call is ``#f``, the conditional update is
deemed to have failed.
