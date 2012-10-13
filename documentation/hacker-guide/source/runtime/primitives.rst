Runtime System Functions
************************

Primitive Functions for the threads library
===========================================

This section describes in detail the arguments, values, and operations
of the primitive functions.

Threads
-------

primitive-make-thread

[Primitive]

Signature

(thread :: <thread>, name :: <optional-name>, priority :: <integer>, function :: <function>) => ()

Arguments

*thread* A Dylan thread object.

*name* The name of the thread (as a ``<byte-string>``) or *#f*.

*priority* The priority at which the thread is to run.

*function* The initial function to run after the thread is created.

Description

Creates a new OS thread and destructively modifies the container slots
in the Dylan thread object with the handles of the new OS thread. The
new OS thread is started in a way which calls the supplied Dylan
function.

primitive-destroy-thread

[Primitive]

Signature

(thread :: <thread>) => ()

Arguments

*thread* A Dylan thread object.

Description

Frees any runtime-allocated memory associated with the thread.

primitive-initialize-current-thread

[Primitive]

Signature

(thread :: <thread>) => ()

Arguments

*thread* A Dylan thread object.

Description

The container slots in the Dylan thread object are destructively
modified with the handles of the current OS thread. This function will
be used to initialize the first thread, which will not have been started
as the result of a call to *primitive-make-thread*.

primitive-thread-join-single

[Primitive]

Signature

(thread :: <thread>) => (error-code :: <integer>)

Arguments

*thread* A Dylan thread object.

Values

*error-code* 0 = ok, anything else is an error, corresponding to a
multiple join.

Description

The calling thread blocks (if necessary) until the specified thread has
terminated.

primitive-thread-join-multiple

[Primitive]

Signature

(thread-vector :: <simple-object-vector>) => (result)

Arguments

*thread-vector* A ``<simple-object-vector>`` containing ``<thread>`` objects

Values

*result* The ``<thread>`` that was joined, if the join was successful;
otherwise, a ``<integer>`` indicating the error.

Description

The calling thread blocks (if necessary) until one of the specified
threads has terminated.

primitive-thread-yield

[Primitive]

Signature

() => ()

Description

For co-operatively scheduled threads implementations, the calling thread
yields execution in favor of another thread. This may do nothing in
some implementations.

primitive-current-thread

[Primitive]

Signature

() => (thread-handle)

Values

*thread-handle* A low-level handle corresponding to the current thread

Description

Returns the low-level handle of the current thread, which is assumed to
be in the handle container slot of one of the ``<thread>`` objects known
to the Dylan library. This result is therefore NOT a Dylan object. The
mapping from this value back to the ``<thread>`` object must be performed
by the Dylan threads library, and not the primitive layer, because the
``<thread>`` object is subject to garbage collection, and may not be
referenced from any low-level data structures.

Simple Locks
------------

primitive-make-simple-lock

[Primitive]

Signature

(lock :: <portable-container>, name :: <optional-name>) => ()

Arguments

*lock* A Dylan ``<simple-lock>`` object.

*name* The name of the lock (as a ``<byte-string>``) or *#f*.

Description

Creates a new OS lock and destructively modifies the container slot in
the Dylan lock object with the handle of the new OS lock.

primitive-destroy-simple-lock

[Primitive]

Signature

(lock :: <portable-container>) => ()

Arguments

*lock* A Dylan ``<simple-lock>`` object.

Description

Frees any runtime-allocated memory associated with the lock.

primitive-wait-for-simple-lock

[Primitive]

Signature

(lock :: <portable-container>) => (error-code :: <integer>)

Arguments

*lock* A Dylan ``<simple-lock>`` object.

Values

*error-code* 0 = ok

Description

The calling thread blocks until the specified lock is available
(unlocked) and then locks it. When the function returns, the lock is
owned by the calling thread.

primitive-wait-for-simple-lock-timed

[Primitive]

Signature

(lock :: <portable-container>, millisecs :: <integer>)
=> (error-code :: <integer>)

Arguments

*lock* A Dylan ``<simple-lock>`` object.

*millisecs* Timeout period in milliseconds

Values

*error-code* 0 = ok, 1 = timeout expired

Description

The calling thread blocks until either the specified lock is available
(unlocked) or the timeout period expires. If the lock becomes available,
this function locks it. If the function returns 0, the lock is owned by
the calling thread, otherwise a timeout occurred.

primitive-release-simple-lock

[Primitive]

Signature

(lock :: <portable-container>) => (error-code :: <integer>)

Arguments

*lock* A Dylan ``<simple-lock>`` object.

Values

*error-code* 0 = ok, 2 = not locked

Description

Unlocks the specified lock. The lock must be owned by the calling
thread, otherwise the result indicates “not locked”.

primitive-owned-simple-lock

[Primitive]

Signature

(lock :: <portable-container>) => (owned :: <integer>)

Arguments

*lock* A Dylan ``<simple-lock>`` object.

Values

*owned* 0= not owned, 1 = owned

Description

Returns 1 if the specified lock is owned (locked) by the calling thread.

Recursive Locks
---------------

primitive-make-recursive-lock

[Primitive]

Signature

(lock :: <portable-container>, name :: <optional-name>) => ()

Arguments

*lock* A Dylan ``<recursive-lock>`` object.

*name* The name of the lock (as a ``<byte-string>``) or *#f*.

Description

Creates a new OS lock and destructively modifies the container slot in
the Dylan lock object with the handle of the new OS lock.

primitive-destroy-recursive-lock

[Primitive]

Signature

(lock :: <portable-container>) => ()

Arguments

*lock* A Dylan``<recursive-lock>`` object.

Description

Frees any runtime-allocated memory associated with the lock.

primitive-wait-for-recursive-lock

[Primitive]

Signature

(lock :: <portable-container>) => (error-code :: <integer>)

Arguments

*lock* A Dylan ``<recursive-lock>`` object.

Values

*error-code* 0 = ok

Description

The calling thread blocks until the specified lock is available
(unlocked or already locked by the calling thread). When the lock
becomes available, this function claims ownership of the lock and
increments the lock count. When the function returns, the lock is
owned by the calling thread.

primitive-wait-for-recursive-lock-timed

[Primitive]

Signature

(lock :: <portable-container>, millisecs :: <integer>)
=> (error-code :: <integer>)

Arguments

*lock* A Dylan ``<recursive-lock>`` object.

*millisecs* Timeout period in milliseconds

Values

*error-code* 0 = ok, 1 = timeout expired

Description

The calling thread blocks until the specified lock is available
(unlocked or already locked by the calling thread). If the lock
becomes available, this function claims ownership of the lock,
increments an internal lock count, and returns 0. If a timeout
occurs, the function leaves the lock unmodified and returns 1.

primitive-release-recursive-lock

[Primitive]

Signature

(lock :: <portable-container>) => (error-code :: <integer>)

Arguments

*lock* A Dylan``<recursive-lock>`` object.

Values

*error-code* 0 = ok, 2 = not locked

Description

Checks that the lock is owned by the calling thread, and returns 2 if
not. If the lock is owned, its internal count is decremented by 1. If
the count is then zero, the lock is then released.

primitive-owned-recursive-lock

[Primitive]

Signature

(lock :: <portable-container>) => (owned :: <integer>)

Arguments

*lock* A Dylan ``<recursive-lock>`` object.

Values

*owned* 0= not owned, 1 = owned

Description

Returns 1 if the specified lock is locked and owned by the calling
thread.

Semaphores
----------

primitive-make-semaphore

[Primitive]

Signature

(lock :: <portable-container>, name :: <optional-name>,
 initial :: <integer>, max :: <integer>) => ()

Arguments

*lock* A Dylan ``<semaphore>`` object.

*name* The name of the lock (as a ``<byte-string>``) or *#f*.

*initial* The initial value for the semaphore count

Description

Creates a new OS semaphore with the specified initial count and
destructively modifies the container slot in the Dylan lock object with
the handle of the new OS semaphore.

primitive-destroy-semaphore

[Primitive]

Signature

(lock :: <portable-container>) => ()

Arguments

*lock* A Dylan ``<semaphore>`` object.

Description

Frees any runtime-allocated memory associated with the semaphore.

primitive-wait-for-semaphore

[Primitive]

Signature

(lock :: <portable-container>) => (error-code :: <integer>)

Arguments

*lock* A Dylan ``<semaphore>`` object.

Values

*error-code* 0 = ok

Description

The calling thread blocks until the internal count of the specified
semaphore becomes greater than zero. It then decrements the semaphore
count.

primitive-wait-for-semaphore-timed

[Primitive]

Signature

(lock :: <portable-container>, millisecs :: <integer>)
=> (error-code :: <integer>)

Arguments

*lock* A Dylan ``<semaphore>`` object.

*millisecs* Timeout period in milliseconds

Values

*error-code* 0 = ok, 1 = timeout expired

Description

The calling thread blocks until either the internal count of the
specified semaphore becomes greater than zero or the timeout period
expires. In the former case, the function decrements the semaphore count
and returns 0. In the latter case, the function returns 1.

primitive-release-semaphore

[Primitive]

Signature

(lock :: <portable-container>) => (error-code :: <integer>)

Arguments

*lock* A Dylan ``<semaphore>`` object.

Values

*error-code* 0 = ok, 3 = count exceeded

Description

This function checks that internal count of the semaphore is not at its
maximum limit, and returns 3 if the test fails. Otherwise the internal
count is incremented.

Notifications
-------------

primitive-make-notification

[Primitive]

Signature

(notification :: <portable-container>, name :: <optional-name>) => ()

Arguments

*notification* A Dylan <*notification>* object.

*name* The name of the notification (as a ``<byte-string>``) or *#f*.

Description

Creates a new OS notification (condition variable) and destructively
modifies the container slot in the Dylan lock object with the handle of
the new OS notification.

primitive-destroy-notification

[Primitive]

Signature

(notification :: <portable-container>) => ()

Arguments

*notification* A Dylan ``<notification>`` object.

Description

Frees any runtime-allocated memory associated with the notification.

primitive-wait-for-notification

[Primitive]

Signature

(notification :: <portable-container>, lock :: <portable-container>)
=> (error-code :: <integer>)

Arguments

*notification* A Dylan ``<notification>`` object.

*lock* A Dylan ``<simple-lock>`` object.

Values

*error-code* 0 = ok, 2 = not locked, 3 = other error

Description

The function checks that the specified lock is owned by the calling
thread, and returns 2 if the test fails. Otherwise, the calling thread
atomically releases the lock and then blocks, waiting to be notified of
the condition represented by the specified notification. When the
calling thread is notified of the condition, the function reclaims
ownership of the lock, blocking if necessary, before returning 0.

primitive-wait-for-notification-timed

[Primitive]

Signature

(notification :: <portable-container>, lock :: <portable-container>,
 millisecs :: <integer>) => (error-code :: <integer>)

Arguments

*notification* A Dylan ``<notification>`` object.

*lock* A Dylan ``<simple-lock>`` object.

*millisecs* Timeout period in milliseconds

Values

*error-code* 0 = ok, 1 = timeout, 2 = not locked, 3 = other error

Description

The function checks that the specified lock is owned by the calling
thread, and returns 2 if the test fails. Otherwise, the calling thread
atomically releases the lock and then blocks, waiting to be notified of
the condition represented by the specified notification, or for the
timeout period to expire. The function then reclaims ownership of the
lock, blocking indefinitely if necessary, before returning either 0 or 1
to indicate whether a timeout occurred.

primitive-release-notification

[Primitive]

Signature

(notification :: <portable-container>, lock :: <portable-container>)
=> (error-code :: <integer>)

Arguments

*notification* A Dylan ``<notification>`` object.

*lock* A Dylan ``<simple-lock>`` object.

Values

*error-code* 0 = ok, 2 = not locked

Description

If the calling thread does not own the specified lock, the function
returns the error value 2. Otherwise, the function releases the
specified notification, notifying another thread that is blocked waiting
for the notification to occur. If more than one thread is waiting for
the notification, it is unspecified which thread is notified. If no
threads are waiting, then the release has no effect.

primitive-release-all-notification

[Primitive]

Signature

(notification :: <portable-container>, lock :: <portable-container>)
=> (error-code :: <integer>)

Arguments

*notification* A Dylan ``<notification>`` object.

*lock* A Dylan ``<simple-lock>`` object.

Values

*error-code* 0 = ok, 2 = not locked

Description

If the calling thread does not own the specified lock, the function
returns the error value 2. Otherwise, the function releases the
specified notification, notifying all other threads that are blocked
waiting for the notification to occur. If no threads are waiting, then
the release has no effect.

Timers
------

primitive-sleep

[Primitive]

Signature

(millisecs :: <integer>) => ()

Arguments

*millisecs* Time interval in milliseconds

Description

This function causes the calling thread to block for the specified time
interval.

Thread Variables
----------------

primitive-allocate-thread-variable

[Primitive]

Signature

(initial-value) => (handle-on-variable)

Arguments

*initial-value* A Dylan object that is to be the initial value of the
fluid variable.

Values

*handle-on-variable* An OS handle on the fluid variable, to be stored
as the immediate value of the variable. Variable reading and assignment
will indirect through this handle. The handle is not a Dylan object.

Description

This function creates a new thread-local variable handle, and assigns
the specified initial value to the location indicated by the handle. The
function must arrange to assign the initial value to the thread-local
location associated with all other existing threads, too. The function
must also arrange that whenever a new thread is subsequently created, it
also has its thread-local location indicated by the handle set to the
initial value.

Simple Runtime Primitives
=========================

.. c:function:: D primitive_allocate(int size)

    This is the interface to the memory allocator which might be dependent
    on the garbage collector. It takes a size in bytes as a parameter, and
    returns some freshly allocated memory which the run-time system knows
    how to memory-manage.

.. c:function:: D primitive_byte_allocate(int word-size, int byte-size)

    This is built on the same mechanism as `primitive_allocate`:c:func:,
    but it is specifically designed for allocating objects which have Dylan
    slots, but also have a repeated slot of byte-sized elements, such as a
    byte string, or a byte vector. It takes two parameters, a size in ‘words’
    for the object slots (e.g., one for ‘class’ and a second for ‘size’),
    followed by the number of bytes for the vector. The value returned from
    the primitive is the freshly allocated memory making up the string.

.. c:function:: D primitive_fill_E_ (D storage[], int size, D value)

    (The odd name is a result of name mangling from ``primitive-fill!``).
    This takes a Dylan object (or a pointer to the middle of one), a size,
    and a value. It inserts the value into as many slots as are specified by
    *size*.

.. c:function:: D primitive_replace_E_ (D dst[], D src[], int size)

    (See `primitive_fill_E_`:c:func: re. name). This copies from the source
    vector into the destination vector as many values as are specified in
    the *size* parameter.

.. c:function:: D primitive_replace_vector_E_ (SOV* dest, SOV* source)

    This is related to `primitive_replace_E_`:c:func:, except that the two
    arguments are guaranteed to be simple object vectors, and they are
    self-sizing. It takes two parameters, ‘dest’, and ‘source’, and the data
    from ‘source’ is copied into ‘dest’. ‘Dest’ is returned.

.. c:function:: D primitive_allocate_vector (int size)

    This is related to `primitive_allocate`:c:func:, except that it takes
    a ‘size’ argument, which is the size of repeated slots in a simple object
    vector (SOV). An object which is big enough to hold the specified indices
    is allocated, and appropriately initialized, so that the ‘class’ field
    shows that it is an SOV, and the ‘size’ field shows how big it is.

.. c:function:: D primitive_copy_vector(D vector)

    This takes a SOV as a parameter, and allocates a fresh SOV of the same
    size. It copies all the data that was supplied from the old one to the
    new one, and returns the new one.

.. c:function:: D primitive_initialize_vector_from_buffer (SOV * vector, int size, D* buffer)

    This primitive takes a pre-existing vector, and copies data into it from
    a buffer so as to initialize an SOV. The primitive takes a SOV to be
    updated, a ‘size’ parameter (the specified size of the SOV), and a
    pointer to a buffer which will supply the necessary data. The class and
    size values for the new SOV are set, and the data written to the rest of
    the SOV. The SOV is returned.

.. c:function:: D primitive_make_string(char * string)

   This takes as a parameter a ‘C’ string with is zero-terminated, and
   returns a Dylan string with the same data inside it.

.. c:function:: D primitive_continue_unwind ()

   This is used as the last thing to be done at the end of an
   unwind-protect cleanup. It is responsible for determining why the
   cleanup is being called, and thus taking appropriate action afterwards.

   It handles 2 basic cases:

   -  a non-local exit
   -  a normal unwind-protect

   In the first case we wish to transfer control back to some other
   location, but there is a cleanup that needs to be done first. In this
   case there will be an unwind-protect frame on the stack which contains a
   marker to identify the target of the non-local exit. Control can thus be
   transferred, possibly invoking another unwind-protect on the way.

   Alternatively, no transfer of control may be required, and
   unwind-protect can proceed normally. As a result of evaluating our
   protected forms, the multiple values of these forms are stored in the
   unwind-protect frame. These values are put back in the multiple values
   area, and control is returned.

.. c:function:: D primitive_nlx (Bind_exit_frame* target, SOV* arguments)

    This takes two parameters: a bind-exit frame which is put on the stack
    whenever a bind-exit frame is bound, and an SOV of the multiple values
    that we wish to return to that bind-exit point. We then step to the
    bind-exit frame target, while checking to see if there are any
    intervening unwind-protect frames. If there are, we put the marker for
    our ultimate destination into the unwind-protect frame that has been
    detected on the stack between us and our destination. The multiple
    values we wish to return are put into the unwind-protect frame. The
    relevant cleanup code is invoked, and at the end of this a
    `primitive_continue_unwind`:c:func: should be called. This should
    detect that there is further to go, and insert the multiple values
    into any intervening frames.

.. c:function:: D primitive_inlined_nlx (Bind_exit_frame* target, D first_argument)

    This is similar to `primitive_nlx`:c:func:, except that it is used when the
    compiler has been able to gain more information about the circumstances
    in which the non-local-exit call is happening. In particular it is used
    when it is possible to in-line the call, so that the multiple values
    that are being passed are known to be in the multiple values area,
    rather than having been created as an SOV. An SOV has to be built up
    from these arguments.

.. c:function:: D* primitive_make_box(D object)

    A box is a value-cell that is used for closed-over variables which are
    subject to assignment. The function takes a Dylan object, and returns a
    value-cell box which contains the object. The compiler deals with the
    extra level of indirection needed to get the value out of the box.

.. c:function:: D* primitive_make_environment(int size, …)

    This is the function which makes the vector which is used in a closure.
    The arguments to this are either boxes, or normal Dylan objects. This
    takes an argument of ‘size’ for the initial arguments to be closed over,
    plus the arguments themselves. ‘Size’ arguments are built up into an SOV
    which is used as an environment.

Entry Point Functions
=====================

.. c:function:: D xep_0 (FN* function, int argument_count)
.. c:function:: D xep_1 (FN* function, int argument_count)
.. c:function:: D xep_2 (FN* function, int argument_count)
.. c:function:: D xep_3 (FN* function, int argument_count)
.. c:function:: D xep_4 (FN* function, int argument_count)
.. c:function:: D xep_5 (FN* function, int argument_count)
.. c:function:: D xep_6 (FN* function, int argument_count)
.. c:function:: D xep_7 (FN* function, int argument_count)
.. c:function:: D xep_8 (FN* function, int argument_count)
.. c:function:: D xep_9 (FN* function, int argument_count)

    These are the XEP entry-point handlers for those Dylan functions which
    do not accept optional parameters. Each Dylan function has an external
    (safe) entry point with full checking. After checking, this calls the
    internal entry point, which is the most efficient available.

    The compiler itself only ever generates code for the internal entry
    point. Any value put into the external entry point field of an object is
    a shared value provided by the runtime system. If the function takes no
    parameters, the value will be ``xep0``; if it takes a single required
    parameter it will be ``xep1``, and so on. There are values available for
    ``xep0`` to ``xep9``. For more than nine required parameters, the
    `xep`:c:func: function is used.

.. c:function:: xep (FN* function, int argument_count, …)

    If the function takes more than nine required parameters, then the
    function will simply be called ``xep``, the general function which will
    work in all such cases. The arguments are passed as ‘varargs’. This
    function will check the number of arguments, raising an error if it is
    wrong. It then sets the calling convention for calling the internal
    entry point. This basically means that the function register is
    appropriately set, and the implementation ‘mlist’ parameter is set to
    ``#f``.

.. c:function:: D optional_xep (FN* function, int argument_count, …)

    This function is used as the XEP code for any Dylan function which has
    optional parameters. In this case, the external entry point conventions
    do not require the caller to have any knowledge of where the optionals
    start. The XEP code is thus responsible for separating the code into
    those which are required parameters, to be passed via the normal machine
    conventions, and those which are optionals. to be passed as a Dylan SOV.
    If the function object takes keywords, all the information about which
    keywords are accepted is stored in the function itself. The vector of
    optional parameters is scanned by the XEP code to see if any appropriate
    ones have been supplied. If one is found, then the associated value is
    taken and used as an implicit parameter to the internal entry point. If
    a value is not supplied, then a suitable default parameter which is
    stored inside the function object is passed instead.

.. c:function:: D gf_xep_0(FN* function, int argument_count)
.. c:function:: D gf_xep_1(FN* function, int argument_count)
.. c:function:: D gf_xep_2(FN* function, int argument_count)
.. c:function:: D gf_xep_3(FN* function, int argument_count)
.. c:function:: D gf_xep_4(FN* function, int argument_count)
.. c:function:: D gf_xep_5(FN* function, int argument_count)
.. c:function:: D gf_xep_6(FN* function, int argument_count)
.. c:function:: D gf_xep_7(FN* function, int argument_count)
.. c:function:: D gf_xep_8(FN* function, int argument_count)
.. c:function:: D gf_xep_9(FN* function, int argument_count)

    These primitives are similar to `xep_0`:c:func: through `xep_9`:c:func:,
    but deal with the entry points for generic functions. Generic functions
    do not require the ‘mlist’ parameter to be set, so a special optimized
    entry point is provided. These versions are for 0 - 9 required
    parameters.  These functions call the internal entry point.

.. c:function:: D gf_xep (FN* function, int argument_count, …)

    This primitive is similar to `xep`:c:func:, but deals with the entry
    points for generic functions. Generic functions do not require the
    ‘mlist’ parameter to be set, so a special optimized entry point is
    provided. This is the general version for functions which do not
    take optional arguments. This function calls the internal entry point.

.. c:function:: D gf_optional_xep (FN* function, int argument_count, …)

    This is used for all generic functions which take optional arguments.
    This function calls the internal entry point.

.. c:function:: D primitive_basic_iep_apply (FN* f, int argument_count, D a[])

    This is used to call internal entry points. It takes three parameters: a
    Dylan function object (where the iep is stored in a slot), an argument
    count of the number of arguments that we are passing to the iep, and a
    vector of all of these arguments. This is a ‘basic’ IEP apply because is
    does no more than check the argument count, and call the IEP with the
    appropriate number of Dylan parameters. It does not bother to set any
    implementation parameters. Implementation parameters which could be set
    in by other primitives are ‘function’, and a ‘mlist’ (the list of
    next-methods) . Not all IEPs care about the ‘function’ or ‘mlist’
    parameters, but when the compiler calls `primitive_basic_iep_apply`:c:func:,
    it has to make sure that any necessary ‘function’ or ‘mlist’ parameters
    have been set up.

.. c:function:: D primitive_iep_apply (FN* f, int argument_count, D a[])

    This is closely related to `primitive_basic_iep_apply`:c:func:. It takes
    the same number of parameters, but it sets the explicit,
    implementation-dependent function parameter which is usually set to the
    first argument, and also sets the ‘mlist’ argument to ‘false’. This is
    the normal case when a method object is being called directly, rather
    than as part of a generic function.

.. c:function:: D primitive_xep_apply (FN* f, int argument_count, D a[])

    This is a more usual usage of apply, i.e., the standard Dylan calling
    convention being invoked by *apply*. It takes three parameters: the
    Dylan function to be called, the number of arguments being passed, and a
    vector containing all those arguments. This primitive relates to the
    external entry point for the function, and guarantees full type checking
    and argument count checking. This primitive does all that is necessary
    to conform with the xep calling convention of Dylan: i.e., it sets the
    ‘function’ parameter, it sets the argument count, and then calls the XEP
    for the function.

Compiler Primitives
*******************

General Primitives
==================

primitive-make-box

[Primitive]

Signature

(object :: <object>) => <object>

primitive-allocate

[Primitive]

Signature

(size :: <raw-small-integer>) => <object>)

primitive-byte-allocate

[Primitive]

Signature

(word-size :: <raw-small-integer>, byte-size :: <raw-small-integer>) =>
<object>)

primitive-make-environment

[Primitive]

Signature

(size :: <raw-small-integer>) => <object>

primitive-copy-vector

[Primitive]

Signature

(vector :: <object>) => <object>

primitive-make-string

[Primitive]

Signature

(vector :: <raw-c-char\*>) => <raw-c-char\*>

primitive-function-code

[Primitive]

Signature

(function :: <object>) => <object>

primitive-function-environment

[Primitive]

Signature

(function :: <object>) => <object>

Low-Level Apply Primitives
==========================

primitive-xep-apply

[Primitive]

Signature

(function :: <object>, buffer-size :: <raw-small-integer>, buffer ::
<object>) => :: <object>

primitive-iep-apply

[Primitive]

Signature

(function :: <object>, buffer-size :: <raw-small-integer>, buffer ::
<object>) => <object>)

primitive-true?

[Primitive]

Signature

(value :: <raw-small-integer>) => <object>

Description

This primitive returns Dylan true if *value* is non-zero, and false if
*value* is zero.

primitive-false?

[Primitive]

Signature

(value :: <raw-small-integer>) => <object>

Description

This is the complement of *primitive-true?*, returning *#t* if the
value is 0, *#f* otherwise.

primitive-equals?

[Primitive]

Signature

(x :: <object>, y :: <object>) => <raw-c-int>

primitive-continue-unwind

[Primitive]

Signature

() => <object>

primitive-nlx

[Primitive]

Signature

(bind-exit-frame :: <raw-c-void\*>, args :: <raw-c-void\*>) =>
<raw-c-void>

primitive-inlined-nlx

[Primitive]

Signature

(bind-exit-frame :: <raw-c-void\*>, first-argument :: <raw-c-void\*>) =>
<raw-c-void>

rimitive-variable-lookup

[Primitive]

Signature

(variable-pointer :: <raw-c-void\*>) => <raw-c-void\*>

primitive-variable-lookup-setter

[Primitive]

Signature

(value :: <raw-c-void\*>, variable-pointer :: <raw-c-void\*>) =>
<raw-c-void\*>

Integer Primitives
==================

primitive-int?

[Primitive]

Signature

(x :: <object>) => <raw-small-integer>

primitive-address-equals?

[Primitive]

Signature

(x :: <raw-address>, y :: <raw-address>) => <raw-address>

primitive-address-add

[Primitive]

Signature

(x :: <raw-address>, y :: <raw-address>) => <raw-address>

primitive-address-subtract

[Primitive]

Signature

(x :: <raw-address>, y :: <raw-address>) => <raw-address>

primitive-address-multiply

[Primitive]

Signature

(x :: <raw-address>, y :: <raw-address>) => <raw-address>

primitive-address-left-shift

[Primitive]

Signature

(x :: <raw-address>, y :: <raw-address>) => <raw-address>

primitive-address-right-shift

[Primitive]

Signature

(x :: <raw-address>, y :: <raw-address>) => <raw-address>

primitive-address-not

[Primitive]

Signature

(x :: <raw-address>) => <raw-address>

primitive-address-and

[Primitive]

Signature

(x :: <raw-address>, y :: <raw-address>) => <raw-address>

primitive-address-or

[Primitive]

Signature

(x :: <raw-address>, y :: <raw-address>) => <raw-address>

primitive-small-integer-equals?

[Primitive]

Signature

(x :: <raw-small-integer>, y :: <raw-small-integer>) =>
<raw-small-integer>

primitive-small-integer-not-equals?

[Primitive]

Signature

(x :: <raw-small-integer>, y :: <raw-small-integer>) =>
<raw-small-integer>

primitive-small-integer-less-than?

[Primitive]

Signature

(x :: <raw-small-integer>, y :: <raw-small-integer>) =>
<raw-small-integer>

primitive-small-integer-greater-than?

[Primitive]

Signature

(x :: <raw-small-integer>, y :: <raw-small-integer>) =>
<raw-small-integer>

primitive-small-integer-greater-than-or-equal?

[Primitive]

Signature

(x :: <raw-small-integer>, y :: <raw-small-integer>) =>
<raw-small-integer>

primitive-small-integer-negate

[Primitive]

Signature

(x :: <raw-small-integer>) => <raw-small-integer>

primitive-small-integer-add

[Primitive]

Signature

(x :: <raw-small-integer>, y :: <raw-small-integer>) =>
<raw-small-integer>

primitive-small-integer-subtract

[Primitive]

Signature

(x :: <raw-small-integer>, y :: <raw-small-integer>) =>
<raw-small-integer>

primitive-small-integer-multiply

[Primitive]

Signature

(x :: <raw-small-integer>, y :: <raw-small-integer>) =>
<raw-small-integer>

primitive-small-integer-divide

[Primitive]

Signature

(x :: <raw-small-integer>, y :: <raw-small-integer>) =>
<raw-small-integer>

primitive-small-integer-modulo

[Primitive]

Signature

(x :: <raw-small-integer>, y :: <raw-small-integer>) =>
<raw-small-integer>

primitive-small-integer-left-shift

[Primitive]

Signature

(x :: <raw-small-integer>, y :: <raw-small-integer>) =>
<raw-small-integer>

primitive-small-integer-right-shift

[Primitive]

Signature

(x :: <raw-small-integer>, y :: <raw-small-integer>) =>
<raw-small-integer>

primitive-small-integer-not

[Primitive]

Signature

(x :: <raw-small-integer>) => <raw-small-integer>

primitive-small-integer-and

[Primitive]

Signature

(x :: <raw-small-integer>, y :: <raw-small-integer>) =>
<raw-small-integer>

primitive-small-integer-or

[Primitive]

Signature

(x :: <raw-small-integer>, y :: <raw-small-integer>) =>
<raw-small-integer>

primitive-small-integer-xor

[Primitive]

Signature

(x :: <raw-small-integer>, y :: <raw-small-integer>) =>
<raw-small-integer>

In addition to the small-integer operators above, there are also
definitions for three other integer types, defined in the same manner.
The following table summarizes the relationship between these types and
Dylan primitives.

Integer Types and Dylan Primitives
                                  
+------------------+---------------------------+----------------------------------+
| General Variety  | Class of Primitive        | Value of *type* in Primitive     |
| of Integer       | Parameters and Return     | Name primitive-*type*-*operator* |
|                  | Values                    |                                  |
+==================+===========================+==================================+
| Small Integer    | ``<raw-small-integer>``   | *small-integer*                  |
+------------------+---------------------------+----------------------------------+
| Big Integer      | ``<raw-big-integer>``     | *big-integer*                    |
+------------------+---------------------------+----------------------------------+
| Machine Integer  | ``<raw-machine-integer>`` | *machine-integer*                |
+------------------+---------------------------+----------------------------------+
| Unsigned Machine | ``<raw-unsigned-machine-  | *unsigned-machine-integer*       |
| Integer          | integer>``                |                                  |
+------------------+---------------------------+----------------------------------+

Float Primitives
================

primitive-decoded-bits-as-single-float

[Primitive}

Signature

(sign :: <raw-small-integer>, exponent :: <raw-small-integer>,
 significand :: <raw-small-integer>) => <raw-single-float>)

primitive-bits-as-single-float

[Primitive]

Signature

(x :: <raw-small-integer>) => <raw-single-float>

Description

Uses a custom emitter to map to a call to a function called
*integer\_to\_single\_float* in the runtime system.

primitive-single-float-as-bits

[Primitive]

Signature

(x :: <raw-single-float>) => <raw-small-integer>

Description

Uses a custom emitter to map to a call to a function called
*single\_float\_to\_integer* in the runtime system.

primitive-single-float-equals?

[Primitive]

Signature

(x :: <raw-single-float>, y :: <raw-single-float>) => <raw-c-int>

primitive-single-float-not-equals?

[Primitive]

Signature

(x :: <raw-single-float>, y :: <raw-single-float>) => <raw-c-int>

primitive-single-float-less-than?

[Primitive]

Signature

(x :: <raw-single-float>, y :: <raw-single-float>) => <raw-c-int>

primitive-single-float-less-than-or-equal?

[Primitive]

Signature

(x :: <raw-single-float>, y :: <raw-single-float>) => <raw-c-int>

primitive-single-float-greater-than?

[Primitive]

Signature

(x :: <raw-single-float>, y :: <raw-single-float>) => <raw-c-int>

primitive-single-float-greater-than-or-equal?

[Primitive]

Signature

(x :: <raw-single-float>, y :: <raw-single-float>) => <raw-c-int>

primitive-single-float-negate

[Primitive]

Signature

(x :: <raw-single-float>) => <raw-single-float>

primitive-single-float-add

[Primitive]

Signature

(x :: <raw-single-float>, y :: <raw-single-float>) => <raw-single-float>

primitive-single-float-subtract

[Primitive]

Signature

(x :: <raw-single-float>, y :: <raw-single-float>) => <raw-single-float>

primitive-single-float-multiply

[Primitive]

Signature

(x :: <raw-single-float>, y :: <raw-single-float>) => <raw-single-float>

primitive-single-float-divide

[Primitive]

Signature

(x :: <raw-single-float>, y :: <raw-single-float>) => <raw-single-float>

primitive-single-float-unary-divide

[Primitive]

Signature

(x :: <raw-single-float>>) => <raw-single-float>

Accessor Primitives
===================

primitive-element

[Primitive]

Signature

(array :: <object>, index :: <raw-small-integer>) => <object>

Description

This is used for de-referencing slots in the middle of Dylan objects,
and thus potentially invokes read-barrier code. It takes two parameters:
a Dylan object, and an index which is the ‘word’ index into the object.
It returns the Dylan value found in that corresponding slot.

primitive-element-setter

[Primitive]

Signature

(new-value :: <object>, array :: <object>, index :: <raw-small-integer>)
=> <object>

Description

This is the assignment operator corresponding to *primitive-element*,
which is used to change the value of a Dylan slot. This takes an extra
initial parameter which is the new value to put into the object. The new
value is stored in the appropriate object at the given index.

primitive-byte-element

[Primitive]

Signature

(array <object>, base-index :: <raw-small-integer>, byte-offset ::
<raw-small-integer>) => <raw-c-char>

Description

This is similar to *primitive-element*, but deals with byte vectors. It
takes a new value and a Dylan object, along with a base offset and a
byte offset. The base offset, expressed in words, and the byte offset,
expressed in bytes, are added, and the byte found at that location is
returned.

primitive-byte-element-setter

[Primitive]

Signature

(new-value :: <raw-c-char>) array :: <object>, base-index ::
<raw-small-integer>,  byte-offset :: <raw-small-integer>) => <raw-c-char>

Description

This is the corresponding setter for *primitive-byte-element*.

primitive-fill!

[Primitive]

Signature

(array :: <object>, size :: <raw-small-integer>, value :: <object>) =>
<object>

primitive-replace!

[Primitive]

Signature

(new-array :: <object>, array :: <object>, size :: <raw-small-integer>)
=> <object>

primitive-replace-bytes!

[Primitive]

Signature

(dst :: <raw-c-void\*>, src :: <raw-c-void\*>, size :: <raw-c-int>) =>
<raw-c-void>

The following primitives, named *primitive-* *type* *-at* and
*primitive-* *type* *-at-setter* load or store, respectively, a value of
the designated *type* at the specified address.

primitive-untyped-at

[Primitive]

Signature

(address :: <raw-pointer>) => <raw-untyped>

primitive-untyped-at-setter

[Primitive]

Signature

(new-value :: <raw-untyped>, address :: <raw-pointer>) => <raw-untyped>

primitive-pointer-at

[Primitive]

Signature

(address :: <raw-pointer>) => <raw-pointer>

primitive-pointer-at-setter

[Primitive]

Signature

(new-value :: <raw-pointer>, address :: <raw-pointer>) => <raw-pointer>

primitive-byte-character-at

[Primitive]

Signature

(address :: <raw-pointer>) => <raw-byte-character>

primitive-byte-character-at-setter

[Primitive]

Signature

(new-value :: <raw-byte-character>, address :: <raw-pointer>) =>
<raw-byte-character>

primitive-small-integer-at

[Primitive]

Signature

(address :: <raw-pointer>) => <raw-small-integer>

primitive-small-integer-at-setter

[Primitive]

Signature

(new-value :: <raw-small-integer>, address :: <raw-pointer>) =>
<raw-small-integer>

primitive-big-integer-at

[Primitive]

Signature

(address :: <raw-pointer>) => <raw-big-integer>

primitive-big-integer-at-setter

[Primitive]

Signature

(new-value :: <raw-big-integer>, address :: <raw-pointer>) =>
<raw-big-integer>

primitive-machine-integer-at

[Primitive]

Signature

(address :: <raw-pointer>) => <raw-machine-integer>

primitive-machine-integer-at-setter

[Primitive]

Signature

(new-value :: <raw-machine-integer>, address :: <raw-pointer>) =>
<raw-machine-integer>

primitive-unsigned-machine-integer-at

[Primitive]

Signature

(address :: <raw-pointer>) => <raw-unsigned-machine-integer>

primitive-unsigned-machine-integer-at-setter

[Primitive]

Signature

(new-value :: <raw-unsigned-machine-integer>, address :: <raw-pointer>)
 => <raw-unsigned-machine-integer>

primitive-single-float-at

[Primitive]

Signature

(address :: <raw-pointer>) => <raw-single-float>

primitive-single-float-at-setter

[Primitive]

Signature

(new-value :: <raw-single-float>, address :: <raw-pointer>) =>
<raw-single-float>

primitive-double-float-at

[Primitive]

Signature

(address :: <raw-pointer>) => <raw-double-float>

primitive-double-float-at-setter

[Primitive]

Signature

(new-value :: <raw-double-float>, address :: <raw-pointer>) =>
<raw-double-float>

primitive-extended-float-at

[Primitive]

Signature

(address :: <raw-pointer>) => <raw-extended-float>

primitive-extended-float-at-setter

[Primitive]

Signature

(new-value :: <raw-extended-float>, address :: <raw-pointer>) =>
<raw-extended-float>

primitive-signed-8-bit-integer-at

[Primitive]

Signature

(address :: <raw-pointer>) => <raw-signed-8-bit-integer>

primitive-signed-8-bit-integer-at-setter

[Primitive]

Signature

(new-value :: <raw-signed-8-bit-integer>, address :: <raw-pointer>)
 => <raw-signed-8-bit-integer>

primitive-unsigned-8-bit-integer-at

[Primitive]

Signature

(address :: <raw-pointer>) => <raw-unsigned-8-bit-integer>

primitive-unsigned-8-bit-integer-at-setter

[Primitive]

Signature

(new-value :: <raw-unsigned-8-bit-integer>, address :: <raw-pointer>)
 => <raw-unsigned-8-bit-integer>

primitive-signed-16-bit-integer-at

[Primitive]

Signature

(address :: <raw-pointer>) => <raw-signed-16-bit-integer>

primitive-signed-16-bit-integer-at-setter

[Primitive]

Signature

(new-value :: <raw-signed-16-bit-integer>, address :: <raw-pointer>)
 => <raw-signed-16-bit-integer>

primitive-unsigned-16-bit-integer-at

[Primitive]

Signature

(address :: <raw-pointer>) => <raw-unsigned-16-bit-integer>

primitive-unsigned-16-bit-integer-at-setter

[Primitive]

Signature

(new-value :: <raw-unsigned-16-bit-integer>, address :: <raw-pointer>)
 => <raw-unsigned-16-bit-integer>

primitive-signed-32-bit-integer-at

[Primitive]

Signature

(address :: <raw-pointer>) => <raw-signed-32-bit-integer>

primitive-signed-32-bit-integer-at-setter

[Primitive]

Signature

(new-value :: <raw-signed-32-bit-integer>, address :: <raw-pointer>)
 => <raw-signed-32-bit-integer>

primitive-unsigned-32-bit-integer-at

[Primitive]

Signature

(address :: <raw-pointer>) => <raw-unsigned-32-bit-integer>

primitive-unsigned-32-bit-integer-at-setter

[Primitive]

Signature

(new-value :: <raw-unsigned-32-bit-integer>, address :: <raw-pointer>)
 => <raw-unsigned-32-bit-integer>

primitive-signed-64-bit-integer-at

[Primitive]

Signature

(address :: <raw-pointer>) => <raw-signed-64-bit-integer>

primitive-signed-64-bit-integer-at-setter

[Primitive]

Signature

(new-value :: <raw-signed-64-bit-integer>, address :: <raw-pointer>)
 => <raw-signed-64-bit-integer>

primitive-unsigned-64-bit-integer-at

[Primitive]

Signature

(address :: <raw-pointer>) => <raw-unsigned-64-bit-integer>

primitive-unsigned-64-bit-integer-at-setter

[Primitive]

Signature

(new-value :: <raw-unsigned-64-bit-integer>, address :: <raw-pointer>)
 => <raw-unsigned-64-bit-integer>

primitive-ieee-single-float-at

[Primitive]

Signature

(address :: <raw-pointer>) => <raw-ieee-single-float>

primitive-ieee-single-float-at-setter

[Primitive]

Signature

(new-value :: <raw-ieee-single-float>, address :: <raw-pointer>) =>
<raw-ieee-single-float>

primitive-ieee-double-float-at

[Primitive]

Signature

(address :: <raw-pointer>) => <raw-ieee-double-float>

primitive-ieee-double-float-at-setter

[Primitive]

Signature

(new-value :: <raw-ieee-double-float>, address :: <raw-pointer>)
 => <raw-ieee-double-float>

primitive-ieee-extended-float-at

[Primitive]

Signature

(address :: <raw-pointer>) => <raw-ieee-extended-float>

primitive-ieee-extended-float-at-setter

[Primitive]

Signature

(new-value :: <raw-ieee-extended-float>, address :: <raw-pointer>)
=> <raw-ieee-extended-float>

