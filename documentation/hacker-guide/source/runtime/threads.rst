Compiler Support for Threads
****************************

Dylan Portability Interface
===========================

The Threads Library is designed for implementation using
different threads APIs from common operating systems, including Unix
and Windows. Harlequin’s implementation of the library is designed
to be directly portable onto these operating systems. This portability
is achieved by using primitive operations defined within our runtime
system. Each primitive operation must be implemented specially for each
operating system.

The set of portable primitive operations is collectively called the
*portability layer*. The Dylan compiler has special knowledge of the
portability layer via primitive function definitions and some
specialized emit methods for flow-graph node types which are specific to
threads.

Portability and Runtime Layers
------------------------------

The design assumes that each of the concrete classes of the
Threads Library (``<thread>``, ``<simple-lock>``, ``<recursive-lock>``,
``<semaphore>`` and ``<notification>``) corresponds with an equivalent
lower-level feature provided directly by either the operating system or
the runtime system. The Dylan objects which are instances of these
classes are implemented as *containers* for handles corresponding to
low-level (non-Dylan) objects. The Dylan objects contain normal Dylan
slots too, and these are directly manipulated by the Dylan library.
However, the slots containing the low-level handles may only be
manipulated via primitive function calls. For each of the classes,
primitive functions are defined to both create and destroy the low-level
handles, as well as to perform the basic functions of the class, such as
*wait-for* and *release*. The platform-specific implementation of these
primitive functions is free to choose any representation for these
handles, provided that it is the same shape as a Dylan slot (which is
equivalent to C’s *void \**).

As with all Dylan objects, the container objects defined by the threads
library are subject to automatic memory management, and possible
relocation by the garbage collector. The contents of the container slots
will be copied during such a relocation — but the values they contain
will not be subject to garbage collection or relocation themselves.

The portability layer provides no direct support for the *dynamic-bind*
operation. The library implements a *dynamic variable* as a thread-local
variable via the high-level Dylan constructs *define thread variable* and
*block ... cleanup* to manage the creation and deletion of new bindings.

The portability layer includes support for conditional update of atomic
variables, as well as assignment. The implementation mechanism for these
is not defined, but it is hoped that many platforms will provide direct
hardware support for this operation. Where hardware support is not
available, the low-level implementation may choose to use a lock to
protect conditional updates and assignments, as a fall back option. It
is assumed that atomic variables may always be read as normal variables.

`Implementations of Dylan Thread Interfaces`_
shows the expected mapping between the concrete Dylan classes and
low-level operating system features, for three of the most popular
general-purpose operating systems.

Implementations of Dylan Thread Interfaces
------------------------------------------

+-------------------------+-----------------------+-----------------------+
|  Dylan Interface        | Unix Implementation   | Win32 Implementation  |
+=========================+=======================+=======================+
| ``<thread>``            | thread                | thread                |
+-------------------------+-----------------------+-----------------------+
| ``<simple-lock>``       | mutex                 | critical region       |
+-------------------------+-----------------------+-----------------------+
| ``<recursive-lock>``    | mutex                 | critical region       |
+-------------------------+-----------------------+-----------------------+
| ``<semaphore>``         | semaphore             | semaphore             |
+-------------------------+-----------------------+-----------------------+
| ``<notification>``      | condition variable    | event                 |
+-------------------------+-----------------------+-----------------------+
| ``dynamic variable``    | thread-local variable | thread-local variable |
+-------------------------+-----------------------+-----------------------+
| ``conditional-update!`` | mutex                 | exchange instruction  |
|                         |                       | (using a guard value  |
|                         |                       | as a lock);           |
+-------------------------+-----------------------+-----------------------+

Dylan Types for Threads Portability
-----------------------------------

Three Dylan types merit discussion for their use with portability
primitives: ``<thread>``, ``<portable-container>``, and ``<optional-name>``.
Objects that are instances of the ``<thread>`` and
``<portable-container>`` classes have slots which contain lower-level
objects that are specific to the Dylan runtime or operating system. The
``<optional-name>`` type allows an object, such as a lock, to have a name
represented as a string or, if no name is supplied, as the Boolean false
value ``#f``.

<thread>

[Class]

A Dylan object of class ``<thread>`` contains two OS handles. One of these
represents the underlying OS thread, and the other may be used by
implementations to contain the current status of the thread, as an aid
to the implementation of the join state.

<portable-container>

[Class]

The ``<portable-container>`` class is used by the implementation as a
superclass for all the concrete synchronization classes (``<simple-lock>``,
``<recursive-lock>``, ``<semaphore>``, and ``<notification>``). Each
``<portable-container>`` object contains an OS handle, which is available
to the runtime for storing any OS-specific data. Subclasses may provide
additional slots.

<optional-name>

[Type]
------

This is a union type which is used to represent names of synchronization
objects. Values of the type are either strings (of class ``<byte-string>``)
or false (``#f``).

Various classes of Dylan objects are passed through the portability
interface, and hence require description in terms of lower level
languages. `Correspondence Between Dylan Types and C
Types`_ maps the layout of these Dylan objects onto
their C equivalents, which are used by runtime-specific implementations
of the portability layer.

In general, all Dylan types can be thought of as equivalent to the C
type ``D``, which is in turn equivalent to the C type ``void*``. Of
course, runtime-specific implementations of the portability layer must
have access to relevant fields of the Dylan objects on which they
operate. The type definitions in `Correspondence Between Dylan Types
and C Types`_ give implementations access to fields
needed for specific types. These definitions are not necessarily
complete descriptions of the Dylan objects, however. The objects may
contain additional fields that are not of interest to the portability
layer, and subclasses may add additional fields of their own.

Correspondence Between Dylan Types and C Types
----------------------------------------------

+----------------------------+---------------+--------------------------------------+
| Dylan Type                 | C Type        | C Type Definition                    |
+============================+===============+======================================+
| ``<object>``               | *D*           | *typedef void\* D;*                  |
+----------------------------+---------------+--------------------------------------+
| ``<integer>``              | *DINT*        | *platform specific (size of void\*)* |
+----------------------------+---------------+--------------------------------------+
| ``<function>``             | *DFN*         | *typedef D(\*DFN)(D, int, …);*       |
+----------------------------+---------------+--------------------------------------+
| ``<simple-object-vector>`` | *SOV\**       | *typedef struct \_sov {              |
|                            |               | * *D class;                          |
|                            |               | * *DINT size;*                       |
|                            |               | *D data[ ];*                         |
|                            |               | *} SOV;*                             |
+----------------------------+---------------+--------------------------------------+
| ``<byte-string>``          | *B\_STRING\** | *typedef struct \_bst {              |
|                            |               | * *D class;                          |
|                            |               | * *DINT size;*                       |
|                            |               | *char data[ ];*                      |
|                            |               | *} B\_STRING;*                       |
+----------------------------+---------------+--------------------------------------+
| ``<optional-name>``        | *D\_NAME*     | *typedef void\* D\_NAME;*            |
+----------------------------+---------------+--------------------------------------+
| ``<portable-container>``   | *CONTAINER\** | *typedef struct \_ctr {              |
|                            |               | * *D class;                          |
|                            |               | * *void\* handle;*                   |
|                            |               | *} CONTAINER;*                       |
+----------------------------+---------------+--------------------------------------+
| ``<thread>``               | *D\_THREAD\** | *typedef struct \_dth {              |
|                            |               | * *D class;                          |
|                            |               | * *void\* handle1;*                  |
|                            |               | *void\* handle2;*                    |
|                            |               | *} D\_THREAD;*                       |
+----------------------------+---------------+--------------------------------------+

Compiler Support for the Portability Interface
==============================================

The Compiler Flow Graph
-----------------------

The front end of the compiler parses Dylan source code and produces an
intermediate representation, the Implicit Continuation Representation
(ICR). The ICR is a directed acyclic graph (DAG) of Dylan objects. A
*leaf* in the ICR represents a basic computational object, such as a
variable (of class ``<variable-leaf>``) or a function (of class
``<function-leaf>``). A *node* in the ICR represents an operation such as
assignment (class ``<assignment>``), conditional execution (class ``<if>``),
or a reference to a leaf (class ``<reference>``).

In mapping Dylan code to the ICR, the compiler uses a set of
*converters*, which perform syntactic pattern matching against
fragments of Dylan code and generate the ICR corresponding to the
matched code. For example, when the compiler encounters a top-level
variable definition (introduced by the Dylan *define variable*
construct), the converter for *define variable* creates a new instance
of ``<global-variable-leaf>`` in the ICR to represent this variable and to
record data such as its name, initial value, and typing information.

The back end of the compiler traverses the flow graph and emits code in
the target language for compiler output. Methods in the back end
specialize on node and leaf classes to enable them to produce the
appropriate output.

Compiler Support for Atomic and Fluid Variables
-----------------------------------------------

The portability layer provides support for atomic variable access and
for Dylan fluid variables (implemented as thread-local variables).
Atomic variables and thread variables are directly represented in the
flow graph, where they are subject to dataflow analysis. The variables
themselves appear as leaves in the graph.

Because both atomic and fluid variables need special treatment when they
are accessed, the back end must emit output that is different from that
for accessing other kinds of variables. The compiler defines two
specialized classes of leaf for the ICR, ``<atomic-global-variable-leaf>``
(corresponding to atomic variables) and ``<fluid-global-variable-leaf>``
(corresponding to fluid variables). These are subclasses of
``<global-variable-leaf>`` and therefore inherit general characteristics
of leaves that represent variables.

ICR leaves representing both atomic and fluid variables are created by
the converter for ``define variable``. When the compiler encounters a
definition of an atomic variable (introduced by the ``define
atomic-variable`` construct), the converter for ``define variable`` creates
an instance of ``<atomic-global-variable-leaf>`` in the ICR. When the
compiler encounters a definition of a fluid variable (introduced by the
``define fluid-variable`` construct), the converter creates an instance of
``<fluid-global-variable-leaf>``.

The operations of reading, writing, and conditionally updating atomic
variables and of reading and writing fluid variables are not represented
by primitive functions. Instead, they are represented directly in the
flow graph. They are implemented by specializing methods on the leaf
classes that represent atomic and fluid variables.

Compiler Support for Primitives
-------------------------------

When the compiler constructs the flow graph, it represents a function
call as a node in the ICR. Just as the compiler distinguishes atomic and
fluid variables by means of specialized leaf classes, so it
distinguishes calls to primitive functions of the portability interface
by means of a specialized node class.

A function call is an operation on several components: the function
object, the arguments, and the destination for returned values. When the
compiler encounters a regular Dylan call, which typically appears as a
call to a generic function, it represents the call in the ICR as a node
of class ``<combination>``.

However, the compiler contains a table of the primitive functions in the
portability interface. Before creating an ICR node to represent a
function call, the compiler looks up the function being called in the
table of primitives. If the function appears in the table, the compiler
creates an ICR node of class ``<primitive-combination>``.

When the back end traverses the flow graph, methods specialized on the
node class ``<primitive-combination>`` emit calls to primitive functions.

Support for Dylan Language Features
===================================

Interfacing to Foreign Code
---------------------------

It is intended that threads created by the Dylan library may
inter-operate with code written in other languages with no special
constraints. Dylan is interfaced with other languages via a Foreign
Language Interface (*FLI*), which acts as a barrier between Dylan
conventions and the *neutral* conventions of the platform. The FLI is
responsible for:

#. mapping between Dylan and foreign data types,
#. converting between Dylan and foreign calling conventions
#. maintaining the Dylan dynamic environment
#. maintaining any support necessary for garbage collection (such as
   ensuring that all Dylan values can be traced).

The first and second of these require no significant extensions to
support multiple threads, since these are inherently computations which
have no effect on any thread other than the one performing the
computation.

There is a requirement that the dynamic environment for each thread is
stored in a thread-local variable. Since the environment is stored in
this way, its value is preserved across calls into foreign code, and it
will still be valid if the foreign code calls back into Dylan. The
techniques described in [MG95] for maintaining the dynamic environment
across foreign calls are therefore directly appropriate to a
multi-threaded implementation too.

If an object is passed to foreign code with dynamic extent, then it is
sufficient to ensure that the object is referenced from the current
stack, which the garbage collector will scan conservatively. In a
multi-threaded implementation, the garbage collector will scan all the
stacks conservatively, so there is no requirement to maintain a
thread-global data structure.

If an object is passed with indefinite extent, then it must be recorded
in a table. The table may be maintained by the runtime system, by means
of suitable primitive functions to add and remove references. There are
potentially synchronization problems associated with multiple threads
manipulating a global data structure — but the runtime system
implementation is free to choose whether to have separate tables for
each thread, or whether to have a global table with an associated lock
to guard accesses. Either technique is possible — but Harlequin have not
yet implemented this feature.

One further consideration is the interaction of the Dylan threads
library itself with foreign components:

If foreign code is not designed for multiple threads (for instance,
because it uses global data structures, and doesn’t synchronize
updates), then the code may fail if it is invoked from multiple Dylan
threads. However, this problem is not related to the Dylan
implementation, since it would fail if called from multiple threads
created by any means. The solution is to modify the foreign component to
make it thread safe.

If foreign code is designed for use with multiple threads, then it is
valid for it to use the synchronization facilities of the Dylan library
(by calling back into Dylan, to invoke the Threads Library
synchronization functions). Alternatively, it may use its own methods
for synchronization, provided that these are not incompatible with the
methods provided by the operating system. This is valid whenever it has
been possible to implement the runtime system support for threads
directly in terms of operating system features, and it is anticipated
that this will always be true if the operating system supports threads.
Typically, foreign code is expected to make direct use of operating
system threads facilities.

However, a problem may arise if a thread is created in foreign code, and
the new thread then calls back into Dylan. In this case, the Dylan
thread library itself will not be able to find an existing ``<thread>``
object corresponding to the current thread, and the fluid variables for
the current thread will not have been correctly initialized. Worse
still, the garbage collector may not have enough information to locate
the roots of the thread. Harlequin have not yet allowed for this in
their implementation, but they have an anticipated solution.

It is possible to detect that a thread has never been executing on the
Dylan side of the FLI before because it will have an uninitialized
(zero) value for its thread-local dynamic environment variable. This can
be checked at a call-in in the stub function which implements the FLI.
Once such a thread has been detected, appropriate initialization steps
can be taken. A function in the runtime system can be called to register
the stack of the thread for root tracing; the dynamic environment can be
set to point to a suitable value on the stack; finally a new Dylan
``<thread>`` object can be allocated and initialized with
``primitive-initialize-current-thread`` (as for the first thread).

Finalization
------------

As has been discussed, the Dylan synchronization objects are implemented
as wrappers around lower-level operating system structures. The Dylan
objects are subject to garbage collection, and their memory will be
automatically freed by the garbage collector at an undefined point in
the program. But the low-level structures are not Dylan objects and must
be explicitly freed when the Dylan container is collected (primitive
functions are provided for this purpose). However, the core language of
Dylan provides no *finalization* mechanism to invoke cleanup code when
objects are reclaimed. Harlequin’s implementation of the Threads
Library strictly requires this, but it is not yet implemented. It is
intended to provide finalization support for Dylan with a new garbage
collector which is currently under development.
