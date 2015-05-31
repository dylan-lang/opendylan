Special Features
================

Introduction to bind-exit and unwind-protect
--------------------------------------------

The following sections describe the implementation for the native code
compiler, only.

Bind-exit and unwind-protect are represented on the stack as frames
which contain information about how to invoke the relevant
continuation. Unwind-protect frames are also chained together, and the
current environment of existing unwind-protects is available in
``%current-unwind-protect-frame``.

There are primitives to build each type of frame, and also to remove
unwind-protect frames (bind-exit frames just have to be popped - so that
is done inline). The primitive which removes unwind-protect frames in
the fall-through case is also responsible for invoking the cleanup code
(which is called as a sub-function in the same function frame as its
parent).

There are also primitives to do non-local exits (*NLX*). These are
passed the address of the bind-exit frame for the destination, and also
the multiple values to be returned. As part of the NLX, any intervening
unwind-protects are invoked and their frames are removed.
Multiple-values are saved around the unwind-protects in the bind-exit
frame of the destination.

unwind-protect
--------------

An *unwind-protect* frame (*UPF*) looks as follows:

+--------+----------------------------------+
| Offset | Value                            |
+========+==================================+
| 8      | address of start of cleanup code |
+--------+----------------------------------+
| 4      | frame pointer                    |
+--------+----------------------------------+
| 0      | previous unwind-protect frame    |
+--------+----------------------------------+

The compiler compiles unwind-protect as follows:

.. code-block:: dylan

    let frame = primitive-build-unwind-protect-frame(tag1);
    do-the-protected-forms-setting-results-as-for-a-return();
    primitive-unwind-protect-cleanup();
    goto(tag-finished);
    tag1:
      do-the-cleanup-forms();
      end-cleanup(); // inlined as a return instruction
    tag-finished:

If the protected body exits normally, then
*primitive-unwind-protect-cleanup* is called (in the runtime system).
This causes the unwind-protect frame to be unlinked from the chain, and
the cleanup code to be invoked, as a subroutine call within the same
function frame as the protected body. The cleanup code finishes by
executing a return instruction. The runtime system ensures that any
multiple values are restored, and returns control to the compiled code,
which then executes the code following the unwind-protect.

If the cleanup code is invoked because of an NLX, then the runtime
function finds the ultimate destination *bind exit frame* (*BEF*) from
the UPF. The runtime function then passes this BEF to another runtime
function (as for *bind-exit)* to test whether there are any further
intervening cleanups, or to transfer control to the ultimate destination
if not.

bind-exit
---------

A *bind-exit* frame (*BEF*) looks as follows:

+--------+------------------------------+
| Offset | Value                        |
+========+==============================+
| 52     | continuation address         |
+--------+------------------------------+
| 48     | frame pointer                |
+--------+------------------------------+
| 44     | current unwind-protect frame |
+--------+------------------------------+
| 4      | space for stack-allocated    |
|        | vector for up to 8 multiple  |
|        | values                       |
+--------+------------------------------+
| 0      | pointer to saved multiple    |
|        | values as a vector           |
+--------+------------------------------+

The compiler compiles bind-exit as follows:

.. code-block:: dylan

    let frame = primitive-build-bind-exit-frame(tag1);
    let closure = make-bind-exit-closure(frame);
    do-the-bind-exit-body-setting-results-as-for-a-return();
    tag1:

During an NLX, multiple-values will be saved in the frame if an
intervening unwind-protect is active. The frame itself contains space
for 8 values. If more values are present, then they will be heap
allocated.

When an NLX occurs, the transfer of control is implemented by a call
into the runtime system, passing the pointer to the BEF as a parameter.
The runtime function first checks whether there is an intervening
cleanup, by testing whether the target dynamic environment in the BEF
matches the current global dynamic environment. If there is no
intervening cleanup, then control is transferred to the destination of
the BEF. Alternatively, if there is an intervening cleanup, then the
ultimate destination field of the current UPF is set to the destination
BEF, and the cleanup code is invoked within a loop which repeatedly
tests for further intervening unwind-protect frames until no more are
found.

Multiple Values
---------------

The current implementation of multiple values supports Common Lisp
semantics. It is about to be replaced by a new version which
supports the new Dylan semantics.

Harlequin’s current implementation uses a register to return a single
Dylan value, as this is the only value that is used by almost all
callers. In addition, each function returns a count of the number of
values being returned. This count can be examined by the caller, if
required, to determine how many values were returned. If a function is
returning more than one value, the additional values are stored in a
global (thread-local) area, where the caller may retrieve then, if
desired. On RISC architectures, the multiple value count is returned in
a register. For the x86 architecture, the *direction flag* register is
set / unset to specify whether single / multiple values are being
returned, respectively. If multiple values are being returned, then the
count of the values is stored in a global (thread-local) location.

Documentation for the new version will be available shortly. Until then,
here’s an overview:

Functions which return a fixed number of return values just return those
values, without returning a count. The first few values will be returned
in registers (an architecture-specific number), and remaining values
will be returned in a thread-local overspill area. If a function always
returns zero values, then no code need be executed to indicate this
fact.

Functions which return a dynamically-sized number of values return their
values as above, but also return a count of the number being returned in
a register. If a function dynamically happens to return zero values,
then the return count will be set to zero, but the value *#f* will be
returned as if it were the first return value.

If the caller of a function can statically determine the number of
return values (i.e. at compile-time), then it need perform no checks.
However, if the caller has no knowledge of the function being called,
then it must check the properties of the callee function object to
determine whether the static or dynamic convention is being used, and
may then need to read either the dynamic return value count, or the
static count in the properties of the function object.

This design has some interesting implications for tail-call
optimization. A function can simply tail another function only if both
the following rules apply:

#. The callee is known to return at least as many values as the caller,
   and they have appropriate types.
#. If the caller returns a dynamically-sized number of values, then the
   callee must too.
