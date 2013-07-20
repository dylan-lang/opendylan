Calling Convention
==================

Some terminology
----------------

Arguments passed to a function at the implementation level fall into 2
different groups. *Language parameters* correspond to the explicit
arguments in the source code. *Implementation parameters* correspond to
the house-keeping information used by the implementation.

The overall calling convention consists of several specific conventions
with different properties, described below. Each convention is
implemented by a separate *entry point*. There are partial orderings
between the entry points for these conventions, depending on how
specific each one is. The code which implements a control flow from one
entry point to the next may be obliged to rearrange parameters (e.g. on
the stack). This process is called *stack fixing*.

The register model
------------------

Three registers are used within the calling convention to support the
passing of *implementation parameters*: Note that for the C backend,
global (or thread-local) variables might be used instead of real
registers to pass these parameters.

+-----------+------------------------------------------------+
| Register  | Purpose                                        |
+===========+================================================+
| arg-count | number of args passed                          |
+-----------+------------------------------------------------+
| function  | the :drm:`<function>` object being called      |
+-----------+------------------------------------------------+
| mlist     | the next-method list (``#f`` for direct-entry) |
+-----------+------------------------------------------------+

The argument passing conventions
--------------------------------

For each of the conventions, arguments are pushed onto the stack in
reverse order (i.e. the rightmost argument is pushed first). The
leftmost (or leftmost few) arguments are passed in registers. This has a
possible disadvantage from the opposite ordering in terms of the need
for temporary variables to hold interim results for order-of-evaluation
reasons. In practice, the disadvantages will be small because:

-  Many arguments to functions are expected to be simple expressions
   (like constants or variable references) - so order of evaluation does
   not normally matter.
-  On a RISC implementation, we won’t want to push each argument anyway
   - instead it will be more efficient to allocate enough stack space
   for the call, and store each argument when it’s available. This works
   well with a conservative GC - but it might be poor with a total GC.

This calling convention has the following advantages:

-  required arguments can always be found at a known offset from a stack
   or frame pointer for any of the calling conventions
-  optional arguments appear in the same order in memory as they would
   if vectored up as ``#rest`` parameters
-  Stack allocating the optional arguments as vectors is almost trivial.

For the native code implementation, the callee is responsible for
popping any arguments from the stack. This is always possible (even with
dynamically sized optional args), because the argcount is available to
say how many arguments were passed. This is not possible for the C
backend - and this is the only substantial difference from the C arg
passing convention.

Calling Convention Goals
------------------------

*Internal entry points* should be as efficient as possible. I.e. there
should not be any constraints on them because Dylan is a dynamic
language.

#. There must be a consistent convention for all functions at the
   *external entry point*, so that functions can be called without the
   caller having any knowledge of what they are.
#. The code which is executed at external entry points should be shared
   by all functions with similar properties / lambda-lists.
#. The design should make the path from the external entry point to the
   internal entry point as simple as is reasonably possible.

The External Entry Point Convention
-----------------------------------

All Dylan function objects support the *external* convention. Each
function object has an *XEP* slot containing the code to support this
convention. External entry points are used for all unoptimized, normal
calls to functions. This includes direct calls to methods and generic
functions. Of course, whenever the compiler can use a more efficient
entry point instead, then it will.

The registers are used as follows:

+-----------+----------------------+
| Register  | Purpose              |
+===========+======================+
| argcount  | number of arguments  |
+-----------+----------------------+
| function  | the function object  |
+-----------+----------------------+
| mlist     | not used             |
+-----------+----------------------+

If the function has a complex lambda list (with ``#rest`` or ``#key``),
then the external entry code will be one of a standard set of stack fixing
functions. This stack fixer will make use of information in the function
register to determine which keys to look for, whether the arg-count is
legal, whether the arguments have appropriate types etc. The stack fixer
will then tail jump to the internal entry point (again, found from the
function object). This mechanism requires 2 transfers of control (caller
-> stack-fixer -> callee).

For example, consider the following Dylan code:

.. code-block:: dylan

    define method func1 (a, b, #rest optionals, #key key1, key2)
    end method;
    func1(1, 2, key2: 99);

For the call to ``func1``, above, the parameters are described in the
following table:

XEP Parameters for the Call to *func1*

+---------------------+-----------------------------------+
| XEP Parameters      | Values                            |
+=====================+===================================+
| language parameters | ``1``, ``2``, ``#"key2"``, ``99`` |
+---------------------+-----------------------------------+
| *argcount*          | 4                                 |
+---------------------+-----------------------------------+
| *function*          | generic function `func1``         |
+---------------------+-----------------------------------+

Internal Entry Point Convention
-------------------------------

The IEP convention uses a fixed number of language parameters,
corresponding to each of the parameters of the function (5 in the case
of *func1*, above, corresponding to a, b, optionals, key1, key2). In
addition, there are two implementation parameters:

-  *mlist*, a list of the next applicable methods to call if the
   function is a method called from a generic function (this parameter
   is used to support calls to *next-method*). If the function is not
   being called from a generic function, the value is *#f* (false).
-  *function*, the Dylan function object being called (as for the XEP).

The implementation parameters are not obligatory for all IEP code. It is
only necessary to pass *mlist* if the function contains a call to
*next-method*. It is only necessary to pass *function* if the function
is a closure (because the value is used by the IEP code to locate the
environment of the closure). If the IEP is called from the XEP code,
both the implementation parameters will always be set, even though they
may not be necessary. For the same call to ``func1``, above, the
parameters are described in ` <runtime.htm#12946>`_.

IEP Parameters for the Call to *func1*

+---------------------+---------------------------------------------+
| IEP Parameters      | Values                                      |
+=====================+=============================================+
| language parameters | ``1``, ``2``, ``optionals``, ``#f``, ``99`` |
+---------------------+---------------------------------------------+
| *mlist*             | ``#f``                                      |
+---------------------+---------------------------------------------+
| *function*          | generic function ``func1``                  |
+---------------------+---------------------------------------------+

Note that the language parameters now correspond to the formal
parameters of the function, whereas, for the XEP, they corresponded to
the supplied arguments.

The value of *optionals* in the set of language parameters is the Dylan
vector ``#[#"key2", 99]`` which corresponds to all the optional arguments.
The language parameter corresponding to ``key1`` is ``#f``, because the
keyword ``#"key1"`` was not supplied. However, the language parameter
corresponding to ``key2`` is ``99``, because ``#"key2"`` was supplied with
that value.

The Method Entry Point Convention
---------------------------------

All :drm:`<method>` objects support the *method entry point* convention. Each
method object has an *MEP* slot containing the code to support this
convention. When a method is called by a generic function (or via next
method), the caller uses a dedicated entry point (available from the
function object). If the method accepts ``#key`` or ``#rest`` parameters, then
the method is called with a (possibly stack-allocated) vector
representing the optional args. This vector appears as a single extra
required argument.

If the method accepts ``#key`` parameters, then the method entry point will
process the supplied keywords - stack fixing them so that they appear as
required arguments. It will then tail-call the internal entry point.

If the method does not accept #key, then the method entry point is the
same as the internal entry point.
