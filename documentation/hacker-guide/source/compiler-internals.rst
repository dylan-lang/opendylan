*****************************
Open Dylan Compiler Internals
*****************************

=============================================================
Overview of the libraries involved in the compilation process
=============================================================

Introduction
------------

This chapter is an overview of the compiler, information was gathered
while hacking on the compiler. It focuses only on DFMC, the Dylan
Flow Machine Compiler, located in source/dfmc of the opendylan
repository.

But first look how to get there: lets consider the command-line
compiler, invoked with ``-build hello-world``. This is parsed by the
executable (in ``environment/console/command-line.dylan``, in method
``execute-main-command``: this has some code like ``if (build?)
run(<build-project-command>, ...``.

This ``<build-project-command>`` is defined in
``environment/commands/build.dylan``, there a method
``do-execute-command`` is specified with a ``<build-project-command>``
as argument. This runs the method ``build-project``, defined in
``environment/dfmc/projects/projects.dylan``, calling
``compile-library``.

This is defined in ``project-manager/projects/compilation.dylan`` and
calls ``parse-and-compile``, which calls ``parse-project-sources``
(defined in ``dfmc/management/definitions-driver``) and
``compile-project-definitions``, which is defined in
``dfmc/browser-support/glue-routines.dylan``, calling
``dfmc-compile-library-from-definitions``, finally defined in
``dfmc/management/world.dylan`` (here under the name
``compile-library-from-definitions``).

To explain these long call chains, we need some more understanding of
the different libraries of the compiler: environment is the public
API, project-manager is a bunch of hacks to care about finding the
project (by using the registry) and calling the compiler, and the
linker (to create a dll/so and executable) afterwards. The libraries
``dfmc/browser-support`` and ``environment/dfmc`` are the glue from
environment to DFMC.

The big picture is pretty simple: management drives the different
libraries, some are the front-end (reader, macro-expander) translating
into definitions; some intermediate language (conversion,
optimization, typist) which work on the flow-graph; others are
back-end, including linker. There is some support needed for the
actual runtime, which is sketched in modeling (parts of which are put
into the dylan runtime library), namespace (which handles namespaces
and defines the dylan library and its modules).

First we need to introduce some terminology and recapitulate some
conventions:

* the unit of compilation is a single dylan library
* the metadata of a library is stored in DOOD, the dylan object-oriented
  database
* loose (development) vs tight (production): loose mode allows runtime
  updates of definitions, like adding generic function into a sealed
  domain, subclassing sealed classes - production mode has stricter
  checks
* batch compilation: when invoked from command line, or building a
  complete library
* interactive compilation: IDE feature to play around, adding a single
  definition to a library

DFMC is well structured, but sadly some libraries use each others,
which they shouldn't (typist, conversion, optimization).

In the remainder of this guide, we will focus on a simple example,
which prints ``Hello`` x times:

.. code-block:: dylan

    define method hello-world (x :: <integer>) => ()
      do(curry(format-out, "Hello %d\n"), range(to: x))
    end

dfmc-management
-----------------

The library dfmc-management drives the compilation process, prints
general information what is happening at the moment (progress,
warnings) and takes care of some global settings like opening and
closing source records, etc.

The main external entry point is ``compile-library-from-definitions`` in
world.dylan. This requires that the source has already been parsed
(really? but it calls compute-library-definitions itself!).

It then calls in sequence ``compute-library-definitions``,
``ensure-library-compiled``, ``ensure-library-glue-linked``,
``ensure-library-stripped`` and ``ensure-database-saved``, apart from
console output (warnings, stats).

The very first method, ``compute-library-definitions``, calls
``ensure-library-definitions-installed``, which calls
``update-compilation-record-definitions``, which mainly calls
``compute-source-record-top-level-forms``. This opens the compilation
record as a stream and calls ``read-top-level-fragment`` to get a
fragment and then ``top-level-convert-forms``.

The reader library defines the ``read-top-level-fragment``, the
definitions library the ``top-level-convert-forms``. Thus, a fragment
is read and converted into definitions.

The method ``ensure-library-compiled`` computes, finishes and checks
the data models. Afterwards the code models are generated (the control
flow graph), then type inference is done and the optimizer is run.
Finally the heaps are generated. These are methods defined in
``compilation-driver.dylan``, calling out to the modeling, conversion,
flow-graph, typist and optimizer libraries.

Finally the glue is emitted (in ``back-end-driver.dylan``) and the
database is saved, which contains metadata of each library (like type
information, code models, etc.).

Some global warnings for libraries are defined and checked for in the
management library.

The unused file ``interface.dylan`` compares module and binding
definitions, in order to judge whether the public API of a
library/module has changed between two versions. Usage of this would
allow lazy recompilation: only recompile if the API has changed of a
linked library.

dfmc-reader
-----------

This library reads the Dylan source, tokenizes it, annotates source
locations, and builds a parse tree. The parser is in
``parser.dylgram``, which uses ``app/parser-compiler`` to generate
``infix-parser.dylan``. The API used is ``read-top-level-fragment``
and ``re-read-fragments``.

Error handling is on the token level, thus a mismatched ``end`` is
noticed. Other sorts of errors are invalid token, integer too large,
character too large, ratios not being supported, end of input (while
more tokens were required).

Every ``<fragment>``, the base class of the abstract syntax tree, has
a compilation-record and a source-position.

So, ``read-top-level-fragment`` returns the following parse tree::

    <body-definition-fragment>:
      fragment-macro: <simple-variable-name-fragment>
                                           fragment-name: #"method-definer"
      fragment-modifiers: #()
      fragment-body-fragment:
        <simple-variable-name-fragment>:
          fragment-name: #"hello-world"
        <parens-fragment>:
          fragment-left-delimiter: <lparen-fragment>
          fragment-nested-fragments:
            <simple-variable-name-fragment>:
              fragment-name: #"x"
            <colon-colon-fragment>
            <simple-variable-name-fragment>:
              fragment-name: #"<integer>"
          fragment-right-delimiter: <rparen-fragment>
        <simple-variable-name-fragment>:
          fragment-name: #"do"
        <parens-fragment>:
          fragment-left-delimiter: <lparen-fragment>
          fragment-nested-fragments:
            <simple-variable-name-fragment>:
              fragment-name: #"curry"
            <parens-fragment>:
              fragment-left-delimiter: <lparen-fragment>
              fragment-nested-fragments:
                <simple-variable-name-fragment>:
                  fragment-name: #"format-out"
                <comma-fragment>
                <string-fragment>:
                  fragment-value: "Hello %d\n"
              fragment-right-delimiter: <rparen-fragment>
            <comma-fragment>
            <simple-variable-name-fragment>:
              fragment-name: #"range"
            <parens-fragment>:
              fragment-left-delimiter: <lparen-fragment>
              fragment-nested-fragments:
                <fragment-syntax-symbol-fragment>:
                  fragment-value: #"to"
                <simple-variable-name-fragment>:
                  fragment-name: #"x"
              fragment-right-delimiter: <rparen-fragment>
          fragment-right-delimiter: <rparen-fragment>
        <semicolon-fragment>
    
NB: the type hierarchy for <body-definition-fragment> is: <definition-fragment>, <macro-call-fragment>, <compund-fragment>, <fragment>, <object>


dfmc-definitions
----------------

Once the abstract syntax tree is generated (by the reader), it's time
to convert this into definitions, which are the names in dylan. There
are several top-level definitions in dylan, namely: binding, class,
constant, (copy-down), domain, function, generic, macro, method,
module, namespace (library) and variable. Every definition has it's
own class, inheriting from ``<top-level-form>`` (defined in
common/top-level-forms.dylan). A top level form at least contains
information about its compilation record, source location, parent
form, sequence number and dependencies and referenced variables.
Additional information available are adjectives, the word defined, its
library, original library, top level methods. As a side note,
dependency tracking is also defined in
``common/top-level-forms.dylan``.

The main entry point for the definition library is
``top-level-convert`` on a fragment, defined in
``top-level-convert.dylan``.

The building of definition objects relies heavily on the
macro-expander, especially on procedural macros described in
D-Expressions: Lisp Power, Dylan Style
(http://people.csail.mit.edu/jrb/Projects/dexprs.pdf). Open Dylan
extends the definitions with compiler, optimizer, primitive and
shared-symbols, mainly used internally in the compiler.

Looking into ``define-method.dylan``, we can see a class
``<method-definition>``. This is built by the parser, more
specifically there is a ``define &definition method-definer``, which
has two rules to match fragments, whereas the second rule is the error
case. The first matches any ``define method`` syntax and calls
``do-define-method`` with the arguments. The method
``do-define-method`` defers the work to helper methods
``parse-method-adjectives`` and ``parse-method-signature``, and
instantiates a ``<method-definition>`` object.

For our small example, ``do-define-method`` creates a single object:

The result of our small example is::

    <method-definition>
      private-form-body: <body-fragment>
        fragment-constituents: <prefix-call-fragment>
          fragment-arguments:
            <prefix-call-fragment>
              fragment-arguments:
                <simple-variable-name-fragment>
                  fragment-name: #"format-out"
                <string-fragment>
                  fragment-value: "Hello %d\n"
              fragment-function: <simple-variable-name-fragment>
                fragment-name: #"curry"
            <prefix-call-fragment>
              fragment-arguments:
                <keyword-syntax-symbol-fragment>
                  fragment-value: #"to"
                <simple-variable-name-fragment>
                  fragment-name: #"x"
              fragment-function: <simple-variable-name-fragment>
                fragment-name: #"range"
          fragment-function: <simple-variable-name-fragment>
            fragment-name: #"do"
      private-form-signature: <method-requires-signature-spec>
        private-spec-argument-next-variable-specs: <next-variable-spec>
          private-spec-variable-name: <simple-variable-name-fragment>
            fragment-name: #"next-method"
        private-spec-argument-required-variable-specs: <typed-required-variable-spec>
          private-spec-type-expression: <simple-variable-name-fragment>
            fragment-name: #"<integer>"
          private-spec-variable-name: <simple-variable-name-fragment>
            fragment-name: #"x"
      private-form-signature-and-body-fragment: <sequence-fragment>
        <parens-fragment>, <simple-variable-name-fragment>, <parens-fragment>, <semicolon-fragment>
      private-form-variable-name-or-names: <simple-variable-name-fragment>
        fragment-name: #"hello-world"

It is noteworthy that still no intra-library information is present,
this is top-level Dylan code without any context. All macros are
expanded.

dfmc-macro-expander
-------------------

The deep magic happens here.

dfmc-convert
------------

   Converts definition objects to model objects. In order to fulfill
   this task, it looks up bindings to objects from other
   libraries. Also converts the bodies of definitions to a flow
   graph. Does some initial evaluation, for example ``limited(<vector>,
   of: <string>)`` gets converted to a ``<&limited-vector-type>``
   instance. Thus, it contains a poor-mans eval.

   Also, creates init-expressions, which may be needed for the
   runtime, since everything can be dynamic, each top-level-form may
   need initializing which are called when the library is loaded.

   Also sets up a lexical environment for the definitions, and checks
   bindings.

   Here, type variables are now recorded into the lexical environment,
   the type variables are passed around while the signature is
   checked.

   After Dylan code is converted, it is in a representation which can
   be passed to a backend to generate code. Modeling objects have
   corresponding compile and run time objects, and are prefixed with
   an ampersand (``<&object>``).

dfmc-modeling
-------------

   Contains modeling of runtime and compile time objects. Since some
   calls are tried to be done at compile time rather than at runtime,
   it provides these compile time methods with a mechanism to override
   the runtime methods (``define &override-function``). An example for
   this is ``^instance?``, compile time methods are prefixed with a ``^``,
   while compile and runtime class definitions are prefixed with ``&``,
   like ``define &class <type>``.

   Also, DOOD (a persistent object store) models and proxies for
   compile time definitions are available in this library, in order to
   load definitions of dependent libraries.

dfmc-flow-graph
---------------

   The flow graph consists of instances of the ``<computation>`` class,
   like ``<if>``, ``<loop-call>``, ``<assignment>``, ``<merge>``. The flow
   graph is in a (pseudo) single state assignment form. Every time any
   algorithm alters the flow graph, it disconnects the deprecated
   computation and inserts new computations. New temporaries are
   introduced if a binding is assigned to a new value. Subclasses of
   ``<computation>`` model control flow, ``<temporary>`` (as well as
   ``<referenced-object>``) data flow.

   Computations are a doubly-linked list, with special cases for merge
   nodes, loops, if, bind-exit and unwind-protect. Every computation
   may have computation-type field, which is bound to a
   ``<type-variable>``. It also may have a temporary slot, which is its
   return value. Several cases, single and multiple return values, are
   supported. The temporary has a link to its generator, a list of
   users and a reference to its value.

   Additional (data flow) information is kept in special slots, test
   in ``<if>``, arguments of a ``<call>``, etc. These are all
   ``<referenced-object>``, or more specially ``<value-reference>``,
   ``<object-reference>``, etc. ``<object-reference>`` contains a binding
   to its actual value.

   ``<temporary>`` and ``<environment>`` classes are defined in this
   library.

   ``join-2x1`` etc. are the operations on the flow graph.

dfmc-typist
-----------

   This library contains runtime type algebra as well as a type
   inference algorithm.

   Main entry point is ``type-estimate``, which calls
   ``type-estimate-in-cache``. Each library contains a type-cache, mapping
   from method definitions, etc. to type-variables.

   Type variables contain an actual type estimate as well as
   justifications (supporters and supportees), used for propagation of
   types.

   converts types to ``<type-estimate>`` objects

   ``type-estimate-function-from-signature`` calls ``type-estimate-body``
   if available (instead of using types of the signature), call chain is
   ``type-estimate-call-from-site`` -> ``type-estimate-call-stupidly-from-fn``
   -> ``function-valtype``


   contains hard-coded hacks for ``make``, ``element``, ``element-setter``
   (in ``type-estimate-call-from-site``)

   typist/typist-inference.dylan:poor-mans-check-type-intersection 
     if #f (the temp), optimizer has determined that type check is superfluous

   dfmc/typist-protocol.dylan:151 - does not look sane!
     define function type-estimate=?(te1 :: <type-estimate>, te2 :: <type-estimate>)
      => (e? :: <boolean>, known? :: <boolean>)
       // Dylan Torah, p. 48: te1 = te2 iff te1 <= te2 & te2 <= te1
       let (sub?-1, known?-1) = type-estimate-subtype?(te1, te2);
       let (sub?-2, known?-2) = type-estimate-subtype?(te1, te2);

dfmc-optimization
-----------------

   This library contains several optimizations: dead code removal,
   constant folding, common subexpression elimination, inlining,
   dispatch upgrading and tail call analyzation.

   Main entry point from management is ``really-run-compilation-passes``.
   This loops over all lambdas in the given code fragment, converts
   assigned variables to a ``<cell>`` representation, renames temporaries
   in conditionals, then runs the "optimizer". This builds an
   optimization queue, initially containing all computations. It calls
   do-optimize on each element of the optimization-queue, as long as
   it returns ``#f`` (protocol is, that, if an optimization was successful,
   it returns ``#t``, if it was not successful, ``#f``). For different types
   of computations different optimizations are run. Default
   optimizations are deletion of useless computations and constant
   folding. ``<bind>`` is skipped, for ``<function-call>`` additionally
   upgrade (analyzes the call, tries to get rid of gf dispatch) and
   inlining is done. ``<primitive-call>`` are optimized by ``analyze-calls``.

   constant folds (constant-folding.dylan):
    // The following is because we seem to have a bogus class hierarchy
    // here 8(
    // We mustn't propagate a constraint type above its station, since
    // the constraint is typically local (true within a particular
    // branch, say).
    & ~instance?(c, <constrain-type>)

   optimization/dispatch.dylan: gf dispatch optimization

   optimization/assignment: here happens the "occurence typing"
   (type inference for instance?)...
   <constrain-type> is only for the instance? and conditionals hack

========================
Adding a DFM computation
========================

What you have to do to add a new node class to the DFM:

* Add it to ``flow-graph/computation.dylan``, and ensure that you export
   it from ``flow-graph/flow-graph-library``.

* Create the converters to generate it.  Likely in conversion, but
  some nodes are only created by optimizations.

* Make sure all the back ends handle it.  This includes, at least:

   * c-back-end
   * debug-back-end -- the printer
   * all native back ends

* In addition, it would be good to add any invariant checks to
  ``flow-graph/checker.dylan.``

====================
DFM block constructs
====================

bind-exit
---------

First, let's look at an example of bind-exit.

.. code-block:: dylan

  block (exit) exit(42); 13 end; =>
    [BIND]
    t2 := [BIND-EXIT entry-state: &t1 body: L1 exit-to: L0]
    L1:
    t4 := ^42
    t12 := exit entry-state: &t1 value: t4
    t6 := ^13
    end-exit-block entry-state: &t1
    L0:
    t7 := [MERGE t2 t6]
    return t7

(That's before register assignment, to make the difference in the
temporaries used in the merge node clear.)

The <bind-exit> node establishes the place the exit jumps to, an
<entry-state>.  This is communicated to <exit> and <end-exit-block>
through the temporary t1.  The temporary returned by the <bind-exit>
is set by the exit procedure.

(The printing code shows up one inconsistency:  the temporary geneated
by the <bind-exit> node is actually not live after that point.  It's
live only if the exit procedure is taken.  On the other hand, the
entry-state is live after that point.  Perhaps which temporary is the
generated one from a <bind-exit> node should be exchanged.)

The merge node combines the two temporaries that could contain the
result of the <merge> node -- t2 by exiting, t6 by falling through.
The <end-exit-block> node exists for at least two purposes:  to
possibly bash the exit procedure or entry state in order to prevent
calls outside of its dynamic scope and to stop a thread in the
execution engine.  It references the entry state in order that it can
be found from the <bind-exit> node.

Before we see the compiled code, here's the DFM code after register
allocation:

.. code-block:: dylan

  block (exit) exit(42); 13 end; =>
    [BIND]
    t2 := [BIND-EXIT entry-state: &t0 body: L1 exit-to: L0]
    L1:
    t1 := ^42
    t3 := exit entry-state: &t0 value: t1
    t2 := ^13
    end-exit-block entry-state: &t0
    L0:
    t2 := [MERGE t2 t2]
    return t2

And this is the C code:

.. code-block:: c

  block (exit) exit(42); 13 end; =>
    D L4988I () {
      D T0;
      D T2;
      D T1;
      D T3;
      
      T0 = dNprimitive_make_bind_exit_frame();
      if (setjmp(dNprimitive_frame_destination(T0))) {
        T2 = dNprimitive_frame_return_value(T0);
        goto L0;
      }
    L1:
      T1 = I(42);
      dNprimitive_nlx(T0, T1);
    L0:
      return(T2);
    }

The only gotcha (other than how setjmp works in C) is that the emission
engine knows that there's no point in generating code for the stuff
that follows an <exit> node;  it's a primitive form of dead code
elimination.  So that's why the ``t2 := ^13`` and ``<end-exit-block>``
nodes are not emitted.

The call to dNprimitive_nlx unwinds all <unwind-protect> frames on the
way back to the entry state marked by T0.  Eventually, (unless some
cleanup calls another exit procedure) it will longjmp to the site of
the setjmp.  The second argument to dNprimitive_nlx is shoved into the
dNprimitive_frame_return_value of the entry state.

On the other hand, if we omit the call to the exit procedure (or if
there's some control flow path which falls through, or if it isn't
inlined, as it was above), the generated code is:

.. code-block:: c

  block (exit) 13 end; =>
    D L1502I () {
      D T0;
      D T1;
      
      T0 = dNprimitive_make_bind_exit_frame();
      if (setjmp(dNprimitive_frame_destination(T0))) {
        T1 = dNprimitive_frame_return_value(T0);
        goto L0;
      }
    L1:
      T1 = I(13);
      /* invalidate T0 */
    L0:
      return(T1);
    }

Note that the call just falls through from the assignment to T1 to the
return;  no jump need take place.

The comment about invalidating reflects something I think we should
do, but haven't done yet, which is ensure that the exit procedure is
bashed when we leave the block.  Bashing a single slot should be
sufficient.

unwind-protect
--------------

Now, let's consider the DFM code for an unwind-protect:

.. code-block:: dylan

  block () xxx() cleanup yyy() end; =>
    [BIND]
    [UNWIND-PROTECT entry-state: t0 body: L1 cleanup: L2 next: L0]
    L1:
    t1 := ^xxx
    t2 := [CALLx t1()]
    end-protected-block entry-state: t0
    L0:
    return t2
    L2:
    t3 := ^yyy
    [CALLx t3()]
    end-cleanup-block entry-state: t0

I think this code is pretty straight-forward, at least in terms of the
data flow graph.  Note that t2 is live in the code outside the block
statement.

.. code-block:: c

  block () xxx() cleanup yyy() end; =>
    D L2437I () {
      D T0;
      D T1;
      D T2;
      D T3;
      
      T0 = dNprimitive_make_unwind_protect_frame();
      if (setjmp(dNprimitive_frame_destination(T0)))
        goto L2;
    L1:
      T1 = dNxxx;
      T2 = CALL0(T1);
    L2:
      T3 = dNyyy;
      CALL0(T3);
      dNprimitive_continue_unwind();
    L0:
      return(T2);
    }

The dNprimitive_continue_unwind just returns in this case.  If the
cleanup clause were invoked by an exit procedure, it would have set a
flag in the frame indicating that it continues non-local-exiting.  The
important thing to see is that the decision about whether to fall
through from the cleanup clause into the code outside the block is
made by dNprimitive_continue_unwind, based on dynamic information.

Final notes
-----------

Finally, note that a block with both an exit procedure (bind-exit) and
a cleanup clause (unwind-protect) is simply a bind-exit wrapped around
an unwind-protect.

Optimizations
-------------

Lots of optimizations can be done.  Off the top of my head:

  - Code following an <exit> is dead;  it should be dead-code
    eliminated in the DFM.

  - If an <exit> is inlined and there are no <unwind-protect>s between
    it and the <bind-exit>, it can be turned into a control transfer.

  - If there are no <exit>s for a given <entry-state>, the <bind-exit>
    node can be removed.

An invalid optimization that had been suggested was to merge nested
<unwind-protect>s without intervening <bind-exit>s with a test in the
merged cleanup to determine whether the inner cleanup is still active.
This isn't valid because then the inner cleanup is no longer protected
by the outer cleanup.

====================
DFM local assignment
====================

We really want the DFM to be a `single assignment` form.  That is,
all temporaries should be defined and then never mutated.  We want
this because it makes many optimizations (common sub-expression
elimination, inlining, etc) significantly easier.  See the usual set
of SSA papers for details;  I can dig up references.

On the other hand, Dylan has assignment to locals, and we model locals
with temporaries.  Since the DFM doesn't have cycles (loops), we could
replace assignments *to variables which aren't closed over* with
new temporaries, in the same was as SSA code is usually generated.
But all the interesting cases in Dylan are when assigned variables are
closed over, especially because they're assigned to in loop bodies.

Instead, based on Keith's suggestion, I map our Dylan-esque DFM into
one that matches how ML, at the language level, with references
(mutable variables):  all temporaries which are assigned to are
replaced with temporaries referring to boxed values.

The current approach:

I introduced three primitives:

.. code-block:: dylan

   make-box t => box             // create a box, containing t
   get-box-value box => t        // return the value inside the box
   set-box-value! box t => t     // set the value inside the box

There is a new compiler pass (eliminate-assignments) which traveres a
DFM graph and does the rewriting.

Here's an example of what happens:

.. code-block:: dylan

  begin let a = 13; a := 42; a end; => // before
    [BIND]
    t0 := ^13
    t1 := ^42
    @a := t1
    return t0
  
  begin let a = 13; a := 42; a end; => // after
    [BIND]
    t0 := ^13
    t1 := [PRIMOP primitive-make-box(t0)]
    t2 := ^42
    [PRIMOP primitive-set-box-value!(t1, t2)]
    t3 := [PRIMOP primitive-get-box-value(t1)] // tail call
    return t3

The eliminate-assignments pass should happen before any of the
`interesting` optimizations, and should never need to be done twice
on the same piece of code.

What remains to be done:

We probably want to turn these primitives into DFM computations before
trying to do any optimizations on them.

make-box currently allocates the boxed cell in the heap.  It should
really allocate the cell either a closure or stack frame, depending on
whether the box has dynamic extent.  If the temporary the box is bound
to (t1 in the example above) is only used as with get-box-value and
set-box-value!, then we know that the box has the same extent as that
temporary.  I'm don't think that all optimizations will preserve that
property, but it will probably be maintained most of the time.

When we have temporaries which aren't closed over, most of the time we
should be able to do SSA-like elimination of assignments, rewriting
them by introducing new temporaries.  For example, assignment inside a
conditional can produce something like this

.. code-block:: dylan

  begin let a = 1; if (p?) a := 2 else end; a end; =>
    [BIND]
    t2 := ^1
    t8 := [PRIMOP primitive-make-box(t2)]
    t9 := ^p?
    if (t9) goto L1 else goto L2
    L1:
    t13 := ^2
    t11 := [PRIMOP primitive-set-box-value!(t8, t13)]
    L0:
    [MERGE t11 t14]
    t10 := [PRIMOP primitive-get-box-value(t8)] // tail call
    return t10
    L2:
    t14 := ^&#f
    goto L0

but that should be easy to turn into

.. code-block:: dylan

    [BIND]
    t1 := ^p?
    if (t1) goto L1 else goto L2
    L1:
    t2 := ^2
    L0:
    t4 := [MERGE t2 t3]
    return t4
    L2:
    t3 := ^1
    goto L0

This sort of optimization, in the absence of cycles, is pretty easy.
It may be more work making it happen for loops built up from tail
calls, but still not as bad as SSA conversion in general.

===================
DFM multiple values
===================

To represent multiple values, there's a new temporary class in the
DFM, <multiple-value-temporary>.  Multiple values temporaries are not
interchangable with other temporaries;  maybe we should introduce a
<simple-temporary> class for non-multiple-value temporaries, but we
can do that later.  In the debugging print code, MV temporaries print
with a * in front of them.

A multiple value temporary is the result of any computation which can
produce multiple values, notably a call.

In order to produce efficient code, we have imposed the requirement that
at most one MV temporary is live at a time (per thread).  This allows
us to allocate space for all MV temporaries ahead of time, as part of
the calling convention, in the `multiple value area`.  It is
generally best to think of the multiple value area, which is used to
pass multiple values across calls, as a single multiple valued
register, which we allocate to the live MV temporary.

When there really is more than one live MV temporary, we must spill
and unspill uses.  One of the important optimizations is to reduce
these spills when the number of values in a MV temporary is known, by
extracting them into normal temporaries and repackaging them as an MV
temporary when needed as one.

A multiple value temporary has slots which describe the number of
required values and whether there are rest values.  Types need to be
incorporated here, just as with other temporaries.  There's also a
slot for a normal temporary, which is used when spilling the multiple
value temporary.

To manipulate multiple values, there are five new computation classes:

  <values>

    super: <computation>
    slots: fixed-values, rest-value

    Creates a ``<multiple-value-temporary>`` from a set of single value
    temporaries.  For now, a ``<values>`` node comes from a converter
    for the `function macro` values;  in the future, there should
    be only one ``<values>`` node created directly, and the rest created
    by inlining the function values from the Dylan library.  (A
    similar change needs to be made for ``<apply>``.)

.. code-block:: dylan

      values(1, 2, 3) =>
        [BIND]
        t0 := ^1
        t1 := ^2
        t2 := ^3
        *t3 := [VALUES t0 t1 t2]
        return *t3

  <extract-single-value>

    super: <computation>
    slots: multiple-values, index, rest-vector?

    Produces a single-valued temporary from an MV temporary.  The
    index is used to select which multiple value is extracted;  the
    indices are numbered from 0.  If rest-vector? is true, a vector
    of the values from index on is returned, rather than just the
    index.  (Perhaps that should be a different <computation> class.)

    These very commonly follow calls, extracting the single value.
    They should also appear based on optimizations of let bindings.

.. code-block:: dylan

      f(g()) =>
        [BIND]
        t0 := ^f
        t1 := ^g
        *t2 := [CALLx t1()]
        t3 := *t2 [0]
        *t4 := [CALLx t0(t3)] // tail call
        return *t4

  <multiple-value-call>

    super: <function-call>

    Like an <apply> with no fixed arguments and a MV temporary as the
    single (last) argument.  Constructed from ``let'' declarations
    which bind multiple values.  (This could be used for all lets, but
    I wanted to wait with that until the multiple value optimizations
    were in place.)

    The most important optimization with these nodes is to upgrade
    the calls to <simple-call> or <apply> with the shape of the
    MV temporary argument is know.  If it's not known, the simplest
    code generation strategy is to extract all of the temporary
    values and transform the call into an <apply>.

.. code-block:: dylan

      begin let (a, b) = f(); g(a, b) end =>
        [BIND]
        t3 := ^[XEP lambda 741 [743] (a, b)
          [BIND]
          t0 := ^g
          *t1 := [CALLx t0(a, b)] // tail call
          return *t1
        end lambda]
        t0 := ^f
        *t1 := [CALLx t0()]
        *t2 := [MV-CALLx t3(*t1)] // tail call
        return *t2

  <multiple-value-spill>
  <multiple-value-unspill>

    super: <temporary-transfer>

    These instructions turn an MV temporary into a single-value
    temporary and vice-versa, for the purpose of maintaining the
    property that a single MV temporary is live at a time.  As much as
    possible, we should try to avoid these instructions in generated
    code, which can be done when we know we're dealing with a fixed
    number of values.

    These computations are only generated by the mandatory compiler
    pass ``spill-multiple-values,'' which should run after all
    optimizations have happened.  (The reason that it should run
    afterwards is the spill code can defeat other optimizations and
    other optimizations can get rid of the need to spill.)

.. code-block:: dylan

      block () f() afterwards g() end =>
        [BIND]
        t0 := ^f
        *t1 := [CALLx t0()]
        t3 := [MV-SPILL *t1]
        t2 := ^g
        [CALLx t2()]
        *t4 := [MV-UNSPILL t3]
        return *t4

    The reason the spill is needed is that the call to g tramples over
    the multiple value area.

In the C run time, there's an extra data structure, MV, as follows:

.. code-block:: c

  typedef struct _mv {
    int count;
    D   value[VALUES_MAX];
  } MV;

There's one global such thing (Preturn_values), and one per bind-exit
or unwind-protect frame, used for the return value that's being passed
around.  The ones that live in those frames should probably be
shortened to some small number of values (2? 4? 8?) and evacuate to
the heap if more multiple values are stored;  it's pretty rare, I
expect, for a large number of values to appear in an unwind-protect
frame, or to be passed back with an exit procedure.

The C code generated for all of these is pretty stupid right now,
calling out to primitives in all cases, so I won't bother to present
it.  I want to get to the task of optimizing multiple values soon.  I
think that a little bit of optimization will go a long way here.

In the native run-time, we'll pass the first few multiple values and
(if there is one) the count in registers.  Tony can describe that far
better than I can.

=============================
define compilation-pass macro
=============================

NOTE: this is currently not used at all - it had been dropped before
going open source, but in general I (hannes) believe it is a good
idea (and plan to revive it), thus I keep the documentation.

I've now replaced the old mechanism for specifying compilation passes
in the DFM compiler (setting the vector *compilation-passes* in
compile.dylan) with a declarative system, based around a macro, define
compilation-pass.

The macro is exported by dfmc-common, so every module should have it.
The basic idea is that you put a compilation-pass definition in the
same place as you define the main entry point for a compiler-pass;
the definition includes things about the pass, such as when its run,
how it is called, and if it should cause other passes to run.

First, a simple example:

.. code-block:: dylan

  define compilation-pass eliminate-assignments,
    visit: functions,
    mandatory?: #t,
    before: analyze-calls;

This defines a pass named `eliminate-assignments`, which runs before
analyze-calls is run;  it is possible to use arbitrarily many before:
options.  The mandatory option declares that the pass is part of
optimization level 0;  that is, it's always run.

The `visit: functions` option says that the function is called for
every function in the form being compiled.  The default is
`visit: top-level-forms`, which corresponds to the previous behavior.

.. code-block:: dylan

  define compilation-pass try-inlining,
    visit: computations,
    optimization: medium,
    after: analyze-calls,
    before: single-value-propagation,
    triggered-by: analyze-calls,
    trigger: analyze-calls;

The `visit: computations` option says that every computation (in the
top-level and all nested lambdas) is passed to the pass's function.
The after: option is like before: in reverse.

The trigger: option runs the named pass if the pass being defined
reports that it changed anything.  If the triggered pass has already
run, then it is queued to run again;  if the triggered pass is
disabled or of a higher optimization level than currently being used,
it's not run.  Triggered-by: is trigger: in reverse.

A pass function reports that it changed something by returning any
non-false value.


Full catalog of options:

  visit:             What things to pass to the pass's function:
    top-level-forms    Just the top-level function.
    functions          Every function.
    computations       Every computation in every function.

  optimization:      What level of optimization to run this pass for?
                     (Choices:  mandatory, low, medium, high.)

  mandatory?:        Always run this pass;  overrides optimization:.

  before:            Run this pass before the named one.
  after:             Run this pass after the named one.

  trigger:           If this pass changed something, run the named pass.
  triggered-by:      If the named pass changes something, run this pass.

  print-before?:     Print the DFM code before calling the pass.
  print-after?:      Print the DFM code after the pass is done.
  print?:            Same as print-before?: #t and print-after?: #t.

  check-before?:     Call ensure-invariants before calling the pass.
  check-after?:      Call ensure-invariants after the pass is done.
  check?:            Same as check-before?: #t and check-after?: #t.

  back-end:          Turn pass on for the named back end. (Default: all)
  exclude-back-end:  Turn pass off for the named back end. (Default: none.)

  disabled?:         Turn pass off;  overrides everything else.

Convenience functions:

  trace-pass(pass-name)
  untrace-pass(pass-name)

    Turns on (or off) printing and checking (both before and after)
    for the pass.

  untrace-passes()

    Calls untrace-pass for all traced passes.


Global state:

The fluid-variable *optimization-level* is meant to be a gross control
of how much optimization is done.  The constants

.. code-block:: dylan

  define constant $optimization-mandatory = 0;
  define constant $optimization-low       = 1;
  define constant $optimization-medium    = 2;
  define constant $optimization-high      = 3;
  
  define constant $optimization-default   = $optimization-medium;

are defined and correspond to the optimization: option in the define
compilation-pass macro.

The fluid-variable *back-end* is used with the options back-end: and
exclude-back-end:.

The fluid-variable *trace-compilation-passes* will print a message
about each pass as it runs, and report when one pass triggers another.

========
Glossary
========

canonical sources
   only meaningful in reference to a particular compiler database or
   compilation context. The set of sources from which the information
   in the database was derived.

compilation context
   the in-memory representation of an open compiler database. Consists
   of a connection to a disk database, a reference to the owning project,
   and a collection of caches for pre-loaded/pre-computed information.

compiler database
   a file or set of files containing information derived from
   compiling a project. A project can have multiple compiler databases,
   corresponding to different target machines, compiler settings, or
   even different versions of the project sources. The project manager is
   responsible for managing the collection of project databases and
   telling the compilation system which database to use.

component
   a native DLL or EXE file.

execution context
   the compiler-derived information about a process, such as the
   namespaces of known runtime components, installed definitions, etc.
   Initialized from the compilation contexts of the preloaded runtime
   components and subsequently updated by interactive execution.

interactive execution
   a mechanism for exploratory programming by which the user can
   execute Dylan forms in an existing process. May allow “out of
   language” operations such as addition of new variables to existing
   modules, redefinition of classes, constants, overriding sealing
   restrictions, etc. Forms to be executed can come from a project or
   from an interactor.

project
   a development environment object representing a Dylan program under
   development. It corresponds to a Dylan library, but it exists before
   the compiler is ever invoked. It is used by the compiler only to
   identify a library in interactions with the project manager.

project manager
   the part of the development environment charged with managing
   projects. The project manager is a client of the compilation system,
   i.e. it is the project manager which is expected to invoke many of the
   functions in this API.

runtime component
   a runtime manager object representing a component loaded into a
   tethered process.

interactor
   a mechanism by which a user can type in source records for
   interactive execution without modifying project sources.

runtime manager
   the part of the development environment charged with controlling
   the runtime. The compilation system is a client of the runtime
   manager, i.e. the compilation system will invoke runtime manager
   functions as needed to effect interactive execution.

source record
   smallest unit of source suitable for compilation. Consists of a
   stream of characters and a module name. Contains complete top-level
   forms (i.e. top-level forms may not be split across source records).

tether
   a runtime manager object representing a debuggable process on the
   runtime. Sometimes referred to as an “access-path”, but I’m staying
   away from that term because it seems to be used differently in
   different documents.


