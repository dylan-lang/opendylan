*****************************
Open Dylan Compiler Internals
*****************************

Introduction
============

This chapter is an overview of the libraries involved during
compilation, information was gathered while hacking on the compiler.
It focuses only on DFMC, the Dylan Flow Machine Compiler, located in
``source/dfmc`` of the opendylan repository.

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
===============

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
===========

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
================

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

Excursion into run-time and compile-time
========================================

NB: not sure whether this should be here or somewhere different.

Some objects are defined in the compiler, but are injected into the
Dylan world. How does this happen?

So, in the Dylan library you see ``// BOOTED:`` comments here and
there. The source location of well-known basic types and functions is
dylan:dylan-user:boot-dylan-definitions().

There is no definition of this specific method.

The method dfmc-definitions:top-level-convert.dylan:
boot-definitions-form? checks exactly for this name. The method
top-level-convert-forms behaves differently if boot-definitions-form?
returns true, namely it calls booted-source-sequence(), which is
defined in boot-definitions.dylan. This method grabs the boot-record
and returns it sorted as a vector.

But what is a boot-record after all? Well, it's definition is all in
boot-definitions.dylan, with the explanation "records the set of
things that must be inserted into a Dylan world at the very
start. Some of things are core definitions, such as converters and
macros, and these are booted at the definition level. The rest are
expressed as source to be fed to the compiler."

The constant ``*boot-record*`` is filled by do-define-core-\*. These
are called by dfmc-modeling. Namely, primitives (which names and
signatures are installed), macros, modules, libraries, classes.

Be aware that the actual implementation of the primitives is in the
runtime (either ``sources/lib/c-run-time/run-time.c`` or the
runtime-generator generates a runtime.o containing those definitions),
but some crucial bits, like the adjectives (``side-effect-free``,
``dynamic-extent``, ``stateless`` and ``opposited``) are in
dfmc-modeling and used in the optimization!

The core classes are emitted from modeling with actual constructors
(be aware that the runtime layout is also recorded in run-time.h).

The dylan library and module definitions are in
modeling/namespaces.dylan.

A noteworthy comment is that a compiler (comp-0, generation 0) loads
the Dylan library (dylan-0), which contains the definitions
(defs-0). When compiling itself (comp-1), first a fresh Dylan library
(dylan-1) is built, which contains still the old booted definitions
(defs-0). It emits new definitions (defs-1) and a new boot-record when
dumping dfmc-definitions. Now the next generation compiler (comp-1)
will use these new definitions in the next Dylan (dylan-2)
library. Beware of dragons.


dfmc-macro-expander
===================

The deep magic happens here.

dfmc-convert
============

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
=============

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
===============

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
===========

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

.. code-block:: dylan

     define function type-estimate=?(te1 :: <type-estimate>, te2 :: <type-estimate>)
      => (e? :: <boolean>, known? :: <boolean>)
       // Dylan Torah, p. 48: te1 = te2 iff te1 <= te2 & te2 <= te1
       let (sub?-1, known?-1) = type-estimate-subtype?(te1, te2);
       let (sub?-2, known?-2) = type-estimate-subtype?(te1, te2);

dfmc-optimization
=================

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

.. code-block:: dylan

    // The following is because we seem to have a bogus class hierarchy
    // here 8(
    // We mustn't propagate a constraint type above its station, since
    // the constraint is typically local (true within a particular
    // branch, say).
    & ~instance?(c, <constrain-type>)

 optimization/dispatch.dylan: gf dispatch optimization

optimization/assignment: here happens the "occurrence typing"
(type inference for instance?)...
``<constrain-type>`` is only for the instance? and conditionals hack

