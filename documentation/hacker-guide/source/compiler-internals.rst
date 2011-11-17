*****************************
Open Dylan Compiler Internals
*****************************

Introduction
------------

This chapter is an overview of the compiler, information was gathered
while hacking on the compiler. It focusses only on DFMC, the Dylan
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

To explain these long call chain, we need some more understanding of
the different libraries of the compiler: environment is the public
API, project-manager is a bunch of hacks to care about finding the
project (by using the registry) and calling the compiler, and the
linker (to create a dll/so and executable) afterwards. The libraries
``dfmc/browser-support`` and ``environment/dfmc`` are the glue from
environment to dfmc.

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
* the metadata of a library
   is stored in DOOD, the dylan object-oriented database
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
which prints 11 lines, ``Hello 0`` to ``Hello 10``:

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

The main external entry point is compile-library-from-definitions in
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
   locations, and builds a parse tree.

   The real parser is generated by app/parser-compiler from
   parser.dylgram, which is basically the BNF notation of Dylan from
   DRM.

dfmc-definitions
----------------

   Once the parse tree is built, the definitions library instantiates
   the semantic Dylan definition objects, like <library-definition>,
   <class-definition>, <method-definition>, etc. So, the parse tree
   gets converted to real Dylan objects, while the body of each TLF is
   only checked for consistence (nesting, parens, end keywords).

   The parsing is done with pattern matching, using Dylan
   macros. Those are non-standard Dylan macros, but dexpr (JRB paper),
   because they allow real Dylan code on the right-hand side, thus
   breaking the circularity, which would otherwise arise by
   transforming Dylan code to Dylan code. A small example is the
   following code snippet:

   macro-case(fragment)
    { ?:name :: ?type:expression } => make(<foobar>, name: name, type: type);

   But Dylan fragments can be emitted on the right-hand side, by using
   "#{ }":

      { ?:name :: ?type1:* => ?type2:*, ?parameters }
        => begin
             let args = parse-function-type(type1);
             let vals = parse-function-type(type2);
             collect-first-into
               (required, make(<typed-required-variable-spec>,
                               variable-name:   name,
                               type-expression: as-expression(#{ limited(<function>, arguments: ?args, values: ?vals) })));
           end;

   Errors like unrecognized keyword (XXX: uh, really? not only keyword
   balancing?) and invalid definition statement, are reported in the
   definitions library. (XXX: more examples!)

   In this library, the signature parser was extended to recognize
   type variables as well as function types.

dfmc-macro-expander
-------------------

Expands macros. Who calls it?

dfmc-convert
------------

   Converts definition objects to model objects. In order to fulfill
   this task, it looks up bindings to objects from other
   libraries. Also converts the bodies of definitions to a flow
   graph. Does some initial evaluation, for example "limited(<vector>,
   of: <string>)" gets converted to a "<&limited-vector-type>"
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
   an ampersand (<&object>).

dfmc-modeling
-------------

   Contains modeling of runtime and compile time objects. Since some
   calls are tried to be done at compile time rather than at runtime,
   it provides these compile time methods with a mechanism to override
   the runtime methods ("define &override-function"). An example for
   this is "^instance?", compile time methods are prefixed with a "^",
   while compile and runtime class definitions are prefixed with "&",
   like "define &class <type>".

   Also, dood (a persistent object store) models and proxies for
   compile time definitions are available in this library, in order to
   load definitions of dependent libraries.

   This library was extended with "<type-variable>" class hierarchy as
   well as "^limited(<function>)" and "<limited-function-type>" were
   introduced.

dfmc-flow-graph
---------------

   The flow graph consists of instances of the "<computation>" class,
   like "<if>", "<loop-call>", "<assignment>", "<merge>". The flow
   graph is in a (pseudo) single state assignment form. Every time any
   algorithm alters the flow graph, it disconnects the deprecated
   computation and inserts new computations. New temporaries are
   introduced if a binding is assigned to a new value. Subclasses of
   <computation> model control flow, <temporary> (as well as
   <referenced-object>) data flow.

   Computations are a doubly-linked list, with special cases for merge
   nodes, loops, if, bind-exit and unwind-protect. Every computation
   may have computation-type field, which is bound to a
   <type-variable>. It also may have a temporary slot, which is its
   return value. Several cases, single and multiple return values, are
   supported. The temporary has a link to its generator, a list of
   users and a reference to its value.

   Additional (data flow) information is kept in special slots, test
   in <if>, arguments of a <call>, etc. These are all
   <referenced-object>, or more specially <value-reference>,
   <object-reference>, etc. <object-reference> contains a binding to
   its actual value.

   "<temporary>" and "<environment>" classes are defined in this
   library.

   "join-2x1" etc. are the operations on the flow graph.

   Thid was extended by "<lexical-required-type-variable>", instances
   of this class are put into the lexical environment.

dfmc-typist
-----------

   This library contains runtime type algebra as well as a type
   inference algorithm.

   Main entry point is type-estimate, which calls
   type-estimate-in-cache. Each library contains a type-cache, mapping
   from method definitions, etc. to type-variables.

   Type variables contain an actual type estimate as well as
   justifications (supporters and supportees), used for propagation of
   types.

   converts types to <type-estimate> objects

   type-estimate-function-from-signature calls type-estimate-body if
   available (instead of using types of the signature), call chain is
   type-estimate-call-from-site -> type-estimate-call-stupidly-from-fn
   -> function-valtype


   contains hard-coded hacks for make, element, element-setter (in
   type-estimate-call-from-site)

   typist/typist-inference.dylan:poor-mans-check-type-intersection 
     if #f (the temp), optimizer has determined that type check is superfluous

   dfmc/typist-protocol.dylan:151 - does not look sane!
     define function type-estimate=?(te1 :: <type-estimate>, te2 :: <type-estimate>)
      => (e? :: <boolean>, known? :: <boolean>)
       // Dylan Torah, p. 48: te1 = te2 iff te1 <= te2 & te2 <= te1
       let (sub?-1, known?-1) = type-estimate-subtype?(te1, te2);
       let (sub?-2, known?-2) = type-estimate-subtype?(te1, te2);

   This complete library has been rewritten.

dfmc-optimization
-----------------

   This library contains several optimizations: dead code removal,
   constant folding, common subexpression elimination, inlining,
   dispatch upgrading and tail call analyzation.

   Main entry point from management is really-run-compilation-passes.
   This loops over all lambdas ín the given code fragment, converts
   assigned variables to a <cell> representation, renames temporaries
   in conditionals, then runs the "optimizer". This builds an
   optimization queue, initially containing all computations. It calls
   do-optimize on each element of the optimization-queue, as long as
   it returns #f (protocol is, that, if an optmization was successful,
   it returns #t, if it was not successful, #f). For different types
   of computations different optimizations are run. Default
   optimizations are deletion of useless computations and constant
   folding. <bind> is skipped, for <function-call> additionally
   upgrade (analyzes the call, tries to get rid of gf dispatch) and
   inlining is done. <primitive-call> are optimized by analyze-calls.

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



