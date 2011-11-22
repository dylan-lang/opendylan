************************
Publications about Dylan
************************

`Extending Dylan's type system for better type inference and error detection - Hannes Mehnert - ILC 2010 <http://www.itu.dk/~hame/ilc2010.pdf>`_

    Whereas dynamic typing enables rapid prototyping and easy
    experimentation, static typing provides early error detection and
    better compile time optimization. Gradual typing provides the best
    of both worlds. This paper shows how to define and implement
    gradual typing in Dylan, traditionally a dynamically typed
    language. Dylan poses several special challenges for gradual
    typing, such as multiple return values, variable-arity methods and
    generic functions (multiple dispatch).

    In this paper Dylan is extended with function types and parametric
    polymorphism. We implemented the type system and a
    unification-based type inference algorithm in the mainstream Dylan
    compiler. As case study we use the Dylan standard library (roughly
    32000 lines of code), which witnesses that the implementation
    generates faster code with fewer errors. Some previously
    undiscovered errors in the Dylan library were revealed.

    https://dl.acm.org/citation.cfm?id=1869643.1869645

`Automatically generated type-safe GTK+ binding for Dylan - Hannes Mehnert - ILC 2009 <http://www.itu.dk/~hame/ilc09.pdf>`_

    We present an automated way to get language bindings for GTK+ for
    Dylan, an object-oriented functional programming language related
    to Lisp. Dylan supports multiple inheritance, polymorphism,
    multiple dispatch, keyword arguments, pattern-based syntax
    extension macros, and many other features. The generated binding
    is type-safe, no up- and downcasts are needed.


`A domain-specific language for manipulation of binary data in Dylan - Hannes Mehnert and Andreas Bogk - ILC 2007 <http://www.itu.dk/~hame/ilc07-final.pdf>`_

    We present a domain specific language for manipulation of binary
    data, or structured byte sequences, as they appear in everyday
    applications such as networking or graphics file manipulation. Our
    DSL is implemented as an extension of the Dylan language, making
    use of the macro facility. Dylan macros, unlike Common Lisp
    macros, are implemented as pattern matches and substitutions on
    the parse tree, and we will show the strengths and limits of this
    approach for the given problem.

    https://dl.acm.org/citation.cfm?id=1622123.1622148

`Efficient Compression of Generic Function Dispatch Tables - Eric Kidd   - Technical Report Dartmouth College Hanover, NH, USA ©2001 <http://www.cs.dartmouth.edu/reports/TR2001-404.pdf>`_

    A generic function is similar to an overloaded operator, but
    provides a way to select an appropriate behavior at run-time
    instead of compile-time. Dujardin and colleagues have proposed an
    algorithm for building and compressing generic function dispatch
    tables.

    We present several modifications to their algorithm, including an
    improvement to Pseudo-Closest-Poles and two new algorithms for
    compressing pole tables. The two new compression algorithms are
    simple and fast, and one produces smaller output than the
    original.

    https://dl.acm.org/citation.cfm?id=867862

`Partial Dispatch: Optimizing Dynamically-Dispatched Multimethod Calls with Compile-Time Types and Runtime Feedback - Jonathan Bachrach and Glenn Burke - Technical Report 2000 <http://people.csail.mit.edu/jrb/Projects/pd.pdf>`_

    We presented an approach to gaining back complete class hierarchy
    information by delaying the construction of dispatch caches until
    the whole class hierarchy is available at run- time. Run-time
    call-site caches can then be constructed as specialized decision
    trees built from disjointness and concrete- subtype operations on
    actual arguments combined with compile-time inferred types
    injected into the run-time. Unnecessary decision steps can be
    avoided and often run-time dispatch can be completely
    eliminated. We consider this to be a nice half-way house between
    full static compilation and dynamic compilation which mitigates
    the runtime expense of separately compiled components while
    satisfying our implementation constraints of code shareable
    components, multi-threaded runtime, incremental development, “pay
    as you go philosophy”, and interoperability with standard tools.

`D-Expressions: Lisp Power, Dylan Style - Jonathan Bachrach and Keith Playford - Technical Report 1999 <http://people.csail.mit.edu/jrb/Projects/dexprs.pdf>`_

    This paper aims to demonstrate that it is possible for a language
    with a rich, conventional syntax to provide Lisp-style macro power
    and simplicity. We describe a macro system and syntax manipulation
    toolkit designed for the Dylan programming language that meets,
    and in some areas exceeds, this standard. The debt to Lisp is
    great, however, since although Dylan has a conventional algebraic
    syntax, the approach taken to describe and represent that syntax
    is distinctly Lisp-like in philosophy.

`A Monotonic Superclass Linearization for Dylan - Kim Barrett and Bob Cassels and Paul Haahr and David A. Moon and Keith Playford and P. Tucker Withington - OOPSLA 1996 <http://192.220.96.201/dylan/linearization-oopsla96.html>`_

    Object-oriented languages with multiple inheritance and automatic
    conflict resolution typically use a linearization of superclasses
    to determine which version of a property to inherit when several
    superclasses provide definitions. Recent work has defined several
    desirable characteristics for linearizations, the most important
    being monotonicity, which prohibits inherited properties from
    skipping over direct superclasses. Combined with Dylan's sealing
    mechanism, a monotonic linearization enables some compile-time
    method selection that would otherwise be impossible in the absence
    of a closed-world assumption.The Dylan linearization is monotonic,
    easily described, strictly observes local precedence order, and
    produces the same ordering as CLOS when that is monotonic. We
    present an implementation based on merging and a survey of class
    heterarchies from several large programs, analyzing where commonly
    used linearizations differ.

    https://dl.acm.org/citation.cfm?id=236337.236343

`Discovering the way programmers think about new programming environments - Joseph Dumas and Paige Parsons - Communications of the ACM June 1995 <https://dl.acm.org/citation.cfm?id=203253>`_

    An interesting usability study of a prototype development
    environment for the Dylan programming language is presented
    here. This study's purpose is to determine just how close the
    prototype is to developers. New approaches to source code
    organization and to the relationship between the environment and
    the application being developed are introduced. An asessment of
    how effectively the prototype conveys these innovations to Dylan
    developers is also given, followed by some proposed improvements.


`Bibliography in bibtex format <publications.bib>`_

