********
Glossary
********

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

DFMC
  the Dylan Flow Machine Compiler, the intermediate representation on
  which type inference and optimization (dispatch, inline, dynamic
  extent, common subexpression elimination, constant folding, dead
  code removal) is done. Source of data and control flow elements is
  in *dfmc/flow-graph*.

execution context
   the compiler-derived information about a process, such as the
   namespaces of known runtime components, installed definitions, etc.
   Initialized from the compilation contexts of the preloaded runtime
   components and subsequently updated by interactive execution.

HARP
  Harlequin Abstract RISC Machine, the native back-end of Open Dylan.
  On this representation register allocation etc is done: input DFM,
  output: assembly. Source is in *harp* subdirectory.

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


