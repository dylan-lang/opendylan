**********************
Jam-based Build System
**********************

The purpose of the Open Dylan :lib:`build-system` is to
coordinate the final stages in the building of a Dylan library project.
The Open Dylan compiler takes a Dylan library in the form of
Dylan source files, and depending on the selected back-end produces:

- LLVM bitcode files (``llvm`` back-end)
- C source files (``c`` back-end)
- x86 machine language object files in COFF format (``harp`` back-end)

For the first two cases a final code generation step needs to be
invoked, and in all cases object files need to be linked together to
construct either an executable object file, or a loadable
dynamically-linked library (also called a shared library). This link
needs to be performed by an external tool such as the Microsoft or GNU
linker. The :lib:`build-system` component, controlled by a user-specified
script file, directs the execution of these external tools.

Why Jam-based?
==============

Use of the Jam scripting language increases the flexibility of Open
Dylan for users, and makes it more maintainable by removing hard-coded
assumptions spread throughout various layers of the compiler. Early
versions of the Open Dylan :lib:`build-system` component permitted users
to choose either the Microsoft or GNU Binutils linker on the Win32
platform, and always used the GNU Binutils ELF linker on Linux
platforms. Later versions of these linkers and associated toolchains
required modifications that went beyond the original flexibility of
the :lib:`build-system` component, as did newer Open Dylan target
platforms.

Though the logic of the original :lib:`build-system` (and a former
companion library, :lib:`linker-support`) was hard-coded, it did allow a
limited amount of parameterization using script files. A typical
parameterization, from the Microsoft linker script, looked like this::

    #
    linkdll
    link /NODEFAULTLIB /INCREMENTAL:NO /PDB:NONE /NOLOGO \
      /ENTRY:$(mangled-dllname)Dll@12 -debug:full -debugtype:cv \
      /nologo /dll /out:$(full-dll-name) kernel32.lib \
      @$(dllname).link $(objects) $(c-libs) /base:$(base) \
      /version:$(image-version) $(linkopts)

Though these script files were poorly documented and insufficiently
flexible, they did inspire the introduction of a real scripting language
to direct the final stages of compilation in Open Dylan.

`Jam`_ is a build tool designed by Christopher Seiwald, founder of
`Perforce Software`_. It is similar in some ways to
:program:`make`, the traditional Unix build tool. However, instead of using
only simple declarative rules to define build targets and the
dependencies between them, Jam contains a full scripting language,
allowing build script authors to define high-level build instructions
that match particular applications. The Jam program also includes
Jambase, a library of rules (functions) for building executables and
libraries from C, C++, and Fortran sources.

The original Jam tool is a standalone program written in C and YACC.
Peter Housel re-implemented the Jam language interpreter and build logic
as a reusable Dylan library for use in the Open Dylan
:lib:`build-system`.

Choosing Build Scripts
======================

Normally you can simply use the build script supplied with Open Dylan
that corresponds to the external linker you will be using. The
supplied build scripts include the following:

:file:`x86_64-linux-build.jam`
    Build script for x86_64 Linux systems using Clang or gcc.
:file:`x86-win32-vc6-build.jam`
    Build script for Microsoft Visual C++ 6.0.
:file:`x86-win32-vc7-build.jam`
    Build script for Microsoft Visual C++ .NET.

The default build script is ``platform-name-build.jam``. You can
select a different build script from the :guilabel:`Link` page of the
:guilabel:`Environment Options` dialog in the IDE, or using the
``-build-script`` option on the console compiler or console
environment command-line.

Build scripts are written using the Jam script language, as described in
the `Jam manual page`_. Most Open Dylan build scripts ``include`` the
``mini-jambase.jam`` file, which contains excerpts from the `Jambase`_
file included with Perforce Jam and described in the `Jambase Reference`_.
They can also make use of additional built-in rules defined by the
Open Dylan build system, as described in `Additional Built-In Jam Rules`_
and `Built-In Jam Variables`_.

How the Compiler Uses the Build System
======================================

When you compile a library, the Open Dylan compiler constructs
a new :file:`build` directory and places the generated object files in it.
It also constructs a text file called :file:`dylanmakefile.mkf` to be read
by the build system. This file contains information imported from the
original LID or HDP project file, as well as information generated
during compilation. Here is a sample :file:`dylanmakefile.mkf`, in this case
the one generated for the ``build-system`` component itself:

::

    comment:        This build file is generated, please don't edit
    library:        build-system
    base-address:   0x63F20000
    major-version:  0
    minor-version:  0
    library-pack:   0
    compilation-mode:       tight
    target-type:    executable
    files:  library
            paths
            variables
            build
            jam-build
    used-projects:  functional-dylan
            dummy
            ..\functional-dylan\
            io
            dummy
            ..\io\
            system
            dummy
            ..\system\
            file-source-records
            dummy
            ..\file-source-records\
            release-info
            dummy
            ..\release-info\
            dfmc-mangling
            dummy
            ..\dfmc-mangling\
            jam
            dummy
            ..\jam\
    all-c-libraries: advapi32.lib
            shell32.lib

External files are used to communicate with the build system in order
for the information to persist between invocations of the compiler. On
the Win32 platform, :file:`dylanmakefile.mkf` files are also copied into the
:file:`lib` directory on installation so that other libraries can link
against the actual DLL (whose name might not be identical to the library
name).

When Open Dylan needs to link a project, it calls the
:lib:`build-system`, passing the name of the build directory and a list of
targets to be built. The build system reads the :file:`dylanmakefile.mkf`
file and builds the targets accordingly.

The Open Dylan compiler's project manager expects the build
script to define the following pseudo (``NotFile``) targets:

``dll``
    Link the project as a dynamically-linked library.
``unify-dll``
    Not currently supported.
``exe``
    Link the project as an executable program.
``unify-exe``
    Link the project as an executable program, with all Dylan libraries
    statically linked.
``release``
    Not currently supported. (Requested when the build ``-release``
    option is used. This option was intended, prior to the open-source
    release, to copy Redistributable versions of referenced system
    Dylan libraries into the target build.)
``clean-all``
    Remove build products in the top-level project, and in all of the
    non-system libraries that it uses.
``clean``
    Remove build products in the top-level project.

Automatically-invoked Jam Rules
===============================

When the build system reads a :file:`dylanmakefile.mkf` file, it invokes
several of the Jam rules (functions) defined in the user's build script.
These rules in turn register the necessary targets and their
dependencies with the Jam build mechanism.

All of the rules described below take *image* as their first parameter;
this is a list whose first element is the library name (from the
``Library:`` keyword of the ``.mkf`` file) and whose optional second
component is the base name of the executable or shared library (from the
``Executable:`` keyword of the ``.mkf`` file).

``DylanLibrary`` *image* ``:`` *version* ``:`` *dir* ``;``
    Link a Dylan library as a shared library or executable image. This is
    always the first rule invoked for a given library, and it is usually
    charged with establishing the library targets and setting global and
    target-specific variables.

    The *version* argument normally contains two components, the first
    obtained from the ``Major-version:`` keyword of the ``.mkf`` file, and
    the second from the ``Minor-version:`` keyword.

    The *dir* argument points to the project source directory (where
    the LID file is located). This is used to set the ``SEARCH_SOURCE``
    during the processing of the ``.mkf`` file.

``DylanLibraryLinkerOptions`` *image* ``:`` *options* ``;``
    Add the given options to the link command line of the shared library and
    executable images. The link options provided in the ``Linker-options:``
    keyword of the ``.mkf`` file are expanded using the usual Jam variable
    expansion rules before being passed to this rule. (This allows
    ``Linker-options:`` keywords in LID and HDP files to refer to
    platform-specific variables such as ``$(guilflags)``).

``DylanLibraryBaseAddress`` *image* ``:`` *address* ``;``
    Set the base address of the shared library. The compiler-computed
    base addresses are only usable on the Win32 platform.

``DylanLibraryCLibraries`` *image* ``:`` *libraries* ``;``
    Link C (or other externally-derived) libraries into the shared library.
    The link options provided in the ``C-libraries:`` keyword of the
    ``.mkf`` file are expanded using the usual Jam variable expansion rules
    before being passed to this rule.

``DylanLibraryCObjects`` *image* ``:`` *objects* ``;``
    Link C (or other externally-derived) object files into the shared
    library.

``DylanLibraryCSources`` *image* ``:`` *sources* ``;``
    Link C source files into the shared library.

``DylanLibraryCHeaders`` *image* ``:`` *headers* ``;``
    This rule normally does nothing. The ``C-header-files:`` HDP/LID
    file keyword that corresponds to it was originally used to ensure
    that files of various sorts (not just C header files) were copied
    into the build directory, but this is no longer done.

``DylanLibraryC++Sources`` *image* ``:`` *sources* ``;``
    Link C++ source files into the shared library.

``DylanLibraryRCFiles`` *image* ``:`` *rcfiles* ``;``
    Link Win32 resource files into the shared library and executable.

``DylanLibraryJamIncludes`` *image* ``:`` *includes* ``;``
    Include other Jam files into the build definition. This is typically
    used via the ``jam-includes:`` keyword in the HDP/LID file. It is
    useful for setting up extensions to library or include search
    paths. Within the body of the included file, the ``IMAGE`` variable
    is bound to the *image* argument values.

``DylanLibraryUses`` *image* ``:`` *library* ``:`` *dir* ``;``
    Link other Dylan libraries into the shared library or
    executable. The *library* argument gives the name of the other
    library, and the *dir* argument gives the name of the other
    library's build directory. If *dir* is ``system``, then the
    library is an installed system library.

Additional Built-In Jam Rules
=============================

The build system defines the following additional built-in rules.

``IncludeMKF`` *includes* ``;``
    Read each of the given ``.mkf`` files and invoke Jam rules as described
    in `Automatically-invoked Jam Rules`_.

``DFMCMangle`` *name* ``;``
    Mangle the given *name* according to the Open Dylan compiler's
    mangling rules. If *name* has a single component, it is considered to be
    a raw name; if there are three components they correspond to the
    variable-name, module-name, and library-name respectively.

The :lib:`jam` engine itself defines the following rules, which were not
included in the original Perforce Jam but are found in extended
implementations such as Boost Jam, FT Jam, or JamPlus.

``COMMAND`` *command* ``;``
    Execute the named *command* in a subshell at Jamfile processing
    time and return the standard output as the first component of the
    return value. This can be useful for running configuration tools
    such as (for example) :program:`pkg-config` or
    :program:`pg_config` when building or linking against FFI
    libraries.

    If additional options are provided, they modify the return value
    as follows:

    ``exit-status``
        Return the exit status of *command* as the second component.
    ``no-output``
        Return an empty string in place of the contents of standard output.
    ``strip-eol``
        Strip trailing LF and CR characters from the end of the
        returned string.

``Split`` *strings* ``:`` *split_characters* ``;``
    Split each of the *strings* at the *split-characters* delimiters,
    with empty splits removed.

Built-In Jam Variables
======================

By default, the Jam build system is provided with some values. Some of these
are derived from the base Jam implementation and are documented in the
`Jam manual page`_ while others are Open Dylan extensions.

``.``
   The build directory.

   *Open Dylan extension.*

``COMPILER_BACKEND``
   The name of the compiler back-end currently in use. Typically one ``c``,
   ``harp`` or ``llvm``.

   *Open Dylan extension.*

``JAMDATE``
   The current date, in ISO-8601 format.

``NT``
   True on Windows.

``OS``
   The OS of the build host, not the target. This will typically be something
   like ``linux``, ``freebsd``, ``darwin`` or ``win32``.

``OSPLAT``
   The CPU architecture of the build host, not the target. This will
   typically be something like ``x86`` or ``x86_64``.

``PERSONAL_ROOT``
   The root of the destination build path, when the ``-personal-root`` compiler
   option or the :envvar:`OPEN_DYLAN_USER_ROOT` environment variable is set.

   *Open Dylan extension.*

``SYSTEM_BUILD_SCRIPTS``
   The path where the installed build scripts can be found.

   *Open Dylan extension.*

``SYSTEM_ROOT``
   The path where the installation of Open Dylan can be found.

   *Open Dylan extension.*

``TARGET_PLATFORM``
   The Open Dylan identifier for the target platform. This is something
   like ``x86-linux`` or ``x86_64-darwin``.

   *Open Dylan extension.*

``UNIX``
   True on non-Windows platforms, like Linux, FreeBSD and macOS.

Editing Jam Files
=================

There is an `Emacs major mode`_ for editing Jam files.

.. _Jam: https://swarm.workshop.perforce.com/view/guest/perforce_software/jam/src/Jam.html
.. _Perforce Software: https://swarm.workshop.perforce.com/view/guest/perforce_software/jam/src/Jam.html
.. _Jam manual page: https://swarm.workshop.perforce.com/view/guest/perforce_software/jam/src/Jam.html
.. _Jambase: https://swarm.workshop.perforce.com/files/guest/perforce_software/jam/src/Jambase
.. _Jambase reference: https://swarm.workshop.perforce.com/view/guest/perforce_software/jam/src/Jambase.html
.. _Emacs major mode: https://web.archive.org/web/20100211015821/http://www.tenfoot.org.uk/index.html
