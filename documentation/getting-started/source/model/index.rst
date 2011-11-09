*************************
Programming in Open Dylan
*************************

Now we have taken a tour of Open Dylan using the pre-written
Reversi application, we take a step back to look at the programming
model in Open Dylan, and to review the features of the development
environment and the Dylan compiler.

.. index:: projects

Projects
========

In Open Dylan, all development work is done in terms of *projects*.
Projects are the development environment and compiler’s way of
representing Dylan libraries.

A project consists mainly of a list of the source files that define the
library, but also contains information about how to compile the library.

While it is possible to edit text files that are not associated with any
project, nearly all other programming tasks in Open Dylan take
place within the context of a project.

Projects and libraries
----------------------

A project represents a single Dylan library. Think of a project as
something that gathers together all the information Open Dylan
needs to be able to compile a particular library.

For example, earlier in this manual, we worked with the Reversi project.
The Reversi project represents a single Dylan library, called Reversi.

Projects and deliverables
-------------------------

You can create deliverable applications, libraries, and components from
projects. Projects can be built into executable (.EXE) or dynamic-link
library (.DLL) files.

Use the *Project* menu for project building. See
` <../delivery.htm#40190>`_, for details of delivery to customers.

When we worked with the Reversi project, we built an executable from it,
but we could just as easily have built a DLL. See
` <../projects.htm#32945>`_ for details.

.. index::
   single: projects; creating new

Creating new projects
---------------------

Open Dylan includes a New Project wizard for creating new projects.

The New Project wizard asks questions about what you are going to
develop, and then creates a new project configured to help you get
working quickly. See ` <../projects.htm#11353>`_.

You can also create a project if you have a Dylan Library Interchange
Description (LID) file for it. LID files have a .LID extension. When you
open a LID file in the development environment, it is converted into a
project file and opened in a project window. (This process does not
modify the original LID file on disk.) See ` <../projects.htm#33260>`_.

.. index:: .HDP files
   single: file extensions; .HDP
   single: projects; profile files

Project files
-------------

Open Dylan stores projects on disk in *project files*. Project
files have the same name as the project, with the extension *.* HDP.
Thus a project called Hello is stored in the file *Hello.hdp*.
(Projects get their names in the New Project wizard or, if they were
created by conversion from a LID file, get their name from the name of
the library that the LID file defines.)

Project files are the files you select when you want to open a project
or add it to another project as a subproject.

The development environment tools automatically save changes to project
files. For example, if you add a new source file to a project, the
change is saved to disk immediately.

Project components
------------------

We now describe the components of a project in more detail. Every
project consists of:

A Dylan library
    Every project defines a single Dylan library. We call this library the
    “library of the project” or, for clarity, the *main* library of the
    project, to distinguish it from other libraries that the project uses.

A project name
    Every project has a name. When you create a new project with the New
    Project wizard, the wizard uses this name to generate default names for
    initial files, libraries and modules in the new project. The compiler
    uses the project name to generate default names for the executables and
    other files it produces during compilation. In both cases, you can
    override the defaults.

Source code files, and other files
    Every project includes source code files. Projects created with the New
    Project wizard will have a *library.dylan* file (which defines the
    project’s library); a *module.dylan* file (which defines the modules of
    the library); and at least one other Dylan source code file containing
    definitions and expressions.

    Projects can also include Windows resource files, static libraries (.LIB
    files), and text files. The compiler ignores any file it does not
    recognize. This flexibility allows such things as including README files
    in a project.

Subprojects
    A project can have *subprojects*. Subprojects are other projects that
    are included in a project; they define their own main library, contain
    their own source files and may have subprojects themselves. For clarity,
    we can call a project a *superproject* when describing it with reference
    to its subprojects.

    See `The build cycle`_ for more on the relationship between projects
    and their subprojects.

Version numbers
    Every project has a major and minor version number. The version numbers
    affect the build process for projects. See ` <../projects.htm#16457>`_
    and `The build cycle`_.

Project settings
    Every project has settings. Among these settings are:

    - The list of source code files and their locations on disk.
    - Compilation mode options. See `Compilation modes`_.
    - Debugging options. See ` <../debug.htm#17520>`_.
    - The list of subprojects the project uses.
    - The locations on disk of the subprojects.

.. _projects-on-disk:

Projects on disk
----------------

A project consists of several files and folders on disk.

First, all the information necessary to build the project is stored in a
*project file* (.HDP file). Then there are the Dylan source files, and
possibly Windows resource and static library (.LIB) files, that make up
the code for the project.

The files that make up a project are stored in a folder called the
*project folder*, which normally has the same name as the project. The
files are stored in the project folder and in several subfolders of the
project folder. The files themselves can refer to other folders where
subprojects and used libraries are stored.

The project folder contains the following files and subfolders:

- The project file. (.HDP file.)
- The source code files. (.DYLAN files)
- The *bin* folder.

  This folder holds the executable (.EXE) or DLL (.DLL) file produced from
  the project.

  In addition, the DLLs of the project’s subprojects are automatically
  copied into this folder, so that they can be found when you execute your
  project’s application.
- The *project* *-build* folder.

  This folder, whose name begins with the name of the project, holds a
  number of intermediate files produced during builds. You will never have
  to do anything with these intermediate files.

  The folder also contains the *compiler database* file for the project.
  This file has the same name as the project and the extension .DDB. See
  `Compiler databases`_ for more details.

  You can remove the compiler database and intermediate files with
  **Project > Remove Build Products**. This forces a complete recompilation
  of a project next time you build it.
- The *lib* folder.

  This folder holds the *linker* file for the project. This file has the
  same name as the project and the extension .LIB or .DEFS. This file is
  needed for other projects to be able to link against the project, a
  process that is part of using a project as a subproject.

  The extension is .LIB if you are using the Microsoft linker, or .DEFS if
  you are using the GNU linker.
- The *release* folder.

  This folder holds a stand-alone version of the project’s application,
  suitable for redistribution to customers or other third parties without
  a copy of Open Dylan on their system. It is created when you choose
  the **Project > Make Release** command.

Projects in the development environment
---------------------------------------

The Open Dylan development environment offers a variety of ways to
examine and manipulate projects. You can view a single project in
multiple windows at the same time. You can also have more than one
project open in the environment at a time.

Apart from the main window and dialog boxes, windows in Open Dylan
are generally instances of programming tools. The tools provide views
onto different pieces of a project, or sometimes different views of the
same pieces.

For example, you might want to have editor windows open on multiple
files in the project, as well as browser windows to show you structural
views and debugger windows to show you stack backtraces or other
information from a running program.

As we saw when touring the environment with the Reversi example,
Open Dylan offers:

-  A project window.
-  A debugger for examining and interacting with paused application
   threads associated with open Dylan projects.
-  A browser for examining the contents and properties of projects and
   of the objects in paused application threads associated with open
   Dylan projects.
-  An editor for source files. Editors are most often invoked from other
   windows on a project, but can be invoked on files outside the context
   of a project.

Development models
==================

The process of development in Open Dylan can be much the same as in
interactive development environments for other languages. Applications
written in Dylan can be developed in the same way as applications
written in static languages like C and C++, for instance.

.. figure:: model-2.gif
   :align: center

   “Static” development model.

You can also develop applications in a more dynamic fashion, using
features in the debugger and browser tools that allow you to interact
with a running application. With these dynamic, interactive features,
you can test bug fixes on the fly and keep your application running
before committing to a rebuild.

.. figure:: model-4.gif
   :align: center

   “Dynamic” development model.

Interactive and incremental development
---------------------------------------

Open Dylan offers both interactive and incremental development
features. It is important to distinguish them clearly:

Incremental development is the ability to recompile portions of a
project and save the resulting object code. By contrast, some
compilation systems require that the entire project be recompiled in
response to any change, however small. Open Dylan always performs
incremental compilations when it can, to keep build times as short as
possible.

Interactive development is the ability to execute code fragments,
including definitions and redefinitions, in a running program. Open
Dylan offers interactive development via the debugger’s interaction
pane. The object code produced during interactive development is not
saved, but just patched into the running program and added to the
in-memory *compiler database* (see `Compilation modes`_). The
object code is lost when the program terminates.

Compilation
===========

This section discusses compilation modes, compiler databases,
optimization (including loose and tight binding), the build cycle
algorithm, and linkers.

.. index:: .DDB files, Compiler databases
   single: file extensions; .DDB
.. _compiler-databases:

Compiler databases
------------------

When compiling a project, Open Dylan produces a compiler database
which models the project. The database provides a rich source of
information to Open Dylan tools about the contents, properties, and
relationships between source code definitions, libraries, and modules.

A project’s compiler database is used when browsing and debugging the
project, and is also used when compiling other projects that use the
project.

The compiler database for a project does not exist until the project has
been built for the first time. Before then, if you try to do anything
that requires the database, the development environment will ask you if
you want to create it.

Once the compiler database has been built, the development environment
will ensure it is kept up to date with each recompilation of the
project.

Open Dylan stores project files on disk for persistence between
sessions. When you close a project, the development environment checks
whether the database has changed since it was last saved, and if it has
it asks you if you want to save the database. (You can use **File > Save
Compiler Database** from the project window to save the compiler database
at other times, if necessary.) When you re-open the project later, the
database is read into memory from the disk file, if it exists.

Compiler database files have a .DDB suffix.

.. index:: compilation modes

Compilation modes
-----------------

The Dylan language encourages programmers to write programs that can be
compiled as efficiently as programs written in static languages. By
adding type declarations and sealing to your project code, the Open
Dylan compiler can optimize it very successfully.

However, the best optimizations come at the costs of longer build times,
and less symbolic information in the debugger. During the larger
proportion of your project’s development, you want projects to build
quickly and to be easier to debug. When it is time to deliver your
product, you will want to turn all the code optimizations on even at the
expense of debugging information and compilation speed.

Like other compilation systems, Open Dylan allows you to switch
between both styles of compilation. For any project, you can specify the
style of compilation to perform by choosing **Project > Settings** in any
window with a **Project** menu, and then choosing the Compile property
page.

That page offers two mode choices:

-  Interactive Development mode
-  Production mode

.. index:: interactive development mode
   single: compilation modes; interactive development mode

You should do the majority of your work on a project in Interactive
Development mode. When compiling a project in this mode, the compiler
does not perform as many optimizations as it can, and is not as strict
about error checking as it can be. The idea here is to keep compilation
times as short as possible.

This mode keeps symbolic information in the compiled code that will make
debugging work easier. Also, if your project was compiled in this mode
you will be able to do more interactive work in the debugger’s
interaction pane, including redefinition. However, compiled code will
not be as fast as it can be.

.. index:: production mode
   single: compilation modes; production mode

When your project work is nearing completion, and you want to see the
compiled version running as fast as possible, switch to compiling the
project in Production mode. Production mode turns on all compiler
optimizations. However, build times will be slower than in Interactive
Development mode, and debugging and interaction will be more limited.

When you have switched to Production mode, you can use Open Dylan’s
*optimization coloring* feature to highlight inefficiencies in your
code. This feature colors source code so that you can see where
optimizations did and did not occur. Adding type declarations and
sealing will secure new optimizations, which you can verify by
refreshing the coloring after rebuilding the project. See
` <../coloring.htm#27192>`_.

Versioning
----------

A project can have major and minor version numbers that will be recorded
in the DLL or EXE that the project builds. You can enter these numbers
on the **Project > Settings…** dialog’s Link page.

Open Dylan uses version numbers at compile time and run time to
determine if compatible versions of Dylan libraries are in use.

The rules differ for compilation in Interactive Development mode and
Production mode. For applications compiled in Interactive Development
mode, the procedure at run time for initializing a library involves
checking the major and minor versions of the Dylan libraries used by the
library being initialized. If the major version number of a used library
does not match that of the library using it, or the minor version number
of a used library is lower than that of the library using it, the
Open Dylan run-time system signals an error.

In Production mode, the run-time check ignores the user-supplied version
numbers and checks whether the used library is the very same one that
was used at compile time. If the library is different, a run-time error
is signalled even if the version is the same.

.. index:: loose binding, tight binding
   single: binding; loose and tight
   single: compilation modes; relationship to loose and tight binding

Binding
-------

Interactive Development mode and Production mode are in fact
combinations of some lower-level compiler modes. Open Dylan
presents these two compilation modes to make development simpler, but
some understanding of these lower-level modes is useful. They are *loose
binding* and *tight binding*.

-  Loose binding This is a way of compiling code that makes no use of
   the type information available in the source. When the compiler is
   run using loose binding, it considers only names and macro
   definitions. References to objects and types are always made
   indirectly through the objects’ names, so that the objects can be
   changed without forcing recompilation of code that uses them.
-  Tight binding This is a way of compiling code that uses all type
   information available in order to drive optimizations. This type
   information includes declared types and some inferred types. Tight
   binding bypasses names, referencing objects and types directly.
   Amongst other optimizations, tight bindings inlines some methods,
   performs tail-call elimination, and removes unused code. These
   optimizations can affect the information seen in the debugger.

Code can be loosely or tightly bound within a library, and it can be
loosely or tightly bound with respect to other libraries. If code within
a library is loosely bound, other libraries will be loosely bound to it.
Similarly, if code is tightly bound within a library, other libraries
will bind tightly to it.

The code within all libraries that Open Dylan supplies—the system
libraries—is tightly bound. This means that all libraries you develop
will bind tightly to whichever of the system libraries you use.

When libraries are compiled in Interactive Development mode, they are
loosely bound internally, and therefore libraries that use them will be
loosely bound to them. When libraries are compiled in Production mode,
they are tightly bound internally, and therefore libraries that use them
are tightly bound to them.

.. index:: build cycle
   single: applications; build cycle

The build cycle
---------------

Building an application or DLL from a project consists of up to three
phases:

#. Building the subprojects.
#. Compiling some or all of the project source code.
#. Linking the project.

For efficiency, when the compiler is asked to build a project it
minimizes the number of these phases that it performs, using the
following decision rules:

-  If phase 2 or 3 is performed, the project is considered changed.
-  A *clean build* always performs all phases for the project and its
   subprojects.

You can ask for a clean build by choosing **Project > Clean Build** in any
window that has a **Project** menu.

-  A build command is always recursively performed on subprojects (phase
   1).
-  If the major version number of any subproject has been changed, then
   all of the source code in the project is recompiled.
-  If the project is tightly bound to any subproject which has changed,
   then all the source code in the project is recompiled.
-  If the project is tightly bound to itself, and if any source code in
   the project has changed, then all the source code in the project is
   recompiled.
-  If the project is loosely bound to itself, then any source code files
   that have changed are recompiled. Additionally, files that depend on
   those changes (such as through macro usage) are recompiled.
-  If the project or any of its subprojects has changed, then the
   project is relinked.

.. note:: To ensure change propagation according to these rules, you
   should always increment the major version number of a project after
   altering any macro definitions in it.

Linkers
-------

Open Dylan offers you a choice of linkers to use to link your Dylan
programs. The default linker is a GNU linker. If you own Microsoft
Developer Studio, you can use Microsoft’s linker instead. See the Linker
page of the main window’s **Options > Environment Options…** dialog.

Executing programs
==================

This section discusses running applications within Open Dylan (and
the benefits of doing so), and the process of library initialization in
an application.

.. index::
   single: applications; running

Starting applications up from within Open Dylan
-----------------------------------------------

An application written in Dylan cannot be started and later connected to
Open Dylan and its project. If you want to be able to debug an
application and browse its compiler database within Open Dylan, you
must start it up by opening its project and starting it with
**Application > Start**. This starts the application up under the
debugger, providing the development environment with a connection to the
application and the capabilities necessary to control its execution and
to interact with it.

.. index::
   single: applications; initialization

Application and library initialization
--------------------------------------

When a Dylan application starts up, it begins by loading the libraries
that it uses. Each library performs its own initialization when it is
loaded. In general, libraries are loaded in a demand-driven, depth-first
order. However, you should not depend on used libraries being loaded in
the same order that they are mentioned in a library definition.

Library initialization is performed by executing the code which
comprises the library, in the order in which it is defined by the
library’s project. This means that the order of the Dylan source files
in a project is significant, and that the order of definitions and
expressions in a Dylan file is significant.

Definitions in a Dylan library are not, in general, said to execute.
Rather, they define the static structure of a program. This is true of
variables and constants initialized to literal values or other values
computable at compile time, and it is also true of classes and
functions. Forward references to such objects are allowed, and all such
objects are created at the start of library initialization, before
expressions are executed. Some definitions rely on the computation of
run-time values; in these cases, forward references may not be allowed.

Expressions in a Dylan library are executed in the order in which they
appear in the project, and the last expression in a project should be a
call to a project’s start function.

.. index::
   single: Compiler databases; relationship to source and run-time views

Source, database, and run-time views
====================================

We have seen that Open Dylan provides several tools to allow us to
view projects in different ways. Some tools can look at the source
representation of a project, while others can look at the run-time
representation—the threads of a running application built from a
project.

It is useful to think of there being three “worlds” in which we can
simultaneously view projects: source, database, and run-time.

Every project has a representation in source code. We view this source
representation with the editor mainly, but the debugger’s source pane
can show us the source code for a function on the stack, and the browser
can show the source for some kinds of object in its Source page.

When we build a project, the compiler database that is created provides
a second representation. Then, when we run the application or DLL we
have created, the running program is itself a third representation of
the project.

So, at any given time, an object may exist in each of these worlds
simultaneously. The source code of the object may exist in a Dylan
source file, a model of the object may exist in the compiler database,
and the object may be instantiated in a running program.

Editor windows show projects in their source representation only.
Browser windows show information from the compiler database, and, if a
program is running, this database information is combined with
information from the program, so you can see the “live” version of the
object.

The debugger and its interaction pane allows you to view the threads of
running programs, and allows you to execute expressions and definitions
in these threads. When you do this, the running program is modified.
When you enter definitions in this way, the definitions are saved in a
temporary layer of the compiler database so that browsing will continue
to be accurate. However, these temporary changes are not saved to disk
in the compiler database file, nor are they reflected in the project
source code files.

There are ways in which the three worlds can get out of sync. Remember
that if you edit a source code definition, the model of it in the
database will not be updated until you rebuild the project. So, for
instance, if you change the inheritance characteristics of a class, the
change will not be reflected in the browser Superclasses page for that
definition until you rebuild. And if you add new definitions to the
project sources, they will also not be visible until you build the
project again.
