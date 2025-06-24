*********************
Single File Libraries
*********************

==============  =============================================
DEP #:          6
Type:           Standards Track
Author:         Carl Gay
Status:         Draft
Created:        12-Feb-2013 (Darwin's birthday)
Last-Modified:  16-Jan-2025
Post-History:   04-Dec-2021 `Matrix channel <https://app.element.io/#/room/#dylan-lang_general:gitter.im>`_
Dylan-Version:  2025.1
==============  =============================================


Revision History
================

The revision history of this document is available on GitHub:
https://github.com/dylan-lang/opendylan/commits/master/documentation/source/proposals/dep-0006-single-file-library.rst

Abstract
========

Open Dylan currently requires users to create three source files to
define a simple library like hello-world:

#. A project file (i.e., .lid file) to describe source files to
   include, link options, etc.

#. A library and module definition source file, often named library.dylan.

#. At least one source file for main program logic, in a module
   defined by the library definition.

This DEP proposes a standard way to define a Dylan library in a single
source file.  This DEP targets implementations of Dylan but does not
propose a change to the Dylan language itself.  In particular it is
proposed as an enhancement for Open Dylan.

Rationale
=========

The three files described above are a lot of overhead for new users
who want to get a feel for the language, who are generally used to
creating a single file and running it directly, perhaps after
compiling it.  A simpler setup would provide a better initial
experience for new users, and would help to make Dylan more attractive
for "scripting" purposes.  The mantra has been that Dylan was designed
for large projects, but there's no reason it can't also excel at
scripting, with this change and the right set of support libraries.

Note that even though the ``deft new library`` command will generate a
skeleton project with three files (and a registry), this does not
remove the need to jump back and forth between the library definition
file and the main source file as you figure out which modules you need
to use.  There is also the issue of new users figuring out that
``deft new library`` exists in the first place, and how LID files work.

There are plenty of libraries that are small enough to comfortably fit
in a single file.  Of the existing libraries in `the dylan-lang github
repository <https://github.com/dylan-lang>`_, at least ten could
comfortably fit in a single file.

Having a library in a single file simplifies sharing of examples and
small tests.  Indeed, one of the motivations for this feature was to
be able to contribute Dylan solutions for the `The Computer Language
Benchmarks Game <https://benchmarksgame-team.pages.debian.net/benchmarksgame/>`_, which
prefers to receive one file per solution.  Obviously it is possible to
explain to them about LID files and how to unpack and compile a
multi-file solution for each problem, but it is more complex than it
needs to be.

This DEP is only the first step to adding better support for
scripting.  More is needed, such as supporting "hash bang" lines
(``#!/usr/bin/dylan``) and only recompiling when the source has
changed.

Gwydion Dylan already supports a simple form of single-file library,
but it has limitations that this DEP removes, specifically the
inability to deal with import conflicts.


Goals
=====

* Make it possible to use the full power of the Dylan module system, or close to it, when
  defining a library in a single file.

* Make all LID file options available in the single-file library
  format.

* Introduce no new syntax.  It should be obvious to someone
  already used to Dylan what's going on.


Background
==========

A typical hello-world application written in Dylan might look like
this:

**hello-world.lid**

.. code-block:: dylan

    Library: hello-world
    Files: library.dylan
           hello-world.dylan

**library.dylan**

.. code-block:: dylan

    Module: dylan-user

    define libary hello-world
      use common-dylan;
      use io;
    end;

    define module hello-world
      use common-dylan;
      use format-out;
    end;

**hello-world.dylan**

.. code-block:: dylan

    Module: hello-world

    format-out("Hello, world!\n")

A way to encode the same information into a regular dylan
source file is needed.  After the implementation of this DEP, the
above library can be defined in a single source file, as follows:

**hello-world.dylan**

.. code-block:: dylan

    Module: hello-world

    define libary hello-world
      use common-dylan;
      use io;
    end;

    define module hello-world
      use common-dylan;
      use format-out;
    end;

    format-out("Hello, world!\n")

This continues to use the standard `Dylan Interchange Format
<https://opendylan.org/books/drm/Dylan_Interchange_Format>`_ as defined
in the DRM, with a set of headers, followed by a blank line, followed
by a *code body*.


Specification
=============

Replacing the LID File
----------------------

LID files have the same format as the header section of a Dylan
Interchange Format source file.  When defining a Dylan project in a
single source file, all LID keywords may appear in the header section.
The compiler or interpreter should handle them in the same way it
would if they were in a separate .lid file.  There is no conflict
between the keywords used in LID files and those used in Dylan source
files.  See :doc:`/library-reference/lid`
for existing Open Dylan LID file keywords.

Module Header
~~~~~~~~~~~~~

The ``Module`` header is required in a single-source library.

Library Header
~~~~~~~~~~~~~~

The ``Library`` header is optional. If missing, the library name is the same as
the module name specified by the ``Module`` header.

.. note::

   This decision could have gone the other way, with ``Library`` required and ``Module``
   optional. However, it is normal for each .dylan file to have a ``Module`` header
   already, so this is consistent with current practice.

Files Header
~~~~~~~~~~~~

The ``Files`` header should not appear in a single-file library.  If
present, the behavior is undefined.

.. note::

   An implementation could choose to point to a Dylan source file instead of a LID file
   in its build system. For example, in Open Dylan a "registry" file could point to a
   ``.dylan`` file instead of a .lid file and there's no reason to prevent the ``.dylan``
   file from including other Dylan source files. However, that is not part of this
   proposal.

Replacing library.dylan
-----------------------

A single-file Dylan library is divided into three logical sections:

1.  Dylan Interchange Format headers, including all headers that are allowed in
    Dylan source files or in LID files.

2.  Library and module definitions (see below). This section is implicitly in the
    ``dylan-user`` module. This section ends when the module whose name is specified in
    the ``Module`` header has been defined.

3.  The main Dylan code.

In the library and module definitions section of the source file normal Dylan
code comments are allowed and there must be:

* Any code needed by the library and module definitions.  For example, it is possible to
  define a macro to help define multiple modules. Note, however, that any code in this
  section is in the ``dylan-user`` module, which is defined by the Dylan implementation,
  so there is no way to export the code for use in other modules.  More specifically,
  there is no way to use the code in the main body of the same source file.

* *Exactly one* ``define library`` expression and its name must match the name
  specified in the ``Library`` header, or the ``Module`` header if there is no
  ``Library`` header.

* *At least* one ``define module`` expression whose name matches the name in the
  ``Module`` header.  This module definition terminates the library and module
  definition section of the source file.

This construction allows the full power of Dylan's module system to be used,
for example defining both implementation and interface modules, exporting the
implementation module for use by a (possibly also single-file) test library,
etc.

Open Dylan Support
------------------

Some changes will need to be made to Open Dylan to support single-file libraries.
A non-exhaustive list:

* The ``dylan-compiler`` and ``dylan-environment`` programs need to accept files with a
  ``.dylan`` extension and attempt to open them as projects, giving an appropriate error
  if the file is badly formed.

* Registry files must be able to point to ``.dylan`` files in addition to ``.lid`` files.

* The IDE will need to handle single-file libraries specially:

  - It should be possible to open a new project by selecting a ``.dylan`` file, with an
    appropriate error if the file doesn't contain library and module definitions.

  - The library and module definitions should be treated as if in the ``dylan-user``
    module.

Model Implementation
====================

A simple way to implement this proposal would be via the following source
transformations. (Such a simplistic implementation is not recommended because
it would cause problems mapping error messages back to the original source
files, among other issues.  This example is provided primarily to demonstrate
that a single-file library has the same semantics as a multi-file library.)

1.  Generate a ``library.dylan`` file containing the entire library and module
    definitions section, verbatim, but in the ``dylan-user`` module. Using the
    hello-world example from above, we generate::

      ------- file: library.dylan -------
      Module: dylan-user

      define libary hello-world
        use common-dylan;
        use io;
      end;

      define module hello-world
        use common-dylan;
        use format-out;
      end;

2.  Generate a "main.dylan" file containing the entire headers section and the
    entire main Dylan code section::

      ------- file: main.dylan -------
      Module: hello-world

      format-out("Hello, world!\n")

#. Generate a LID file that includes the entire headers section. Add a
   ``Library`` header matching the ``Module`` header if none was provided, and
   add "library.dylan" and "main.dylan" to the ``Files`` header::

     ------- file: hello-world.lid -------
     Library: hello-world
     Module: hello-world
     Files: library.dylan
            main.dylan

#. Compile the generated LID file in the normal way.

Alternatives Considered
=======================

The initial version of this DEP considered adding special Dylan Interchange
Format headers (``Use-library``, ``Use-module``, etc.) in which the library and
module imports would be specified. That version was rejected because it did not
allow the full power of Dylan's module system to be used. See older revisions
of this file for more detail.
