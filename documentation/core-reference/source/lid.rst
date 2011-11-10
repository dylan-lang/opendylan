*******************
Library Interchange
*******************

Introduction
============

This chapter is about the library interchange definition format, *LID*.

The DRM defines an interchange format for Dylan source files (DRM, page
21), but does not define an interchange format for Dylan libraries.
Without such an agreed format, different Dylan development environments
would find it difficult to import and build libraries developed using
another Dylan vendor’s environment. It would also be impossible to
automate the process of importing a library into another environment.

LID solves this problem. It allows you to describe Dylan library sources
in a form that any Dylan environment should be able to understand.
Harlequin and other Dylan vendors have adopted LID to make it easier to
port applications from one environment to another.

.. note:: LID is a convention, and not an extension to the Dylan language.

.. note:: The Harlequin Dylan environment can convert LID files to its own
   internal project file format, the *.hdp* file. It can also save project
   files as LID files with the *File > Save As* command in the project
   window.

LID files
=========

LID works by supplementing each set of library sources with a LID file.
A *LID file* describes a Dylan library using a set of keyword
statements. Together, these statements provide enough information for
specifying and locating the information necessary to build a library
from its source. This means all Dylan libraries designed for interchange
consist of at least two files: a LID file, and one or more files
containing the library source code.

Thus a LID file performs a similar function to the *makefile* used in
some C and C++ development environments.

LID files have the file extension *.lid*.

Every file referred to by a LID file must reside in the same folder
(directory) as the LID file.

LID keyword statements
======================

A LID file consists of a series of keyword/value statements, just like
the Dylan source file interchange format described in DRM (page 21.) In
this section, we describe the standard LID keywords.

Library:
--------

LID file keyword

.. code-block:: dylan

    Library: *library-name*

Names the Dylan library being described. The *library-name* must be the
name of the Dylan library that the LID file describes. This keyword is
required in every LID file, and it may appear only once per LID file.

Files:
------

LID file keyword

.. code-block:: dylan

    Files: *file-designators*

Associates a set of files with the library named by the *Library:*
keyword. This keyword can appear one or more times per LID file. Every
file specified is considered to be associated with the library.

A file designator is something that can be mapped to a concrete file
name. Only one file designator can appear per line of the LID file. See
`File naming conventions`_ for a description of the format of file
designators and how they are mapped to concrete file names.

The order in which the designated source files are specified with the
*Files:* keyword in the LID file determines the initialization order
across the files within the defined library.

All the files specified must reside in the same folder (directory) as
the LID file.

Synopsis:
---------

LID file keyword

.. code-block:: dylan

    Synopsis: *arbitrary text*

A concise description of the library.

Keywords:
---------

LID file keyword

.. code-block:: dylan

    Keywords: *comma-separated phrases*

Any number of phrases, separated by commas, that are relevant to the
description or use of the library.

Author:
-------

LID file keyword

.. code-block:: dylan

    Author: *arbitrary text*

The name of the library’s author.

Version:
--------

LID file keyword

.. code-block:: dylan

    Version: *arbitrary text*

The current version number of the library.

Description:
------------

LID file keyword

.. code-block:: dylan

    Description: *arbitrary text*

A description of the library. The intention of this keyword is to
provide a fuller, less concise description than that given by the
*Synopsis:* keyword.

Comment:
--------

LID file keyword

.. code-block:: dylan

    Comment: *arbitrary text*

Any additional comments about the library.

Common Dylan’s LID extensions
=============================

This section contains extensions to LID that Common Dylan supports.

Specifying foreign files and resource files
-------------------------------------------

The following keywords allow you to specify that files of foreign source
code and resource files are a part of the library.

C-Source-Files:
^^^^^^^^^^^^^^^

LID file keyword

.. code-block:: dylan

    C-Source-Files: *c-source-files*

Identifies one or more C source files which are to be included as part
of the library. Dylan environments copy these files to their build area
and ensure that they are compiled by the appropriate batch file. The
filenames specified must include the *.c* suffix.

C-Header-Files:
^^^^^^^^^^^^^^^

LID file keyword

.. code-block:: dylan

    C-Header-Files: *c-header-files*

Identifies one or more C header files included as part of the library.
Dylan environments copy these files to their build area and ensure that
they are compiled by the appropriate batch file. Any files specified
using the *C-Source-Files:* or *RC-Files:* keywords depend on these
header files in order to decide when they need to be recompiled. The
file names given here must include the *.h* suffix.

C-Object-Files:
^^^^^^^^^^^^^^^

LID file keyword

.. code-block:: dylan

    C-Object-Files: *c-object-files*

Identifies one or more C object files included as part of the library.
Dylan environments copy these files to their build area and ensure that
they are compiled by the appropriate batch file and included in the
final output as *.DLL* or *.EXE* files. The file names given here must
include the *.obj* suffix.

RC-Files:
^^^^^^^^^

LID file keyword

.. code-block:: dylan

    RC-Files: *resource-files*

Identifies one or more resource files to be included as part of the
library. Dylan environments copy these files to their build area and
ensure that they are compiled by the appropriate batch file. The
resulting resource object files are included in the *.DLL* or *.EXE*
built for the library. The file names given here must include the *.rc*
suffix.

C-Libraries:
^^^^^^^^^^^^

LID file keyword

.. code-block:: dylan

    C-Libraries: *c-lib-files*

Identifies one or more C libraries to be included in the link phase when
building the *.DLL* or *.EXE* for the library. You can use this keyword
to specify arbitrary linker options as well as libraries.

Unlike the other keywords described in this section, the *C-Libraries:*
keyword propagates to dependent libraries. For example, suppose library
A uses library B, and the LID file or library B specifies

.. code-block:: dylan

    C-Libraries: foo.lib

In this case, both library A and library B are linked against *foo.lib*.

Specifying compilation details
------------------------------

The following keywords control aspects of compilation for the library.

Executable:
^^^^^^^^^^^

LID keyword

.. code-block:: dylan

    Executable: *name*

Specifies the name of the executable (that is, *.DLL* or *.EXE*) file
to be generated for this library.

The suffix (*.DLL*, *.EXE*) should not be included in the *name* as
the appropriate suffix will be added automatically.

If this keyword is not specified, the compiler generates a default name
for the executable from the name of the library. With some library
names, particularly when you are building a DLL, you may need to specify
this keyword to override the default name and avoid conflicts with other
DLLs from a third party.

Base-Address:
^^^^^^^^^^^^^

LID keyword

.. code-block:: dylan

    Base-Address: *address*

Specifies the base address of the DLL built from this Dylan library. The
*address* must be a hexadecimal value. For convenience, you can use
either Dylan (*#xNNNNNNNN*) or C (*0xNNNNNNNN*) notations when
specifying the address.

This base address is ignored when building a *.EXE* file.

If this keyword is not specified, the compiler will compute a default
base address for the library. However, it is possible for more than one
library to end up with the same default base address. If an application
uses any of these libraries, all but one of them will have to be
relocated when the application starts. This process is automatic, but
cuts down on the amount of sharing, increases your application’s memory
footprint, and slows down load time. In such circumstances, you may want
to give one or more libraries an explicit base address using this
keyword.

Linker-Options:
^^^^^^^^^^^^^^^

LID keyword

.. code-block:: dylan

    Linker-Options: *options*

Specifies additional options and libraries to be passed to the linker
when building this DLL or EXE. Unlike the C-Libraries: keyword, the
options and libraries specified here apply only to this Dylan library;
they are not propagated to any libraries which use this library.

File naming conventions
=======================

In practice, importing a source distribution into a Dylan program
involves unpacking the source distribution into its own subtree and then
informing the environment of the location of the tree root. The
environment then walks the entire subtree locating LID files, which
describe the libraries in the distribution by giving a name to, and
designating the source files associated with, each library.

Importing a Dylan program into the environment in this way requires two
things:

#. That the LID files in the distribution can be identified.
#. That the file designators supplied to the *Files:* keyword in LID files
   can be mapped to the corresponding source filenames on disk.

If you are importing files from a platform that does not insist on, or
conventionally use, standard filename suffixes to identify the filetype
(such as MacOS), then you must rename your source files as follows:

- LID files must be given filenames with the suffix *.lid*.
- Dylan source files must be given filenames with the suffix *.dylan* or
  *.dyl*.

The file designators that appear in LID files may be a string of
characters of any length, constructed from the set of hyphen,
underscore, and the mixed-case alphanumeric characters. Note that you do
not have to specify the source filename suffix as part of the filename
designator. This ensures that the LID files themselves do not need to be
edited when importing source code from a platform, such as MacOS, that
does not insist on particular filename suffixes to specify the file
type.

The name of a LID file is not significant, and in particular need not be
the same as the library name. Hierarchical directory structure can be
used to organize multi-library systems as long as the files directly
associated with each library are in a single directory.

Application example
===================

This section contains an example of a complete Dylan application that
uses a generic factorial calculation routine to return the value of the
factorial of 100. Two libraries are defined: the *factorial* library
provides an implementation of the generic factorial routine, and the
*factorial-application* library provides a method that calls the generic
routine and returns the appropriate result.

File: *fact.lid*. LID file describing the components of the *factorial*
library.

.. code-block:: dylan

    Library: factorial
    Synopsis: Provides a naive implementation of the factorial
              function
    Keywords: factorial, integer, simple, recursive
    Files: fact-def
           fact

File: *fact-def.dyl*. Defines the *factorial* library and its one
module.

.. code-block:: dylan

    Module: dylan-user

    define library factorial
      use dylan;
      export factorial;
    end;

    define module factorial
      export fact;
    end;

File: *fact.dyl*. Defines the method for calculating a factorial.

.. code-block:: dylan

    Module: factorial

    define generic fact(n);

    define method fact(n == 0)
      1;
    end;

    define method fact(n)
      n * fact(n - 1);
    end;

File: *app.lid*. LID file describing the components of the
*factorial-application* library.

.. code-block:: dylan

    Library: factorial-application
    Synopsis: Computes factorial 100
    Files: appdef
           app
    Start-Module: factorial-application
    Start-Function: main

File: *appdef.dyl*. Defines the *factorial-application* library and its
one module.

.. code-block:: dylan

    Module: dylan-user

    define library factorial-application
      use dylan;
      use factorial;
    end library;

    define module factorial-application
      use dylan;
      use factorial;
    end module;

File: *app.dyl*. Defines a routine that calls the factorial routine.

.. code-block:: dylan

    Module: factorial-application

    define method main (#rest ignore)
      fact(100);
    end method;

The following example demonstrates how files of foreign source code and
resource files can be integrated into a Dylan library:

.. code-block:: dylan

    Library: app-with-foreign-code
    Synopsis: Uses some C code and resources
    Files: dylan-code
    C-Source-Files: first.c
      second.c
    C-Header-Files: headers.h
    RC-Files: extra-resources.rc
