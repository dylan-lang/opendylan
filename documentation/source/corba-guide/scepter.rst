****************************
Using the Dylan IDL Compiler
****************************

Introduction
============

The Open Dylan IDL compiler, Scepter, is available as a standalone
executable called :file:`console-scepter.exe`. The executable is
available in the :file:`bin` subfolder of your top-level Open Dylan
installation folder.

The IDL compiler reads an IDL file and generates (up to) three Dylan
projects from it. The projects contain stub and skeleton code. Each
project defines one of the libraries specified in the :doc:`idl-app`.

The project’s name is identical to the name of the library which it
defines. By default the projects are placed in subfolders of the
current working folder called :file:`protocol`, :file:`stubs`, and
:file:`skeletons`.

General usage
=============

The IDL compiler (:program:`console-scepter.exe`) is invoked from a
console prompt. The general form of the command is:

**console-scepter** [*options*] <*file*>

which compiles the IDL file *file*. Only one filename can be
supplied. Options can be prefixed with the character ``/`` or
``-``. Options that take a value must be separated from the value by a
``:`` character or whitespace.

The following invocation compiles the file :file:`bank.idl`::

   console-scepter bank.idl

Code generation options
=======================

``/language:`` *name*
   The ``/language`` option is deprecated, but is still supported for
   backward compatibility. It has been replaced with the ``/back-end``
   option.

``/back-end:`` *name*
   The ``/back-end`` option selects which back-end the compiler should
   use on the IDL file. Two values are supported:

   ``dylan``
      This is the default value. The Dylan back-end generates Dylan
      protocol, stubs and skeletons libraries.

   ``ir``
      This value causes the compiler to load the definitions in the
      IDL file into an Interface Repository. It will overwrite any
      definitions in the repository with the same scoped names. The
      compiler uses a call to the standard CORBA ORB operation
      ``ResolveInitialReferences`` to obtain a reference to the
      Interface Repository. Note that Open Dylan does not have its own
      Interface Repository, but you can use one supplied by a third
      party. You may use the ``-ORBinterface-repository-file`` and
      ``-ORBinterface-repository`` options (described in Section
      8.3.4, “ORB runtime switches”) to specify an object reference
      for ``ResolveInitialReferences`` to return.

   More than one ``/back-end`` option may be supplied if you want to
   load the IDL into an Interface Repository and generate Dylan code.

``/parse``
   Parse the IDL file only. This option overrides ``/back-end``.

The following three options only apply to the Dylan back-end.

``/directory:`` *dir*
   By default :program:`console-scepter.exe` puts the Dylan projects
   it generates into subfolders in the current working folder. You can
   force it to put the subfolders in another folder dir by using the
   ``/directory`` option.

``/prefix:`` *name*
   The default names for the subfolders are :file:`protocol`,
   :file:`stubs`, and and :file:`skeletons`. You may specify a prefix
   for these names with the ``/prefix`` option. This might be useful,
   for example, where you want to put the project subfolders from more
   than one IDL file into a common folder.

``/stubs``
   By default :program:`console-scepter.exe` generates three libraries
   for each IDL file: a protocol library, a stubs library, and a
   skeletons library. However, you may not always want to generate
   both the stubs and skeletons libraries. For example if you are
   developing a client application you will only need the stubs
   library. The ``/stubs`` option causes
   :program:`console-scepter.exe` to generate only the protocol and
   stubs libraries.

Preprocessor options
======================

``/preprocess``
   Runs the preprocessor on the IDL file and displays the result on
   standard output. The preprocessor output is not compiled.

``/define:`` *name*\ [``=`` *value*]
   Define the macro *name*. If the optional *value* is supplied, the
   name is defined to have that value.

``/undefine:`` *name*
   Undefine the macro *name*.

``/include:`` *dir*
   Adds *dir* to the search path list which the preprocessor uses to
   find files mentioned in ``#include`` directives. More than one
   ``/include`` option may be supplied.

Miscellaneous options
=====================

``/help``
   Display the command-line usage message.

``/version``
   Display version information.

``/debugger``
   Enter a debugger if console-scepter crashes.

``/case``
   By default console-scepter ignores case when recognizing keywords
   and identifiers, but this option requires the characters in
   identifiers and keywords to have exactly the same case as the
   identifier or keyword definition in order to be recognised.

``/nowarnings``
   Suppress compilation warning messages.

``/trace``
   Trace compilation stages.

Examples
========

The following example would compile the bank demo IDL file. The
generated projects are placed in subfolders called
:file:`bank-protocol`, :file:`bank-stubs`, and :file:`bank-skeletons`
in the current working folder::

   console-scepter /prefix:bank bank.idl

The next example would compile the bank demo IDL file and place the
generated projects in the folders :file:`c:\bank-client\protocol`,
:file:`c:\bank-client\stubs` and :file:`c:\bank-client\skeletons`::

   console-scepter /language dylan /directory c:\bank-client bank.idl

The following example loads the Interface Repository with the
definitions in the Bank demo IDL file::

   console-scepter /back-end ir bank.idl

You can generate the Dylan projects at the same time using::

   console-scepter /back-end:ir /back-end:dylan bank.idl

