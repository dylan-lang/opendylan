Platform Specific Projects
==========================

.. index:: Platform Specific Projects

When a project involves platform-specific code, some consideration needs to be made to the lid file and registry, as well as the layout of code.
As an example, consider the ``io`` library, found in https://github.com/dylan-lang/opendylan/tree/master/sources/io/.


Lid File
--------

For further details of the lid file format, see `Lid file`_.

.. _Lid file: http://opendylan.org/documentation/library-reference/lid.html

1) Library

    =============== ========================= =========================
    Keyword         unix-io.lid               win32-io.lid
    =============== ========================= =========================
    Library:        io                        io
    =============== ========================= =========================

The library remains the same across all platforms since it is, after all, a platform-dependent version of that library.

2) Files

    =============== ========================= =========================
    Keyword         unix-io.lid               win32-io.lid
    =============== ========================= =========================
    Files:          buffered-format           buffered-format
    ..              format-condition          format-condition
    ..              unix-interface            win32-interface
    ..              unix-file-accessor        win-file-accessor
    ..              unix-standard-io          win32-standard-io
    ..              format-out                format-out
    ..              *(etc)*                   *(etc)*
    C-Source-Files: unix-portability.c
    RC-Files:       ..                        version.rc
    =============== ========================= =========================

Each platform's project may contain files that are the same in each platform, as well as files which are present in one but not in another.

3) Linked Libraries (from dylan-lang/opendylan/sources/system)

    =============== ========================= =========================
    Keyword         x86_64-linux-system.lid   x86-win32-system.lid
    =============== ========================= =========================
    C-Libraries:    -ldl                      advapi32.lib
    ..              ..                        shell32.lib
    =============== ========================= =========================

Each platform's project will probably require a different set of linked libraries.



Registry
--------

For further details of setting up the registry entries, see :doc:`source-registries`.

Normally, when a reference to a (platform independent) project is placed in the registry, 
it is put into the generic directory. Platform dependent projects are placed in the platform-labelled
subdirectories. *e.g.*

dylan-lang/opendylan/sources/registry/x86_64-linux/io
  ``abstract://dylan/io/unix-io.lid``

dylan-lang/opendylan/sources/registry/x86-win32/io
  ``abstract://dylan/io/win32-io.lid``


Code Layout
-----------

The ``io`` library is laid out in the following manner:

1. All platform-specific code is inside a single module (``io-internals``).
2. ``*-interface.dylan`` contains the low-level functions accessing the platform-specific libraries (*e.g.* ``unix-read``, ``win32-read``).
3. ``*-file-accessor.dylan`` uses the functions from (2) to produce a platform-independent API (*e.g.* ``accessor-read-into!``).
4. Only those methods from (3) are exported from the module. 
