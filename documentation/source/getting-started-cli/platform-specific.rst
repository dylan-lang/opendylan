Platform Specific Projects
==========================

.. index:: Platform Specific Projects

When a project involves platform-specific code, some consideration needs to be
made to the LID file and registry, as well as the layout of code.  As an
example, consider the `io
<https://github.com/dylan-lang/opendylan/tree/master/sources/io/>`_ and `system
<https://github.com/dylan-lang/opendylan/tree/master/sources/system/>`_
libraries.

In summary, there must be a LID file for each version of your library that
needs some platform-specific code, and a platform-specific registry file that
points to the appropriate LID file.

LID File
--------

For further details of the LID file format, see :doc:`../library-reference/lid`.

1) The ``Library`` keyword

   The library keyword remains the same across all platforms since it is, after
   all, a platform-dependent version of that library:

   =============== ========================= =========================
   Keyword         unix-io.lid               win32-io.lid
   =============== ========================= =========================
   Library:        io                        io
   =============== ========================= =========================


2) The ``Files`` keyword

   Each platform's project may contain files that are the same in each
   platform, as well as files which are present in one but not in another:

   =============== ========================= =========================
   Keyword         unix-io.lid               win32-io.lid
   =============== ========================= =========================
   Files:          buffered-format           buffered-format
   ..              format-condition          format-condition
   ..              unix-interface            win32-interface
   ..              unix-file-accessor        win32-file-accessor
   ..              unix-standard-io          win32-standard-io
   ..              format-out                format-out
   ..              *(etc)*                   *(etc)*
   C-Source-Files: unix-portability.c
   RC-Files:       ..                        version.rc
   =============== ========================= =========================

3) The ``C-Libraries`` keyword

   Each platform's project will probably require a different set of linked
   libraries. (Here we use an example from the "system" library as the "io"
   library doesn't link directly against any C libraries.)

   =============== ========================= =========================
   Keyword         x86_64-linux-system.lid   x86-win32-system.lid
   =============== ========================= =========================
   C-Libraries:    -ldl                      advapi32.lib
   ..              ..                        shell32.lib
   =============== ========================= =========================

4) The ``Platforms`` keyword

   Platform-specific LID files should use the ``Platforms`` keyword to indicate
   which platforms they apply to. This helps the `deft update
   <https://package.opendylan.org/deft/index.html#deft-update>`_ command decide
   which registry files to create. If there is no ``Platforms`` keyword
   ``deft`` assumes the LID file applies to the current platform and creates a
   registry file for its library.

   =============== ========================= =========================
   Keyword         unix-io.lid               win32-io.lid
   =============== ========================= =========================
   Platforms:      x86_64-linux              win32
   ..              x86_64-darwin             ..
   ..              x86_64-freebsd            ..
   =============== ========================= =========================


LID File Inheritance
--------------------

When LID files are almost identical, it can be useful to create
a base LID file and inherit it, overriding whatever is necessary
for each platform. A per-platform LID file might look like::

    Library:        uv
    LID:            uv-posix.lid
    C-libraries:    -framework CoreServices


Registry
--------

For further details of setting up the registry entries, see
:doc:`source-registries`.

Normally, when a reference to a platform-independent project is placed in the
registry, it is put into the "generic" directory. Platform dependent projects
are placed in subdirectories named for the platform. For example,
:file:`opendylan/sources/registry/x86_64-linux/io` points to ``unix-io.lid``
and :file:`opendylan/sources/registry/x86-win32/io` points to ``win32-io.lid``.


Code Layout
-----------

The "io" library is laid out in the following manner:

1. All platform-specific code is inside a single module (io-internals).
2. ``*-interface.dylan`` contains the low-level functions accessing the
   platform-specific libraries (*e.g.* ``unix-read``, ``win32-read``).
3. ``*-file-accessor.dylan`` uses the functions from (2) to produce a
   platform-independent API (*e.g.* ``accessor-read-into!``).
4. Only those methods from (3) are exported from the module. 

.. note:: Most libraries that are portable aren't as complex in
   their layout as the ``io`` library and don't require separate
   modules.
