*************************
Dylan Language Extensions
*************************

Introduction
============

The Dylan language is described in `The Dylan Reference Manual
<http://opendylan.org/books/drm/>`_ by Andrew Shalit (Addison-Wesley,
1996). We call this book "the DRM" hereafter.

Open Dylan provides an implementation of the Dylan language
described by the DRM, with a few exceptions that are documented in
:doc:`language-differences`.

Open Dylan provides the Dylan language in the ``dylan`` module of the
``dylan`` library.

This chapter is an introduction to Open Dylan's extensions to the
Dylan language.  These extensions are either built in to the ``dylan``
library or are available in a separate library, :doc:`common-dylan
<../common-dylan/index>`.

The majority of the extensions are in the :doc:`common-extensions
<../common-dylan/common-extensions>` module
of the :doc:`common-dylan <../common-dylan/index>` library. That library
also exports a number of smaller modules that contain other basic
facilities such as :doc:`simplified formatting <../common-dylan/simple-io>`,
:doc:`pseudo-random integer generation <../common-dylan/simple-random>`,
and :doc:`object finalization <../common-dylan/finalization>`.

Open Dylan provides a convenience library, ``common-dylan``, that
combines the ``dylan`` and ``common-extensions`` modules to provide a
convenient "dialect" of Dylan, exported from the module ``common-dylan``:

.. code-block:: dylan

    define library common-dylan
      use dylan, export: all;
      use common-extensions, export: all;

      export common-dylan;
    end module;

    define module common-dylan
      use dylan, export: all;
      use common-extensions, export: all;
    end module;

This section describes the common language extensions, that is,
extensions made to the Dylan library as it is defined in DRM. These
extensions are available to applications in the ``dylan`` library’s
``dylan`` module.

.. toctree::
   :titlesonly:

   language-differences
   define-function
   for-iteration
   weak-tables
   inlining

All the other language extensions are described in :doc:`../common-dylan/index`.
