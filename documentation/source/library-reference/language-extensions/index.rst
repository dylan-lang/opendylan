*************************
Dylan Language Extensions
*************************

.. TODO: how do the ../dylan/ documents (finalization, primitives, threads) fit in?

.. current-library:: common-dylan
.. current-module:: common-dylan

The Dylan language is described in the :drm:`Dylan Reference Manual <Title>` by
Andrew Shalit (Addison-Wesley, 1996), hereafter referred to as "the DRM".

The ways in which Open Dylan differs from the DRM are documented in
:doc:`language-differences`. The rest of this document describes features added
to the language while retaining compatibility with the DRM.  These extensions
are either built in to the ``dylan`` library or are available in a separate
library, :doc:`common-dylan <../common-dylan/index>`.

The :doc:`common-dylan <../common-dylan/index>` library exports the
:mod:`common-dylan:common-dylan` module and several other very frequently used
modules.

The majority of the extensions are in the :doc:`common-extensions
<../common-dylan/common-extensions>` module
of the :doc:`common-dylan <../common-dylan/index>` library. That library
also exports a number of smaller modules that contain other basic
facilities such as :doc:`simplified formatting <../common-dylan/simple-format>`,
:doc:`pseudo-random integer generation <../common-dylan/simple-random>`,
and :doc:`object finalization <../dylan/finalization>`.

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

This section describes Dylan language extensions, that is, extensions made to
the Dylan library, and to the base language syntax, as they are defined in the
DRM. These extensions are available to applications in the ``dylan`` library's
``dylan`` module.

.. toctree::
   :titlesonly:

   language-differences
   define-function
   for-iteration
   weak-tables
   inlining
   numbers
   object-with-elements
   macro-system-extensions
   parser-expansions
   alternative-curry-syntax
   numeric-literals
   string-literals

All the other language extensions are described in :doc:`../common-dylan/index`.
