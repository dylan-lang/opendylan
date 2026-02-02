************************
The common-dylan Library
************************

.. current-library:: common-dylan

.. library:: common-dylan

.. module:: common-dylan

The ``common-dylan`` library contains a number of features that were either
omitted from the Dylan language described in the DRM, or that Open
Dylan's developers have found useful in a broad range of situations.

In addition to the modules documented below, the ``common-dylan`` library exports the
``common-dylan`` module which re-exports everything in the :doc:`common-extensions
<common-extensions>` module and :drm:`dylan <Index>` modules.  This is a convenience
module that is very widely used.

.. toctree::
   :maxdepth: 1

   byte-vector
   common-extensions
   machine-words
   simple-format
   simple-profiling
   simple-random
   simple-timers
   transcendentals

.. TODO(cgay): streams-protocol, locators-protocol.

The ``common-dylan`` library also re-exports these modules which are defined in the
``dylan`` library: 

- `The dylan Module <https://opendylan.org/books/drm/>`_
- :doc:`../dylan/threads`
- :doc:`../dylan/finalization`
