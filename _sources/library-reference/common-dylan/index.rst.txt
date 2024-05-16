************************
The common-dylan Library
************************

.. current-library:: common-dylan

The ``common-dylan`` library contains a number of features that were either
omitted from the Dylan language described in the DRM, or that Open
Dylan's developers have found useful in a broad range of situations.

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

The ``common-dylan`` module re-exports everything in the
``common-extensions`` and ``dylan`` modules.  This is a convenience module
that is very widely used.

.. TODO(cgay): streams-protocol, locators-protocol.

It also re-exports these modules which are defined in
:doc:`the dylan library <../dylan/index>`:

- `The dylan Module </books/drm/>`_
- :doc:`../dylan/threads`
- :doc:`../dylan/finalization`
