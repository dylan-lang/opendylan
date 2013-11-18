************************
The common-dylan Library
************************

.. current-library:: common-dylan

The *common-dylan* library contains the *common-extensions* module and
the Dylan library. It provides a number of features that were either
omitted from the Dylan language described in the DRM, or that Open
Dylan's developers have found useful in a broad range of situations.

The Common Dylan library exports the following modules:

- :doc:`common-extensions` Miscellaneous extensions to the Dylan language
  that have been found to be broadly useful.

- The *common-dylan* module re-exports everything in the
  *common-extensions* and *dylan* modules.  This is a convenience module
  that is very widely used.

- :doc:`simple-io` Simple formatting facilities. For more flexible
  formatting and printing, see the *io* library.

- :doc:`simple-random` A facility for generating pseudo-random integers.

- :doc:`transcendentals` A set of open generic functions for ANSI
  C-like behavior over real numbers.

- :doc:`machine-words` A set of functions for representing a limited
  range of integral values.

.. TODO(cgay): streams-protocol, locators-protocol, simple-profiling, byte-vector.

It also re-exports these modules which are defined in the *dylan* library:

- `dylan <http://opendylan.org/books/drm/>`_ The dylan module.
- :doc:`threads` An interface to threading functionality.
- :doc:`finalization` An object finalization interface.

.. toctree::
   :hidden:

   common-extensions
   finalization
   machine-words
   simple-io
   simple-random
   simple-timers
   threads
   transcendentals

