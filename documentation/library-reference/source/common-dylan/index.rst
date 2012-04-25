************************
The common-dylan Library
************************

.. current-library:: common-dylan

The Common Dylan library contains the Common Extensions library and the
Dylan library. It provides a number of features that were either omitted
from the Dylan language described in the DRM, or that Open Dylan's
developers have found useful in a broad range of situations.

The Common Dylan library exports the following modules:

- :doc:`common-dylan`
  Miscellaneous extensions to the Dylan language.
- :doc:`simple-io` Simple formatting facilities. For more flexible
  formatting and printing, consider the separate Format and Print
  libraries.
- :doc:`simple-random` A simple facility for generating pseudo-random
  integers.
- :doc:`threads` An interface to threading functionality.
- :doc:`finalization` An object finalization interface.
- :doc:`transcendentals`
  A set of open generic functions for ANSI C-like behavior over real
  numbers.
- :doc:`machine-words`
  A set of functions for representing a limited range of integral
  values.

.. toctree::
   :hidden:

   common-dylan
   finalization
   machine-words
   simple-io
   simple-random
   threads
   transcendentals

