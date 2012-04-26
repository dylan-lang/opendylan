********************
The simple-io Module
********************

.. current-library:: common-dylan
.. current-module:: simple-io

Common Dylan provides several libraries relevant to formatting and
printing strings, or otherwise using strings for output. These libraries
include *format*, *format-out*, *print*, and *standard-io*. The facilities
provided by these libraries will be excess to many users’ requirements,
who may prefer to use the *simple-io* module that the
*common-dylan* library exports.

.. function:: format-out

   Formats its arguments to the standard output.

   :signature: format-out *format-string* #rest *format-arguments* => ()

   :parameter format-string: An instance of ``<byte-string>``.
   :parameter format-arguments: Instances of ``<object>``.

   :description:

     Formats its arguments to the standard output.

     This function does not use the :var:`*standard-output*` stream
     defined by the Standard-IO module in the IO library.

