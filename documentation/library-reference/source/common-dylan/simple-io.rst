********************
The simple-io Module
********************

.. current-library:: common-dylan
.. current-module:: simple-io

Common Dylan provides several libraries relevant to formatting and
printing strings, or otherwise using strings for output. These libraries
include :doc:`format <../io/format>`, :doc:`format-out <../io/format-out>`,
:doc:`print <../io/print>`, and :doc:`standard-io <../io/standard-io>`. The
facilities provided by these libraries will be excess to many users’
requirements, who may prefer to use the ``simple-io`` module that the
``common-dylan`` library exports.

.. function:: format-out

   Formats its arguments to the standard output.

   :signature: format-out *format-string* #rest *format-arguments* => ()

   :parameter format-string: An instance of :drm:`<byte-string>`.
   :parameter format-arguments: Instances of :drm:`<object>`.

   :description:

     Formats its arguments to the standard output.

     This function does not use the :var:`*standard-output*` stream
     defined by the :doc:`Standard-IO <../io/standard-io>` module in the
     :doc:`IO <../io/index>` library.

