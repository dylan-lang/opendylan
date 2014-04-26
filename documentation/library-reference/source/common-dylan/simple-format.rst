************************
The simple-format Module
************************

.. current-library:: common-dylan
.. current-module:: simple-format

Common Dylan provides several libraries relevant to formatting and
printing strings, or otherwise using strings for output. These libraries
include :doc:`format <../io/format>`, :doc:`format-out <../io/format-out>`,
:doc:`print <../io/print>`, and :doc:`standard-io <../io/standard-io>`. The
facilities provided by these libraries will be excess to many users’
requirements, who may prefer to use the ``simple-format`` module that the
``common-dylan`` library exports.

.. function:: format-to-string

   Returns a formatted string constructed from its arguments.

   :signature: format-to-string *format-string* #rest *format-arguments* => *string*

   :parameter format-string: An instance of :drm:`<byte-string>`.
   :parameter #rest format-arguments: Instances of :drm:`<object>`.
   :value result-string: An instance of :drm:`<byte-string>`.

   :conditions:

     This function signals an error if any of the format directives in
     *format-string* are invalid.

   :description:

     Returns a formatted string constructed from its arguments, which
     include a *format-string* of formatting directives and a series of
     *format-arguments* to be formatted according to those directives.

     The *format-string* must be a Dylan format string as described on
     :drm:`pages 112–114 of the DRM <Condition_Messages>`.

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

