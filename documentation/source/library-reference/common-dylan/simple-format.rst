************************
The simple-format Module
************************

.. current-library:: common-dylan
.. current-module:: simple-format

The `simple-format` module provides a subset of the functionality of the
:doc:`format <../io/format>`, :doc:`format-out <../io/format-out>`, :doc:`print
<../io/print>`, and :doc:`standard-io <../io/standard-io>` modules, for
programs that just need to do simple output to the console without pulling in
the full :doc:`IO <../io/index>` library.

.. function:: format-to-string

   Returns a formatted string constructed from its arguments.

   :signature: format-to-string *format-string* #rest *format-arguments* => *string*

   :parameter format-string: An instance of :drm:`<byte-string>`.
   :parameter #rest format-arguments: Instances of :drm:`<object>`.
   :value result-string: An instance of :drm:`<byte-string>`.

   :conditions:

     This function signals an error of type :drm:`<simple-error>` if any of the
     format directives in *format-string* are invalid.

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

     This function writes directly to the console using the underlying OS
     facilities rather than using the :var:`*standard-output*` stream defined
     by the :doc:`standard-io <../io/standard-io>` module in the :doc:`IO
     <../io/index>` library. For example, on Unix systems this writes to file
     descriptor 1 via the ``write`` C function.

