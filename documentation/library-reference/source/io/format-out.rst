*********************
The format-out Module
*********************

.. current-library:: io
.. current-module:: format-out

Introduction
============

The Format-Out module is a convenient repackaging of two libraries that
provides a simple way to send text to the platform’s standard output
stream. For this purpose, Format-Out uses the Format module and the
Standard-IO module and defines a function :gf:`format-out`. The Format-Out
module exports all the identifiers described in this document. The
Format-Out module re-exports two modules, ``format`` from the Format
library and ``standard-io`` from the Standard-IO library.

:doc:`format` and :doc:`standard-io` give full details of the Format and
Standard-IO libraries.

The format-out module
=====================

This section contains a reference entry for each item exported from the
*format-out* module.

.. generic-function:: format-out

   Formats its arguments on the standard output.

   :signature: format-out *control-string* #rest *arguments* => ()

   :parameter control-string: An instance of ``<string>``.
   :parameter #rest arguments: Instances of ``<object>``.

   :description:

     Calls the :gf:`format` function from the *format* module on
     :var:`*standard-output*` from the *standard-io* module,
     *control-string*, and *arguments*.

   See also

   - :gf:`format`
   - :var:`*standard-output*`

.. method:: format-out
   :sealed:
   :specializer: <byte-string>

   Formats its arguments on the standard output.

   :signature: format-out *control-string* #rest *arguments* => ()

   :parameter control-string: An instance of ``<byte-string>``.
   :parameter #rest arguments: Instances of ``<object>``.

   :description:

   Formats its arguments on the standard output. There is one method for
   :gf:`format-out`, and it is specialized to instances of ``<byte-string>``.
