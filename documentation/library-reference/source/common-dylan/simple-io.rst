********************
The Simple IO module
********************

.. current-library:: common-dylan
.. current-module:: simple-io

Common Dylan provides several libraries relevant to formatting and
printing strings, or otherwise using strings for output. These libraries
include Format, Format-out, Print, and Standard-IO. The facilities
provided by these libraries will be excess to many users’ requirements,
who may prefer to use the *simple-io* module that the
*common-dylan* library exports.

The `format-out`_ function converts its
arguments into a Dylan *format string* and then sends that string to the
standard output. The `format-to-string`_
function converts its arguments into a format string and then returns
that format string.

format-out
----------

Function
''''''''

Summary

Formats its arguments to the standard output.

Signature

format-out *format-string* #rest *format-arguments* => ()

Arguments

*format-string* An instance of ``<byte-string>``.

*format-arguments*

Instances of ``<object>``.

Values

None.

Description

Formats its arguments to the standard output.

This function does not use the *\*standard-output\** stream defined by
the Standard-IO library.

format-to-string
----------------

Function
''''''''

Summary

Returns a formatted string constructed from its arguments.

Signature

format-to-string *format-string* #rest *format-arguments* => *string*

Arguments

*format-string* An instance of ``<byte-string>``.

*format-arguments*

Instances of ``<object>``.

Values

*result-string* An instance of ``<byte-string>``.

Exceptions

This function signals an error if any of the format directives in
*format-string* are invalid.

Description

Returns a formatted string constructed from its arguments, which include
a *format-string* of formatting directives and a series of
*format-arguments* to be formatted according to those directives.

The *format-string* must be a Dylan format string as described on pages
112–114 of the DRM.
