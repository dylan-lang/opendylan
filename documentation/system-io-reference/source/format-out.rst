*********************
The Format-Out Module
*********************

Introduction
============

The Format-Out module is a convenient repackaging of two libraries that
provides a simple way to send text to the platform’s standard output
stream. For this purpose, Format-Out uses the Format module and the
Standard-IO module and defines a function `format-out`_. The Format-Out
module exports all the identifiers described in this document. The
Format-Out module re-exports two modules, *format* from the Format
library and *standard-io* from the Standard-IO library.

:doc:`format` and :doc:`standard-io` give full details of the Format and
Standard-IO libraries.

The FORMAT-OUT module
=====================

This section contains a reference entry for each item exported from the
*format-out* module.

format-out
----------

Function

Summary

Formats its arguments on the standard output.

Signature

.. code-block:: dylan

    format-out *control-string* #rest *arguments* => ()

Arguments

- *control-string* An instance of *<string>*.
- *arguments* Instances of *<object>*.

Values

None.

Description

Calls the *format* function from the *format* module on
` <standard-io.htm#17449>`_ from the *standard-io* module,
*control-string*, and *arguments*.

See also
`format <format.htm#12592>`_
` <standard-io.htm#17449>`_

format-out
----------

Sealed g.f. method

Summary
Formats its arguments on the standard output.

Signature

.. code-block:: dylan

    format-out *control-string* #rest *arguments* => ()

Arguments

- *control-string* An instance of *<byte-string>*.
- *arguments* Instances of *<object>*.

Values

None.

Description

Formats its arguments on the standard output. There is one method for
*format-out*, and it is specialized to instances of *<byte-string>*.
