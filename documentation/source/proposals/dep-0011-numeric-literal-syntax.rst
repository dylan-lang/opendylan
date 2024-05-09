**********************
Numeric Literal Syntax
**********************

===============  =============================================
DEP #:           11
Type:            Standards Track
Author:          Carl Gay
Status:          Final
Created:         11-Feb-2019 (Thomas Edison's birthday)
Last-Modified:   13-Feb-2019
Post-History:    `04-Mar-2019 <https://groups.google.com/forum/#!topic/dylan-lang/Wb3D6ioe1GM/discussion>`_
Target-Version:  Open Dylan 2019.1
===============  =============================================


Abstract
========

This DEP proposes to change the literal syntax for Dylan integers and
floating point numbers by allowing the underscore character to be used
between any two digits.

Motivation
==========

This change is designed to aid readability of large integers and large
or high precision floats.

Consider ::

  define constant $nanos-per-second = 1000000000;

vs ::

  define constant $nanos-per-second = 1_000_000_000;

This should eliminate the need to count groups of three digits to be
certain you're reading it correctly.

Proposal
========

The Dylan lexical grammar is changed as follows:

* For clarity, a new character class is introduced with a single
  character, underscore (_):

  | underscore:
  |     _

* underscore :subscript:`opt` is added to the second alternative of
  the `decimal-integer`, `binary-integer`, `octal-integer`, and
  `hex-integer` productions:

  | binary-integer:
  |     binary-digit
  |     binary-integer underscore :subscript:`opt` binary-digit

  | octal-integer:
  |     octal-digit
  |     octal-integer underscore :subscript:`opt` octal-digit

  | decimal-integer:
  |     decimal-digit
  |     decimal-integer underscore :subscript:`opt` decimal-digit

  | hex-integer:
  |     hex-digit
  |     hex-integer underscore :subscript:`opt` hex-digit

The proposed change in floating point numeric syntax follows because
the `floating-point` production references the `decimal-integer`
production.

Examples of valid numeric literals include::

  1_000_000
  -4_000_000
  2.000_002
  3_000.000_123
  4.0e1_000
  #xdead_beef
  #b1111_0000

and also::

  1_2_3_4_5_6_7  // valid but not recommended

Examples of uses of underscore that are **not** allowed include::

  1__2
  _123
  -123_
  1_.23
  1._23
  1.23_
  #x_feed
  #o777_

Reference Implementation
========================

This was incorporated into Open Dylan 2020.1 in this commit
`<https://github.com/dylan-lang/opendylan/commit/0b3c60e279c21b137c05051153738bc4034072e4>`_.


Backward Compatibility
======================

Dylan code that uses the new syntax is not compatible with older
versions of Dylan compilers. Any code that uses the new syntax creates
a dependency on the Open Dylan version in which this feature is
released.  In particular, this syntax should not be used in compiler
code (or supporting libraries) until it is supported by the oldest
bootstrapping compiler.

Copyright
=========

This document may be freely redistributed.
