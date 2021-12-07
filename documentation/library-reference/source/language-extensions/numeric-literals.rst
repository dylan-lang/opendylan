Numeric Literal Syntax
======================

`Dylan Enhancement Proposal (DEP) 11
<https://opendylan.org/proposals/dep-0011-numeric-literal-syntax.html>`_ extends the
literal syntax for integers and floats to allow ``_`` (underscore) between any two
consecutive digits, to provide better readability for very large constants. This includes
binary, octal, decimal, and hexadecimal literals.

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

See `DEP 11 <https://opendylan.org/proposals/dep-0011-numeric-literal-syntax.html>`_ for
details.
