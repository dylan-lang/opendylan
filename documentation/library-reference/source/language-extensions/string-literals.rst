String Literal Syntax
=====================

`Dylan Enhancement Proposal (DEP) 12
<https://opendylan.org/proposals/dep-0012-string-literals.html>`_ extends the
literal syntax for strings to include strings without any escape character
processing ("raw" strings) and multi-line strings. Briefly,

#. Multi-line strings begin with three double-quote characters: ``"""``

#. End-of-line sequences in multi-line strings are always parsed as a single
   Newline (``\n``) character, regardless of source file line endings or the
   conventions of the operating system.

#. Any string, whether delimited by ``"`` or ``"""`` may be prefixed with
   ``#r`` or ``#R`` to disable escape sequence processing.

#. Any leading whitespace that *matches the whitespace preceding the end
   ``"""`` delimiter* is removed, allowing multi-line strings to be formatted
   nicely in source code.

See `DEP 12 <https://opendylan.org/proposals/dep-0012-string-literals.html>`_
for details.
