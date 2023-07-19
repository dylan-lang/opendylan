*********************
String Literal Syntax
*********************

===============  =============================================
DEP #:           12
Supersedes:      DEP 8
Type:            Standards Track
Affects-DRM:     Yes
Author:          Carl Gay
Status:          Draft
Created:         22-Mar-2023
Last-Modified:   22-Mar-2023
Post-History:    `28-Mar-2023 <https://groups.google.com/g/dylan-lang/c/xhofah0KYt8>`_
Target-Version:  2023.1
===============  =============================================


Abstract
========

Dylan string literals have two deficiencies when compared to many current
programming languages, most notably the inability to encode multi-line strings
into a single source token that crosses multiple source lines and no fully
supported way to represent unescaped ("raw") strings. This DEP proposes a
literal syntax for both of these cases. Briefly, it proposes syntax for
one-line raw strings delimited by ``#r"``, multi-line strings delimited by
``"""`` and ``#r"""``, and quoted symbols delimited by ``#"""``.


Rationale
=========

Summary: Literal syntax to address both of these deficiencies would be nice to
have and is something that people expect to exist in modern programming
languages.

Multi-line String Literals
--------------------------

While it should be noted that encoding long string literals into source code is
not good practice when internationalization is desired, it is convenient to
have this ability for quick scripts and especially for encoding test data.
Most modern programming languages provide this ability.

For very short multi-line strings one can get away with using \\n or
\\r\\n in a regular string::

  "line one\nline two"

but readability quickly suffers as these strings get longer.  An alternative is
to put one string on each line and rely on automatic string concatenation by
the compiler::

  "line one\n"
  "line two"

The problem here is that changing the contents of the string quickly becomes an
editing chore as the programmer attempts to keep the lines a similar length,
and remembering to put the \\n at the end of each line.  Having a multi-line
string literal syntax would alleviate both of these problems.

Raw String Literals
-------------------

Raw string literals (that is, strings in which ``'\\'`` is not a special
character) are useful for inputting data that needs to contain many ``'\\'``
characters. The primary example of this is regular expressions, which quickly
become difficult to interpret when they themselves contain many backslashes.

Open Dylan has a fairly simple work-around for this problem, which it calls
`parser expansions
<https://opendylan.org/documentation/library-reference/language-extensions/parser-expansions.html>`_::

   define function string-parser (s) s end;
   define constant s1 = "\\.\\[\\*\\\\\\]";     // standard Dylan
   define constant s2 = #:string:"\.\[\*\\\]";  // uses string-parser

There are some problems with this mechanism:

#. Because the parser can have any name ("string" in the above example) and
   various different delimiters, it's difficult to provide good editor support.

#. The parser only allows a single character to delimit the string, meaning
   that that character is forbidden within the string literal.  For long string
   literals it can be hard to predict which delimiter character (if any) will
   not be needed.

#. For the relatively common task of specifying a raw string, each individual
   library needs to either define ``string-parser`` or import it. (This could
   be made easier by exporting it from the ``common-dylan`` module.)

#. This kind of parser prevents consecutive string literal concatenation by the
   compiler because the reader doesn't produce a string literal fragment for
   it.  Instead it emits a call to ``string-parser``.

Having a standard raw string literal syntax would eliminate or reduce these
problems.


Specification
=============

Multi-line string literals are delimited by three double quote characters on
each end: ``"""``. Any string literal, whether one-line or multi-line, may be
prefixed with ``#r`` or ``#R`` to disable backslash escape processing, i.e., to
make it a raw string literal.

Literal end-of-line sequences are always interpreted as a single LF character,
in both raw and escaped string literals, regardless of operating system
platform. Specifically,

#. CR (character code 13) by itself is converted to a single LF (character code
   10).

#. CRLF (character codes 13 10, when occurring consecutively) are converted to
   a single LF (character code 10).

An implication of this design is that the only way to include a CR character in
a string literal is with the escape sequence ``\r`` in a non-raw string.

All string literals, whether escaped, raw, one-line, or multi-line, continue to
adhere to the rule that consecutive string literals separated by only
whitespace are automatically concatenated.

Because Dylan's quoted symbol syntax (also known as "unique string" syntax) is
just `#` followed by any standard string, we also allow ``#"""`` to indicate a
multi-line quoted symbol, to be consistent. No new syntax is provided to create
a "raw" quoted symbols, i.e., quoted symbols without escape processing.

In the Dylan Reference Manual, in the section `Tokens
<https://opendylan.org/books/drm/Lexical_Grammar#HEADING-117-3>`_, ``#r`` is
added to the ``#-word`` production.

The BNF, which augments `Character and String Literals
<https://opendylan.org/books/drm/Lexical_Grammar#HEADING-117-38>`_ in the DRM,
is shown below.

::

    CHARACTER-LITERAL:
        ' character '

    character:
        any printing character (including space) except for ' or \

        \ escape-character

    STRING:
        " more-string

        " " " multi-line-string

        # r raw-string

    more-string:
        string-character more-string

        "

    multi-line-string:
        " " " more-multi-line-string

    more-multi-line-string:
        multi-line-string-character more-multi-line-string

        " " "

    multi-line-string-character:
        any character except for \ or three " in a row

        \ escape-character

    raw-string:
        " more-raw-string

        " " " more-raw-string-multi-line

    more-raw-string:
        raw-string-character more-raw-string

        "

    more-raw-string-multi-line:
        raw-string-character-multi-line more-raw-string-multi-line

        " " "

    string-character:
        any printing character (including space) except for " or \

        \ escape-character

    raw-string-character:
        any printing character (including space) except for "

    raw-string-character-multi-line:
        any character but not three " in a row

    escape-character:
        one of \ ' " a b e f n r t 0

        < hex-digits >

Examples
--------

Equivalent to ``"abc"``::

  """abc""" or #r"abc" or #r"""abc"""

Equivalent to ``"line one\nline two"`` but **never** equivalent to ``"line
one\r\nline two"``::

  """line one
  line two"""

Equivalent to ``"let x = \"foo\";"``::

  """let x = "foo";"""

Equivalent to ``"\nfoo\nbar\n"``::

  """
  foo
  bar
  """

Equivalent to ``"^\\s*([0-9A-Fa-f]+)\\s*"``::

  #r"^\s*([0-9A-Fa-f]+)\s*"

Equivalent to ``"foo\nbar\\[A-Z]+"``::

  #r"""foo
  bar\[A-Z]+"""


Reference Implementation
========================

A reference implementation is underway `on github
<https://github.com/cgay/opendylan/commits/dep12>`_.


Revision History
================

The revision history of this document is available here:
https://github.com/dylan-lang/website/commits/master/source/proposals/dep-0012-string-literals.rst
