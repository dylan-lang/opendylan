*******************************************
Better String Manipulation for common-dylan
*******************************************

==============  =============================================
Proposal #:     1
Title:          Better Strings API
Author:         Carl Gay
Status:         Draft
Content-Type:   text/x-rst
Created:        2011.12.27
Dylan-Version:  2011.1
==============  =============================================


.. contents::


Abstract
========

The ``dylan`` and ``common-dylan`` libraries have only limited string
manipulation functionality.  This proposal is an attempt to provide a
richer strings API.

For this draft I'm being slightly loose with signatures.  I use names
such as char-or-string rather than writing out each overloaded
signature with explicit types.  I'll fix that in a later draft once
the basic form of the API is agreed.  --cgay



Goals
=====

#. Comprehensive string APIs available via the ``common-dylan`` module
   in the ``common-dylan`` library.

#. Replace existing string-extensions module with names directly
   available in common-dylan.

#. Consistent high-level API for when readability and convenience are
   paramount.  Low-level functions available where a significant
   performance improvement can be had.

#. String operations consistent with regular expression operations
   where possible.  i.e., share the same generic functions


Non Goals
=========

#. Unicode

#. Immutable strings

#. Strings not implemented as subclasses of ``<sequence>``.


Constants
=========

TODO: remove these.  I don't see any need for them to be exported.
Leaving them here for now, in case something comes up.

$control-characters
$printable-characters
$graphic-characters
$decimal-digits
$octal-digits
$hexadecimal-digits
$ascii-letters
$whitespace-characters
$alphanumeric-characters



Functions
=========


Predicates
----------

These all work for a single character or for a string.  When applied
to a string they are true if they're true for each character in the
string.

These are mainly taken from string-extensions/character-type.dylan.

ascii-control?     (char-or-string) => (<boolean>)

ascii-printable?   (char-or-string) => (<boolean>)

ascii-graphic?     (char-or-string) => (<boolean>)

ascii-letter?      (char-or-string) => (<boolean>)

lowercase?         (char-or-string) => (<boolean>)

uppercase?         (char-or-string) => (<boolean>)

whitespace?        (char-or-string) => (<boolean>)

alphanumeric?      (char-or-string) => (<boolean>)

decimal-digit?     (char-or-string) => (<boolean>)

octal-digit?       (char-or-string) => (<boolean>)

hexadecimal-digit? (char-or-string) => (<boolean>)


Comparisons
-----------

Characters
~~~~~~~~~~

Case insensitive character comparisons for use as the value of various
"test" parameters.  For case-sensitive comparison, use "=".
::

    ci-char-equal?    (char1, char2)
    ci-char-less?     (char1, char2)
    ci-char-greater?  (char1, char2)

**Discussion**

The "ci-*" naming scheme is consistent with other proposed functions
(below).  Case-insensitive character comparison is used frequently
enough in our existing code base that at least ``ci-char-equal?``
should exist.  ``ci-char-less?`` and ``ci-char-greater?`` are likely
to be less useful, but are provided for completeness.


Strings
~~~~~~~

String comparisons, both case-sensitive and case-insensitive.  These
default to comparing the entire string but allow comparing substrings
via keyword arguments.
::

    string-compare (string1, string2, #key start1, end1, start2, end2, test)
        Low-level function used to implement the functions below.

These are case-sensitive::

    string-equal? (string1, string2, #key start1, end1, start2, end2)
    string-less? (string1, string2, #key start1, end1, start2, end2)
    string-greater? (string1, string2, #key start1, end1, start2, end2)

These are case-insensitive::

    ci-string-equal?   (string1, string2, #key start1, end1, start2, end2)
    ci-string-less?    (string1, string2, #key start1, end1, start2, end2)
    ci-string-greater? (string1, string2, #key start1, end1, start2, end2)

From the ``dylan`` module (included for completeness)::

    =  (char-or-string, char-or-string)
    <  (char-or-string, char-or-string)
    >  (char-or-string, char-or-string)


**Discussion**

Some people may object to the ci-* functions on the grounds that a
"test" parameter could be added to the non-ci-* functions instead.
But consider this type of code, which is likely to be fairly common::

    sort(seq, test: ci-string-less?)

Instead one would have to write this::

    sort(seq, test: rcurry(string-less?, test: ci-char-equal?))

or worse, if ci-char-equal? is removed on the same grounds::

    sort(seq, test: rcurry(string-less?, test: method (c1, c2)
                                                 as-lowercase(c1) = as-lowercase(c2)
                                               end))

or, the less efficient but more concise::

    sort(seq, test: method (s1, s2) as-lowercase(s1) < as-lowercase(s2) end)

The abbreviation "ci" stands for case-insensitive.  In the author's
opinion "case-insensitive-string-less?" etc. are simply too long and
"ci" is easy enough to remember.  The author is willing to be
out-voted on this point.


Conversions
-----------

make(<string>, size: n, fill: char)

concatenate(sequence, #rest sequences)  // somewhat redundant with "join"

copy(string, #key start, end)

lowercase(char-or-string, #key start, end)

lowercase!(char-or-string, #key start, end)

uppercase(char-or-string, #key start, end)

uppercase!(char-or-string, #key start, end)

wrap(string, width, #key prefix = "", test, strict?) => (new-string)
    Return a copy of ``string`` which has been line wrapped at column width
    ``width``.  Each line is prefixed with ``prefix``, which defaults to the
    empty string.  The ``test`` determines where it is acceptable to wrap
    long lines.  It defaults to a function that will only wrap on whitespace.
    If any non-breakable sequence of characters (plus the prefix) is wider
    than ``width``, then ``width`` will be exceeded in that case unless
    ``strict?`` is true.

    **TODO:** exact contract of ``test`` function.  It may become more
    obvious during implementation.  Consider: how to deal with wrapping
    on '-'.

pad-left(string, width, #key fill = ' ')
pad-right(string, width, #key fill = ' ')
pad-center(string, width, #key fill = ' ')
    Return a string of the given ``width``.  If the given ``string``
    is shorter than ``width``, add ``fill`` equally to both sides of
    the result such that the original string is center aligned.

    Examples::

      pad-center("x", 5) => "  x  "
      pad-center("x", 4) => "  x " or " x  "    (unspecified)
      pad-center("x", 7, fill: '.') => "...x..."

    Return a string padded on the left or right with ``fill`` until it
    is ``width`` columns wide.  If ``string``.size is >= ``width`` no
    padding is performed.

strip(string, #key start, end, test)

find(string, pattern, #key start, end, test)

starts-with?(string, pattern) => <boolean>

ends-with?(string, pattern) => <boolean>

replace(string, pattern, new, #key test)

replace!(string, pattern, new, #key test)

slice(sequence, #key start, end, step) => (new-sequence)

slice!(sequence, #key start, end, step) => (sequence)
    Return value may share structure with original sequence.

join(parts, separator, #key start, end, key, conjunction) => (string)

split(string, separator, #key start, end, max, remove-if-empty?) => (sequence-of-strings)

split-lines(string, #rest split-keys) => (sequence-of-strings)
    Alternative: split(text, find-end-of-line, #key ...)

count(string, pattern, #key start, end, test, overlap?) => (n)

interpolate(string, table) => (string)
  interpolate("foo {x}", table("x" => 5)) => "foo 5"


Other
=====

* Move <character-set> and friends from string-extensions to
  regular-expressions.  That's the only place that uses it, and it is
  unlikely to be of much use elsewhere.  See conversation in #dylan on
  2011.12.26.

