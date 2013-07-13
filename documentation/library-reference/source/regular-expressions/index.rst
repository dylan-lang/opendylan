*******************************
The regular-expressions Library
*******************************

.. current-library:: regular-expressions
.. current-module:: regular-expressions


Overview
========

The ``regular-expressions`` library exports the
``regular-expressions`` module, which contains functions that compile
and search for regular expressions. The module has the same semantics
as Perl (version 4) unless otherwise noted.

A regular expression that is grammatically correct may still be
illegal if it contains an infinitely quantified sub-regex that matches
the empty string. That is, if R is a regex that can match the empty
string, then any regex containing R*, R+, and R{n,} is illegal. In
this case, the ``regular-expressions`` library will signal an
:class:`<invalid-regex>` error when the regex is parsed.

Quick Start
-----------

The most common use of regular expressions is probably to perform a
search and figure out what text matched and/or where it occurred in a
string.  You need to ``use regular-expressions;`` in both your library
and your module, and then...

.. code-block:: dylan

  define constant $re :: <regex> = compile-regex("^abc(.+)123$");

  let match :: <regex-match> = regex-search($re, "abcdef123");
  // match is #f if search failed.

  if (match)
    let text = match-group(match, 1);
    // text = "def"

    let (text, start, _end) = match-group(match, 1);
    // text = "def", start = 3, _end = 6

    match-group(match, 2) => error: <invalid-match-group>
    // group 0 is the entire match
    ...

  compile-regex("*") => error: <invaled-regex>


Reference
=========

.. class:: <regex>
   :sealed:

      A compiled regular expression object.  These should only be
      created via :func:`compile-regex`.


.. class:: <regex-error>
   :sealed:

     The superclass of all regular expression-related errors.

   :superclasses: <format-string-condition>, <error>


.. class:: <invalid-regex>
   :sealed:

      Signalled by :func:`compile-regex` when the given regular
      expression cannot be compiled.

   :superclasses: :class:`<regex-error>`
   :keyword pattern:


.. generic-function:: regex-error-pattern
   :sealed:

      Return the pattern that caused an :class:`<invalid-regex>` error.

   :signature: regex-error-pattern *error* => *pattern*

   :parameter error: An :class:`<invalid-regex>`.
   :value pattern: A :drm:`<string>`.


.. class:: <invalid-match-group>
   :sealed:

      Signalled when an invalid group identifier is passed to :func:`match-group`.

   :superclasses: :class:`<regex-error>`


.. class:: <regex-match>
   :sealed:

      Stores the match groups and other information about a specific regex search result.

   :superclasses: :drm:`<object>`
   :keyword regular-expression:


.. generic-function:: compile-regex
   :sealed:

      Compile a string into a :class:`<regex>`.

   :signature: compile-regex *pattern* #key *case-sensitive* *verbose* *multi-line* *dot-matches-all* *use-cache* => *regex*

   :parameter pattern: A :drm:`<string>`.
   :parameter #key case-sensitive: A :drm:`<boolean>`, default ``#t``.
   :parameter #key verbose: A :drm:`<boolean>`, default ``#f``.
   :parameter #key multi-line: A :drm:`<boolean>`, default ``#f``.
   :parameter #key dot-matches-all: A :drm:`<boolean>`, default ``#f``.
   :parameter #key use-cache: A :drm:`<boolean>`, default ``#t``.  If true,
     the resulting regular expression will be cached and re-used the
     next time the same string is compiled.
   :value regex: A :class:`<regex>`.
   :conditions: :class:`<invalid-regex>` is signalled if *pattern* can't
     be compiled.


.. generic-function:: regex-pattern
   :sealed:

      Return the :drm:`<string>` from which *regex* was created.

   :signature: regex-pattern *regex* => *pattern*

   :parameter regex: A :class:`<regex>`.
   :value pattern: A :drm:`<string>`.


.. generic-function:: regex-group-count
   :sealed:

      Return the number of groups in a :class:`<regex>`.

   :signature: regex-group-count *regex* => *num-groups*

   :parameter regex: A :class:`<regex>`.
   :value num-groups: An :drm:`<integer>`.


.. generic-function:: regex-position
   :sealed:

      Find the position of *pattern* in *text*.

   :signature: regex-position *pattern* *text* #key *start* *end* *case-sensitive* => *regex-start*, #rest *marks*

   :parameter pattern: A :class:`<regex>`.
   :parameter text: A :drm:`<string>`.
   :parameter #key start: A :drm:`<integer>`, default ``0``.  The index at which
     to start the search.
   :parameter #key end: An :drm:`<integer>`, default ``*text*.size``.  The index
     at which to end the search.
   :parameter #key case-sensitive: A :drm:`<boolean>`, default ``#t``.
   :value regex-start: An instance of ``false-or(<integer>)``.
   :value #rest marks: An instance of :drm:`<object>`.

   A match will only be found if it fits entirely within the range
   specified by *start* and *end*.

   If the regular expression is not found, return #f, otherwise return
   a variable number of indices marking the start and end of groups.

   This is a low-level API.  Use :func:`regex-search` if you want to
   get a :class:`<regex-match>` object back.


.. generic-function:: regex-replace
   :sealed:

      Replace occurrences of *pattern* within *big* with *replacement*.

   :signature: regex-replace *big* *pattern* *replacement* #key *start* *end* *count* *case-sensitive* => *new-string*

   :parameter big: The :drm:`<string>` within which to search.
   :parameter pattern: The :class:`<regex>` to search for.
   :parameter replacement: The :drm:`<string>` to replace *pattern* with.
   :parameter #key start: An :drm:`<integer>`, default ``0``.  The index in *big*
     at which to start searching.
   :parameter #key end: An :drm:`<integer>`, default ``*big*.size``.  The index
     at which to end the search.
   :parameter #key case-sensitive: A :drm:`<boolean>`, default ``#t``.
   :parameter #key count: An instance of ``false-or(<integer>)``, default ``#f``.
     The number of matches to replace.  ``#f`` means to replace all.
   :value new-string: An instance of :drm:`<string>`.

   A match will only be found if it fits entirely within the range
   specified by *start* and *end*.

.. generic-function:: regex-search
   :sealed:

      Search for a *pattern* within *text*.

   :signature: regex-search *pattern* *text* #key *anchored* *start* *end* *case-sensitive* => *match*

   :parameter pattern: The :class:`<regex>` to search for.
   :parameter text: The :drm:`<string>` in which to search.
   :parameter #key anchored: A :drm:`<boolean>`, default ``#f``.  Whether or
     not the search should be anchored at the start position.  This is
     useful because "^..." will only match at the beginning of a string,
     or after \n if the regex was compiled with multi-line = #t.
   :parameter #key start: An :drm:`<integer>`, default ``0``.  The index in *text*
     at which to start searching.
   :parameter #key end: An :drm:`<integer>`, default ``*text*.size``.  The index
     at which to end the search.
   :parameter #key case-sensitive: A :drm:`<boolean>`, default ``#t``.
   :value match: An instance of ``false-or(<regex-match>)``.  ``#f`` is returned
     if no match was found.

   A match will only be found if it fits entirely within the range
   specified by *start* and *end*.

.. generic-function:: regex-search-strings
   :sealed:

      Find all matches for a regular expression within a string.

   :signature: regex-search-strings *pattern* *text* #key *anchored* *start* *end* *case-sensitive* => #rest *strings*

   :parameter pattern: An instance of :class:`<regex>`.
   :parameter text: An instance of :drm:`<string>`.
   :parameter #key anchored: An instance of :drm:`<boolean>`.
   :parameter #key start: An :drm:`<integer>`, default ``0``.  The index in *text*
     at which to start searching.
   :parameter #key end: An :drm:`<integer>`, default ``*text*.size``.  The index
     at which to end the search.
   :parameter #key case-sensitive: A :drm:`<boolean>`, default ``#t``.
   :value #rest strings: An instance of :drm:`<object>`.

   A match will only be found if it fits entirely within the range
   specified by *start* and *end*.

.. generic-function:: match-group
   :sealed:

      Return information about a specific match group in a :class:`<regex-match>`.

   :signature: match-group *match* *group* => *text* *start-index* *end-index*

   :parameter match: An instance of :class:`<regex-match>`.
   :parameter group: An instance of :drm:`<string>` or :drm:`<integer>`.
   :value text: An instance of ``false-or(<string>)``.
   :value start-index: An instance of ``false-or(<integer>)``.
   :value end-index: An instance of ``false-or(<integer>)``.
   :conditions: :class:`<invalid-match-group>` is signalled if ``group``
     does not name a valid group.

   The requested group may be an :drm:`<integer>` to access groups by
   number, or a :drm:`<string>` to access groups by name.  Accessing
   groups by name only works if they were given names in the compiled
   regular expression via the ``(?<foo>...)`` syntax.

   Group 0 is always the entire regular expression match.

   It is possible for the group identifier to be valid and for ``#f``
   to be returned.  This can happen, for example, if the group was in
   the part of an ``|`` (or) expression that didn't match.
