The regular-expressions Module
------------------------------

.. current-library:: regular-expressions
.. current-module:: regular-expressions

Overview
========

The Regular-expressions library exports the Regular-expressions module,
which contains various functions that deal with regular expressions
(regex). The module is based on Perl (version 4), and has the same
semantics unless otherwise noted. The syntax for Perl-style regular
expressions can be found on page 103 of Programming Perl by Larry Wall
and Randal L. Schwartz. There are some differences in the way this
library handles regular expressions. The biggest difference is that
regular expressions in Dylan are case insensitive by default. Also,
when given an unparsable regex, this library will produce undefined
behavior while Perl would give an error message.

A regular expression that is grammatically correct may still be illegal
if it contains an infinitely quantified sub-regex that may match the
empty string. That is, if R is a regex that can match the empty string,
then any regex containing R*, R+, and R{n,} is illegal. In this case,
the Regular-expressions library will signal an :class:`<invalid-regex>`
error when the regex is parsed. Note: Perl also has this restriction,
although it isn’t mentioned in Programming Perl.

Reference
=========

.. generic-function:: compile-regex

   :signature: compile-regex *pattern* #key *case-sensitive* *verbose* *multi-line* *dot-matches-all* *use-cache* => *regex*

   :parameter pattern: An instance of ``<string>``.
   :parameter #key case-sensitive: An instance of ``<boolean>``.
   :parameter #key verbose: An instance of ``<boolean>``.
   :parameter #key multi-line: An instance of ``<boolean>``.
   :parameter #key dot-matches-all: An instance of ``<boolean>``.
   :parameter #key use-cache: An instance of ``<boolean>``.
   :value regex: An instance of :class:`<regex>`.

.. generic-function:: group-end

   :signature: group-end *object* => #rest *results*

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: group-start

   :signature: group-start *object* => #rest *results*

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: group-text

   :signature: group-text *object* => #rest *results*

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: groups-by-name

   :signature: groups-by-name *object* => #rest *results*

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: groups-by-position

   :signature: groups-by-position *object* => #rest *results*

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. class:: <invalid-match-group>

   :superclasses: :class:`<regex-error>`


.. class:: <invalid-regex>

   :superclasses: :class:`<regex-error>`

   :keyword pattern:

.. class:: <match-group>

   :superclasses: <object>

   :keyword end:
   :keyword start:
   :keyword text:

.. generic-function:: match-group

   :signature: match-group *match* *group* => *text* *start-index* *end-index*

   :parameter match: An instance of :class:`<regex-match>`.
   :parameter group: An instance of ``<object>``.
   :value text: An instance of ``false-or(<string>)``.
   :value start-index: An instance of ``false-or(<integer>)``.
   :value end-index: An instance of ``false-or(<integer>)``.

.. class:: <regex>

   :superclasses: <mark>:regex-implementation

   :keyword group-count:
   :keyword group-number-to-name:
   :keyword pattern:

.. class:: <regex-error>

   :superclasses: <format-string-condition>, <error>

.. generic-function:: regex-group-count

   :signature: regex-group-count *object* => #rest *results*

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. class:: <regex-match>

   :superclasses: <object>

   :keyword regular-expression:

.. generic-function:: regex-pattern

   :signature: regex-pattern *object* => #rest *results*

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: regex-position

   :signature: regex-position *pattern* *text* #key *start* *end* *case-sensitive* => *regex-start*, #rest *marks*

   :parameter pattern: An instance of :class:`<regex>`.
   :parameter text: An instance of ``<string>``.
   :parameter #key start: An instance of ``<integer>``.
   :parameter #key end: An instance of ``<integer>``.
   :parameter #key case-sensitive: An instance of ``<boolean>``.
   :value regex-start: An instance of ``false-or(<integer>)``.
   :value #rest marks: An instance of ``<object>``.

.. generic-function:: regex-replace

   :signature: regex-replace *big* *pattern* *replacement* #key *start* *end* *count* *case-sensitive* => *new-string*

   :parameter big: An instance of ``<string>``.
   :parameter pattern: An instance of :class:`<regex>`.
   :parameter replacement: An instance of ``type-union(<string>, <function>)``.
   :parameter #key start: An instance of ``<integer>``.
   :parameter #key end: An instance of ``<integer>``.
   :parameter #key count: An instance of ``false-or(<integer>)``.
   :parameter #key case-sensitive: An instance of ``<boolean>``.
   :value new-string: An instance of ``<string>``.

.. generic-function:: regex-search

   :signature: regex-search *pattern* *text* #key *anchored* *start* *end* *case-sensitive* => *match*

   :parameter pattern: An instance of :class:`<regex>`.
   :parameter text: An instance of ``<string>``.
   :parameter #key anchored: An instance of ``<boolean>``.
   :parameter #key start: An instance of ``<integer>``.
   :parameter #key end: An instance of ``<integer>``.
   :parameter #key case-sensitive: An instance of ``<boolean>``.
   :value match: An instance of ``false-or(<regex-match>)``.

.. generic-function:: regex-search-strings

   :signature: regex-search-strings *pattern* *text* #key *anchored* *start* *end* *case-sensitive* => #rest *strings*

   :parameter pattern: An instance of :class:`<regex>`.
   :parameter text: An instance of ``<string>``.
   :parameter #key anchored: An instance of ``<boolean>``.
   :parameter #key start: An instance of ``<integer>``.
   :parameter #key end: An instance of ``<integer>``.
   :parameter #key case-sensitive: An instance of ``<boolean>``.
   :value #rest strings: An instance of ``<object>``.

