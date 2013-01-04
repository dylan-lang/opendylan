*******************
The STRINGS library
*******************

.. current-library:: strings
.. current-module:: strings

The strings library exports definitions for basic string manipulation.

.. note::

  * This library does not address any higher-level operations such as
    text formatting or anything that requires semantic knowledge of
    words, such as *pluralize*.

  * Where it makes sense, functions can be applied to either a single
    character or a string of characters.  For example, ``lowercase('C')
    => 'c'`` and ``lowercase("Foo") => "foo"``.

  * Functions are case-sensitive by default.  Versions that ignore
    alphabetic case are named with a trailing "-ic" or "-ic?".

  * Open Dylan doesn't yet support Unicode.  When it does, this library
    will be updated to support it also.

The strings library was originally defined in `DEP-0004
<http://opendylan.org/proposals/dep-0004.html>`_.  Some additional
background material can be found there.


.. contents::  Contents
   :local:


The STRINGS module
==================

Character Class Predicates
--------------------------

.. generic-function:: alphabetic?
   :sealed:

   Return ``#t`` if the argument is alphabetic, else ``#f``.

   :signature: alphabetic? (string-or-character, #key) => (alphabetic?)
   :parameter string-or-character: An instance of ``type-union(<string>, <character>)``.
   :value alphabetic?: An instance of ``<boolean>``.

.. method:: alphabetic?
   :specializer: <character>
   :sealed:

   Returns ``#t`` if the given character is a member of the set a-z or
   A-Z.  Otherwise returns ``#f``.

   :signature: alphabetic? (character) => (alphabetic?)
   :parameter character: An instance of ``<character>``.
   :value alphabetic?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     alphabetic?('a') => #t
     alphabetic?('-') => #f
   
.. method:: alphabetic?
   :specializer: <string>
   :sealed:

   Returns ``#t`` if every character in the string is a member of the
   set a-z or A-Z.  Otherwise returns ``#f``.

   :signature: alphabetic? (string, #key start, end) => (alphabetic?)
   :parameter string: An instance of ``<string>``.
   :parameter #key start: Index into ``string`` at which to start
     the comparison.  An instance of ``<integer>``, default 0.
   :parameter #key end: Index into ``string`` at which to stop
     the comparison.  An instance of ``<integer>``, default ``string.size``.
   :value alphabetic?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     alphabetic?("abc") => #t
     alphabetic?("abc123") => #f
     alphabetic?("abc123", end: 3) => #t
   
------------

.. generic-function:: alphanumeric?
   :sealed:

   Returns ``#t`` if the argument is alphabnumeric, otherwise ``#f``.

   :signature: alphanumeric? (string-or-character, #key) => (alphanumeric?)
   :parameter string-or-character: An instance of ``type-union(<string>, <character>)``.
   :value alphanumeric?: An instance of ``<boolean>``.

.. method:: alphanumeric?
   :specializer: <character>
   :sealed:

   Returns ``#t`` if the argument is a member of the set of characters
   a-z, A-Z, or 0-9, otherwise ``#f``.

   :signature: alphanumeric? (character) => (alphanumeric?)
   :parameter character: An instance of ``<character>``.
   :value alphanumeric?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     alphanumeric?('Z') => #t
     alphanumeric?('9') => #t
     alphanumeric?('*') => #f

.. method:: alphanumeric?
   :specializer: <string>
   :sealed:

   Returns ``#t`` if every character in the string is a member of the
   set a-z, A-Z, or 0-9, otherwise ``#f``.

   :signature: alphanumeric? (string) => (alphanumeric?)
   :parameter string: An instance of ``<string>``.
   :value alphanumeric?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     alphanumeric?("abc123") => #t
     alphanumeric?("abc...") => #f
     alphanumeric?("abc...",  end: 3) => #t

------------

.. generic-function:: control?
   :sealed:

   Returns ``#t`` if the argument is entirely composed of control
   characters, otherwise ``#f``.

   :signature: control? (string-or-character, #key) => (control?)
   :parameter string-or-character: An instance of ``type-union(<string>, <character>)``.
   :value control?: An instance of ``<boolean>``.

.. method:: control?
   :specializer: <character>
   :sealed:

   Returns ``#t`` if the argument is not a graphic or whitespace
   character, otherwise ``#f``.

   :signature: control? (character) => (control?)
   :parameter character: An instance of ``<character>``.
   :value control?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     control?('a') => #f
     control?('\0') => #t

.. method:: control?
   :specializer: <string>
   :sealed:

   Returns ``#t`` if the argument is entirely composed of non-graphic,
   non-whitespace characters.

   :signature: control? (string) => (control?)
   :parameter string: An instance of ``<string>``.
   :parameter #key start: Index into ``string`` at which to start
     the comparison.  An instance of ``<integer>``, default 0.
   :parameter #key end: Index into ``string`` at which to stop
     the comparison.  An instance of ``<integer>``, default ``string.size``.
   :value control?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     control?("\0\a\b") => #t
     control?("abc\0") => #f
     control?("abc\0", start: 3) => #t

------------

.. generic-function:: graphic?
   :sealed:

   Returns ``#t`` if the argument is entirely composed of
   graphic characters.

   :signature: graphic? (string-or-character, #key) => (graphic?)
   :parameter string-or-character: An instance of ``type-union(<string>, <character>)``.
   :value graphic?: An instance of ``<boolean>``.

.. method:: graphic?
   :specializer: <character>
   :sealed:

   Returns ``#t`` if the argument is a graphic character, defined as
   those with character codes between 32 (Space) and 126 (~) in the US
   ASCII character set.

   :signature: graphic? (character, #key) => (graphic?)
   :parameter character: An instance of ``<character>``.
   :value graphic?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     graphic?('a') => #t
     graphic?('\b') => #f

.. method:: graphic?
   :specializer: <string>
   :sealed:

   Returns ``#t`` if the argument is entirely composed of graphic
   characters, defined as those with character codes between 32
   (Space) and 126 (~).

   :signature: graphic? (string, #key) => (graphic?)
   :parameter string: An instance of ``<string>``.
   :parameter #key start: Index into ``string`` at which to start
     the comparison.  An instance of ``<integer>``, default 0.
   :parameter #key end: Index into ``string`` at which to stop
     the comparison.  An instance of ``<integer>``, default ``string.size``.
   :value graphic?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     graphic?("ABC") => #t
     graphic?("ABC\n") => #f
     graphic?("ABC\n", end: 3) => #t

------------

.. generic-function:: printable?
   :sealed:

   Returns ``#t`` if the argument is entirely composed of printable
   characters, defined as either a graphic or whitespace character.

   :signature: printable? (string-or-character, #key) => (printable?)
   :parameter string-or-character: An instance of ``type-union(<string>, <character>)``.
   :value printable?: An instance of ``<boolean>``.

.. method:: printable?
   :specializer: <character>
   :sealed:

   Returns ``#t`` if the argument is a printable character, defined as
   either a graphic or whitespace character.  Otherwise ``#f`` is
   returned.

   :signature: printable? (character, #key) => (printable?)
   :parameter character: An instance of ``<character>``.
   :value printable?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     printable?('x') => #t
     printable?('\t') => #t
     printable?('\0') => #f

.. method:: printable?
   :specializer: <string>
   :sealed:

   Returns ``#t`` if the argument is entirely composed of printable
   characters, defined as either a graphic or whitespace character.
   Otherwise ``#f`` is returned.

   :signature: printable? (string, #key) => (printable?)
   :parameter string: An instance of ``<string>``.
   :parameter #key start: Index into ``string`` at which to start
     the comparison.  An instance of ``<integer>``, default 0.
   :parameter #key end: Index into ``string`` at which to stop
     the comparison.  An instance of ``<integer>``, default ``string.size``.
   :value printable?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     printable?("a b c") => #t
     printable?("abc\0") => #f
     printable?("abc\0", end: 3) => #t

------------

.. generic-function:: whitespace?
   :sealed:

   Returns ``#t`` if the argument is entirely composed of whitespace
   characters.

   :signature: whitespace? (string-or-character, #key) => (whitespace?)
   :parameter string-or-character: An instance of ``type-union(<string>, <character>)``.
   :value whitespace?: An instance of ``<boolean>``.

.. method:: whitespace?
   :specializer: <character>
   :sealed:

   Returns ``#t`` if the argument is ' ' (Space), '\\t' (Tab), '\\n'
   (Newline), '\\f' (Formfeed), or '\\r' (Return).  Otherwise ``#f`` is
   returned.

   :signature: whitespace? (character, #key) => (whitespace?)
   :parameter character: An instance of ``<character>``.
   :value whitespace?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     whitespace?(' ') => #t
     whitespace?('\r') => #t
     whitespace?('x') => #f

.. method:: whitespace?
   :specializer: <string>
   :sealed:

   Returns ``#t`` if the argument is entirely composed of whitespace
   characters, defined as ' ' (Space), '\\t' (Tab), '\\n' (Newline),
   '\\f' (Formfeed), or '\\r' (Return).  Otherwise ``#f`` is returned.

   :signature: whitespace? (string, #key) => (whitespace?)
   :parameter string: An instance of ``<string>``.
   :parameter #key start: Index into ``string`` at which to start
     the comparison.  An instance of ``<integer>``, default 0.
   :parameter #key end: Index into ``string`` at which to stop
     the comparison.  An instance of ``<integer>``, default ``string.size``.
   :value whitespace?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     whitespace?("x\t x") => #f
     whitespace?("x\t x", start: 1, end: 3) => #t

------------

.. generic-function:: decimal-digit?
   :sealed:

   Returns ``#t`` if the argument is a decimal digit, otherwise ``#f``.

   :signature: decimal-digit? (string-or-character, #key) => (decimal-digit?)
   :parameter string-or-character: An instance of ``type-union(<string>, <character>)``.
   :value decimal-digit?: An instance of ``<boolean>``.

.. method:: decimal-digit?
   :specializer: <character>
   :sealed:

   Returns ``#t`` if the character is a member of the set [0-9],
   otherwise ``#f`` is returned.

   :signature: decimal-digit? (character, #key) => (decimal-digit?)
   :parameter character: An instance of ``<character>``.
   :value decimal-digit?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     decimal-digit?('a') => #f
     decimal-digit?('4') => #t

.. method:: decimal-digit?
   :specializer: <string>
   :sealed:

   Returns ``#t`` if every character in the string is a member of the
   set [0-9], otherwise ``#f`` is returned.

   :signature: decimal-digit? (string, #key) => (decimal-digit?)
   :parameter string: An instance of ``<string>``.
   :parameter #key start: Index into ``string`` at which to start
     the comparison.  An instance of ``<integer>``, default 0.
   :parameter #key end: Index into ``string`` at which to stop
     the comparison.  An instance of ``<integer>``, default ``string.size``.
   :value decimal-digit?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     decimal-digit?("123") => #t
     decimal-digit?("x123y") => #f
     decimal-digit?("x123y", start: 1, end: 4) => #t

------------

.. generic-function:: hexadecimal-digit?
   :sealed:

   Returns ``#t`` if the argument is entirely composed of hexadecimal
   digits, otherwise ``#f`` is returned.

   :signature: hexadecimal-digit? (string-or-character, #key) => (hexadecimal-digit?)
   :parameter string-or-character: An instance of ``type-union(<string>, <character>)``.
   :value hexadecimal-digit?: An instance of ``<boolean>``.

.. method:: hexadecimal-digit?
   :specializer: <character>
   :sealed:

   Returns ``#t`` if the character is a member of the set [0-9a-fA-F],
   otherwise ``#f`` is returned.

   :signature: hexadecimal-digit? (character, #key) => (hexadecimal-digit?)
   :parameter character: An instance of ``<character>``.
   :value hexadecimal-digit?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     hexadecimal-digit?('a') => #t
     hexadecimal-digit?('g') => #f
     hexadecimal-digit?('0') => #t

.. method:: hexadecimal-digit?
   :specializer: <string>
   :sealed:

   Returns ``#t`` if every character in the string is a member of the
   set [0-9a-fA-F], otherwise ``#f`` is returned.

   :signature: hexadecimal-digit? (string, #key) => (hexadecimal-digit?)
   :parameter string: An instance of ``<string>``.
   :parameter #key start: Index into ``string`` at which to start
     the comparison.  An instance of ``<integer>``, default 0.
   :parameter #key end: Index into ``string`` at which to stop
     the comparison.  An instance of ``<integer>``, default ``string.size``.
   :value hexadecimal-digit?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     hexdecimal-digit?("ff00") => #t
     hexdecimal-digit?(" ff00 ") => #f
     hexdecimal-digit?(" ff00 ", start: 1, end: 5) => #t

------------

.. generic-function:: octal-digit?
   :sealed:

   Returns ``#t`` if the argument is entirely composed of octal
   digits, otherwise ``#f`` is returned.

   :signature: octal-digit? (string-or-character, #key) => (octal-digit?)
   :parameter string-or-character: An instance of ``type-union(<string>, <character>)``.
   :value octal-digit?: An instance of ``<boolean>``.

.. method:: octal-digit?
   :specializer: <character>
   :sealed:

   Returns ``#t`` if the character is a member of the set [0-9a-fA-F],
   otherwise ``#f`` is returned.

   :signature: octal-digit? (character, #key) => (octal-digit?)
   :parameter character: An instance of ``<character>``.
   :value octal-digit?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     octal-digit?('7') => #t
     octal-digit?('0') => #t
     octal-digit?('8') => #f

.. method:: octal-digit?
   :specializer: <string>
   :sealed:

   Returns ``#t`` if every character in the string is a member of the
   set [0-9a-fA-F], otherwise ``#f`` is returned.

   :signature: octal-digit? (string, #key) => (octal-digit?)
   :parameter string: An instance of ``<string>``.
   :parameter #key start: Index into ``string`` at which to start
     the comparison.  An instance of ``<integer>``, default 0.
   :parameter #key end: Index into ``string`` at which to stop
     the comparison.  An instance of ``<integer>``, default ``string.size``.
   :value octal-digit?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     octal-digit?("700") => #t
     octal-digit?("7008") => #f
     octal-digit?("7008", end: 3) => #t


Substring Functions
-------------------

.. generic-function:: count-substrings
   :sealed:

   Count how many times a substring pattern occurs in a larger string.

   :signature: count-substrings (big pattern #key start end ignore-case?) => (count)
   :parameter big: An instance of ``<string>``.  The string in which to search.
   :parameter pattern: An instance of ``<string>``.  The substring to search for.
   :parameter #key start: An instance of ``<integer>``, default 0.  Where to start searching.
   :parameter #key end: An instance of ``<integer>``, default ``big.size``.
     Where to stop searching.  Note that if ``pattern``
     is not completely between the bounds of ``start`` (inclusive) and
     ``end`` (exclusive) it will not be counted.
   :parameter #key ignore-case?: An instance of ``<boolean>``, default ``#f``.
   :value count: An instance of ``<integer>``.
   :example:

   .. code-block:: dylan

     count-substrings("", "") => 1
     count-substrings("xxxxxx", "xx", end: 5) => 2  // no overlap
     count-substrings("xXx", "x", ignore-case?: #t) => 3

.. generic-function:: find-substring
   :sealed:

   Find the index of a substring pattern in a larger string.  Returns
   ``#f`` if not found.

   :signature: find-substring (big pattern #key start end ignore-case?) => (index)
   :parameter big: An instance of ``<string>``.  The string in which to search.
   :parameter pattern: An instance of ``<string>``.  The substring to search for.
   :parameter #key start: An instance of ``<integer>``, default 0.  Where to start searching.
   :parameter #key end: An instance of ``<integer>``, default ``big.size``.
     Where to stop searching.  Note that if ``pattern``
     is not completely between the bounds of ``start`` (inclusive) and
     ``end`` (exclusive) it will not match.
   :parameter #key ignore-case?: An instance of ``<boolean>``, default ``#f``.
   :value index: An instance of ``false-or(<integer>)``.
   :example:

   .. code-block:: dylan

     find-substring("My dog has fleas.", "dog") => 3

.. generic-function:: replace-substrings
   :sealed:

   Replace a substring pattern in a larger string.  Allocates a new
   string for the return value if any replacements are done.  If there
   are no replacements the implementation may return ``big`` unmodified.

   :signature: replace-substrings (big pattern replacement #key count start end ignore-case?) => (new-string)
   :parameter big: An instance of ``<string>``.  The string in which
     to search.
   :parameter pattern: An instance of ``<string>``.  The substring
     pattern to search for.
   :parameter replacement: An instance of ``<string>``.  The string
     with which to replace ``pattern``.
   :parameter #key count: An instance of ``false-or(<integer>)``.  The
     number of occurrences to replace.  The default is ``#f``, meaning to
     replace all.  Replacements are performed from left to right
     within ``big`` until ``count`` has been reached.
   :parameter #key start: An instance of ``<integer>``, default 0.  Where to
     start searching.
   :parameter #key end: An instance of ``<integer>``, default
     ``big.size``.  Where to stop searching.  Note that if ``pattern``
     is not completely between the bounds of ``start`` (inclusive) and
     ``end`` (exclusive) it will not be replaced.
   :parameter #key ignore-case?: An instance of ``<boolean>``, default ``#f``.
   :value new-string: An instance of ``<string>``.
   :example:

   .. code-block:: dylan

     replace-substrings("My cat and your cat", "cat", "dog")
       => "My dog and your dog"

Case Conversion Functions
-------------------------

.. generic-function:: lowercase
   :sealed:

   Returns a lowercased version of its argument.

   :signature: lowercase (string-or-character) => (new-string-or-character)
   :parameter string-or-character: An instance of ``type-union(<string>, <character>)``.
   :value new-string-or-character: An instance of ``type-union(<string>, <character>)``.

.. method:: lowercase
   :specializer: <character>
   :sealed:

   If the given character is alphabetic, its lowercase equivalent is returned.
   Otherwise the character itself is returned.

   :signature: lowercase (character) => (new-character)
   :parameter character: An instance of ``<character>``.
   :value lowercase-character: An instance of ``<character>``.
   :example:

   .. code-block:: dylan

     lowercase('A') => 'a'
     lowercase('#') => '#'

.. method:: lowercase
   :specializer: <string>
   :sealed:

   Returns a newly allocated string with all uppercase characters
   converted to lowercase.  The implementation may return the given
   string unchanged if it contains no uppercase characters.

   :signature: lowercase (string) => (lowercase-string)
   :parameter string: An instance of ``<string>``.
   :parameter #key start: An instance of ``<integer>``, default 0.  The index
     at which to start lowercasing.
   :parameter #key end: An instance of ``<integer>``, default
     ``string.size``.  The index before which to stop lowercasing.
   :value lowercase-string: An instance of ``<string>``.
   :example:

   .. code-block:: dylan

     lowercase("Hack Dylan!") => "hack dylan!"
     lowercase("Hack Dylan!", end: 4) => "hack"

-------------

.. generic-function:: lowercase!
   :sealed:

   :signature: lowercase! (string-or-character) => (new-string-or-character)
   :parameter string-or-character: An instance of ``type-union(<string>, <character>)``.
   :value new-string-or-character: An instance of ``type-union(<string>, <character>)``.

.. method:: lowercase!
   :specializer: <character>
   :sealed:

   If the given character is alphabetic, its lowercase equivalent is
   returned.  Otherwise the character is returned unchanged.  This
   operation is not a mutation; this method is provided for symmetry
   with :meth:`lowercase(<character>)`.

   :signature: lowercase! (character) => (new-character)
   :parameter character: An instance of ``<character>``.
   :value lowercase-character: An instance of ``<character>``.
   :example:

   .. code-block:: dylan

     lowercase!('A') => 'a'
     lowercase!('#') => '#'

.. method:: lowercase!
   :specializer: <string>
   :sealed:

   Mutates the given string such that all uppercase characters are
   converted to lowercase.

   :signature: lowercase! (string) => (string)
   :parameter string: An instance of ``<string>``.
   :parameter #key start: An instance of ``<integer>``, default 0.  The index
     at which to start lowercasing.
   :parameter #key end: An instance of ``<integer>``, default
     ``string.size``.  The index before which to stop lowercasing.
   :value lowercase-string: An instance of ``<string>``.
   :example:

   .. code-block:: dylan

     let text = concatenate("Hack", "Dylan!");
     lowercase!(text);
       => "hackdylan!"
     text;
       => "hackdylan!"
     lowercase!("Hack Dylan!")
       => error, attempt to modify a string constant

-------------

.. generic-function:: lowercase?
   :sealed:

   Returns ``#t`` if the argument is entirely composed of
   non-uppercase characters.

   :signature: lowercase? (string-or-character) => (is-lowercase?)
   :parameter string-or-character: An instance of ``type-union(<string>, <character>)``.
   :value is-lowercase?: An instance of ``<boolean>``.

.. method:: lowercase?
   :specializer: <character>
   :sealed:

   Returns ``#t`` if the given character is not an uppercase alphabetic.
   Otherwise ``#f`` is returned.

   :signature: lowercase? (character) => (is-lowercase?)
   :parameter character: An instance of ``<character>``.
   :value is-lowercase?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     lowercase?('n') => #t
     lowercase?('N') => #f
     lowercase?('*') => #t

.. method:: lowercase?
   :specializer: <string>
   :sealed:

   Returns ``#t`` if the argument does not contain any uppercase
   alphabetic characters.  Otherwise ``#f`` is returned.

   :signature: lowercase? (string) => (is-lowercase?)
   :parameter string: An instance of ``<string>``.
   :parameter #key start: An instance of ``<integer>``, default 0.  The index
     at which to start checking.
   :parameter #key end: An instance of ``<integer>``, default
     ``string.size``.  The index before which to stop checking.
   :value is-lowercase?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     lowercase?("Why me?") => #f
     lowercase?("Why me?", start: 1) => #t
     lowercase?("e.e. cummings") => #t

-------------

.. generic-function:: uppercase
   :sealed:

   Returns an uppercased version of its argument.

   :signature: uppercase (string-or-character) => (new-string-or-character)
   :parameter string-or-character: An instance of ``type-union(<string>, <character>)``.
   :value new-string-or-character: An instance of ``type-union(<string>, <character>)``.

.. method:: uppercase
   :specializer: <character>
   :sealed:

   If the given character is alphabetic, its uppercase equivalent is returned.
   Otherwise the character itself is returned.

   :signature: uppercase (character) => (new-character)
   :parameter character: An instance of ``<character>``.
   :value uppercase-character: An instance of ``<character>``.
   :example:

   .. code-block:: dylan

     uppercase('x') => 'X'
     uppercase('*') => '*'

.. method:: uppercase
   :specializer: <string>
   :sealed:

   Returns a newly allocated string with all lowercase alphabetic
   characters converted to uppercase.  The implementation may return
   the original string unchanged if it contains no lowercase characters.

   :signature: uppercase (string) => (uppercase-string)
   :parameter string: An instance of ``<string>``.
   :parameter #key start: An instance of ``<integer>``, default 0.  The index
     at which to start uppercasing.
   :parameter #key end: An instance of ``<integer>``, default
     ``string.size``.  The index before which to stop uppercasing.
   :value uppercase-string: An instance of ``<string>``.
   :example:

   .. code-block:: dylan

     uppercase("Hack Dylan!") => "HACK DYLAN!"
     uppercase("Hack Dylan!", end: 4) => "HACK"

-------------

.. generic-function:: uppercase!
   :sealed:

   :signature: uppercase! (string-or-character) => (new-string-or-character)
   :parameter string-or-character: An instance of ``type-union(<string>, <character>)``.
   :value new-string-or-character: An instance of ``type-union(<string>, <character>)``.

.. method:: uppercase!
   :specializer: <character>
   :sealed:

   If the given character is alphabetic, its uppercase equivalent is
   returned.  Otherwise the character is returned unchanged.  This
   operation is not a mutation, but the method is provided for symmetry
   with :meth:`uppercase(<character>)`.

   :signature: uppercase! (character) => (uppercase-character)
   :parameter character: An instance of ``<character>``.
   :value uppercase-character: An instance of ``<character>``.
   :example:

   .. code-block:: dylan

     uppercase!('t') => 'T'
     
.. method:: uppercase!
   :specializer: <string>
   :sealed:

   Mutates the given string such that all lowercase characters are
   converted to uppercase.

   :signature: uppercase! (string) => (uppercase-string)
   :parameter string: An instance of ``<string>``.
   :parameter #key start: An instance of ``<integer>``, default 0.  The index
     at which to start uppercasing.
   :parameter #key end: An instance of ``<integer>``, default
     ``string.size``.  The index before which to stop uppercasing.
   :value uppercase-string: An instance of ``<string>``.
   :example:

   .. code-block:: dylan

     let text = concatenate("Hack", "Dylan!");
     uppercase!(text);
       => "HACKDYLAN!"
     text;
       => "HACKDYLAN!"
     uppercase!("Hack Dylan!")
       => error, attempt to modify a string constant

-------------

.. generic-function:: uppercase?
   :sealed:

   Returns ``#t`` if the argument is entirely composed of
   non-lowercase characters.

   :signature: uppercase? (string-or-character) => (is-uppercase?)
   :parameter string-or-character: An instance of ``type-union(<string>, <character>)``.
   :value is-uppercase?: An instance of ``<boolean>``.

.. method:: uppercase?
   :specializer: <character>
   :sealed:

   Returns ``#t`` if the given character is not a lowercase alphabetic.
   Otherwise ``#f`` is returned.

   :signature: uppercase? (character) => (is-uppercase?)
   :parameter character: An instance of ``<character>``.
   :value is-uppercase?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     uppercase?('T') => #t
     uppercase?('t') => #f
     uppercase?('^') => #t

.. method:: uppercase?
   :specializer: <string>
   :sealed:

   Returns ``#t`` if the argument does not contain any lowercase
   alphabetic characters.  Otherwise ``#f`` is returned.

   :signature: uppercase? (string) => (is-uppercase?)
   :parameter string: An instance of ``<string>``.
   :parameter #key start: An instance of ``<integer>``, default 0.  The index
     at which to start checking.
   :parameter #key end: An instance of ``<integer>``, default
     ``string.size``.  The index before which to stop checking.
   :value is-uppercase?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     uppercase?("AbC") => #f
     uppercase?("ABC") => #t

Comparison Functions
--------------------

Case insensitive character comparison functions are provided for
convenience.  (See `DEP-0004
<http://opendylan.org/proposals/dep-0004.html>`_ for discussion.)

.. function:: char-compare

   Returns -1 if char1 < char2, 0 if char1 = char2, and 1 if char1 >
   char2, using *case sensitive* comparison.

   :signature: char-compare (char1 char2) => (result)
   :parameter char1: An instance of ``<character>``.
   :parameter char2: An instance of ``<character>``.
   :value result: An instance of ``one-of(-1, 0, 1)``.
   :example:

   .. code-block:: dylan

     char-compare('a', 'b') => -1
     char-compare('a', 'a') => 0
     char-compare('b', 'a') => 1
     char-compare('a', 'B') => 1

.. function:: char-compare-ic

   Returns -1 if char1 < char2, 0 if char1 = char2, and 1 if char1 >
   char2, using *case insensitive* comparison.

   :signature: char-compare-ic (char1 char2) => (result)
   :parameter char1: An instance of ``<character>``.
   :parameter char2: An instance of ``<character>``.
   :value result: An instance of ``one-of(-1, 0, 1)``.
   :example:

   .. code-block:: dylan

     char-compare-ic('a', 'b') => -1
     char-compare-ic('a', 'a') => 0
     char-compare-ic('b', 'a') => 1
     char-compare-ic('a', 'B') => -1

.. function:: char-equal-ic?

   Returns ``#t`` if char1 and char2 are the same, *ignoring case*.
   Otherwise ``#f`` is returned.

   :signature: char-equal-ic? (char1 char2) => (equal?)
   :parameter char1: An instance of ``<character>``.
   :parameter char2: An instance of ``<character>``.
   :value equal?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     char-equal-ic?('a', 'A') => #t

.. generic-function:: string-compare
   :sealed:

   Returns -1 if string1 < string2, 0 if string1 and string2 are the
   same, and 1 if string1 > string2, using *case sensitive* comparison.

   :signature: string-compare (string1 string2 #key start1 end1 start2 end2 test) => (result)
   :parameter string1: An instance of ``<string>``.
   :parameter string2: An instance of ``<string>``.
   :parameter #key start1: An instance of ``<integer>``, default 0.  The index in
     ``string1`` at which to start the comparison.
   :parameter #key end1: An instance of ``<integer>``, default ``string1.size``.
     The index in ``string1`` before which to stop the comparison.
   :parameter #key start2: An instance of ``<integer>``, default 0.  The index in
     ``string2`` at which to start the comparison.
   :parameter #key end2: An instance of ``<integer>``, default ``string2.size``.
     The index in ``string2`` before which to stop the comparison.
   :parameter #key test: An instance of ``<function>``, default ``char-compare``.
   :value result: An instance of ``one-of(-1, 0, 1)``.
   :example:

   .. code-block:: dylan

     string-compare("abc", "abc") => 0
     string-compare("the", "them") => -1
     string-compare("beer", "bee") => 1

.. generic-function:: string-equal?
   :sealed:

   Returns ``#t`` if string1 and string2 are of equal length and
   contain the same sequence of characters.  Otherwise returns ``#f``.

   :signature: string-equal? (string1 string2 #key start1 end1 start2 end2 test) => (equal?)
   :parameter string1: An instance of ``<string>``.
   :parameter string2: An instance of ``<string>``.
   :parameter #key start1: An instance of ``<integer>``, default 0.  The index in
     ``string1`` at which to start the comparison.
   :parameter #key end1: An instance of ``<integer>``, default ``string1.size``.
     The index in ``string1`` before which to stop the comparison.
   :parameter #key start2: An instance of ``<integer>``, default 0.  The index in
     ``string2`` at which to start the comparison.
   :parameter #key end2: An instance of ``<integer>``, default ``string2.size``.
     The index in ``string2`` before which to stop the comparison.
   :parameter #key test: An instance of ``<function>``, default ``char-compare``.
   :value equal?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     string-equal?("abc", "abc") => #t
     string-equal?("ABC", "abc") => #f
     string-equal?("the", "them") => #f
     string-equal?("the", "them", end2: 3) => #t


.. generic-function:: string-equal-ic?
   :sealed:

   Returns ``#t`` if string1 and string2 are of equal length and
   contain the same sequence of characters, ignoring case.  Otherwise
   returns ``#f``.

   :signature: string-equal-ic? (string1 string2 #key start1 end1 start2 end2) => (equal?)
   :parameter string1: An instance of ``<string>``.
   :parameter string2: An instance of ``<string>``.
   :parameter #key start1: An instance of ``<integer>``, default 0.  The index in
     ``string1`` at which to start the comparison.
   :parameter #key end1: An instance of ``<integer>``, default ``string1.size``.
     The index in ``string1`` before which to stop the comparison.
   :parameter #key start2: An instance of ``<integer>``, default 0.  The index in
     ``string2`` at which to start the comparison.
   :parameter #key end2: An instance of ``<integer>``, default ``string2.size``.
     The index in ``string2`` before which to stop the comparison.
   :value equal?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     string-equal-ic?("ABC", "abc") => #t
     string-equal-ic?("the", "them") => #f
     string-equal-ic?("The", "them", end2: 3) => #t

.. generic-function:: string-greater?
   :sealed:

   Return ``#t`` if ``string1`` is greater than ``string2``, using
   *case sensitive* comparison.

   :signature: string-greater? (string1 string2 #key start1 end1 start2 end2 test) => (greater?)
   :parameter string1: An instance of ``<string>``.
   :parameter string2: An instance of ``<string>``.
   :parameter #key start1: An instance of ``<integer>``, default 0.  The index in
     ``string1`` at which to start the comparison.
   :parameter #key end1: An instance of ``<integer>``, default ``string1.size``.
     The index in ``string1`` before which to stop the comparison.
   :parameter #key start2: An instance of ``<integer>``, default 0.  The index in
     ``string2`` at which to start the comparison.
   :parameter #key end2: An instance of ``<integer>``, default ``string2.size``.
     The index in ``string2`` before which to stop the comparison.
   :parameter #key test: An instance of ``<function>``, default ``char-compare``.
   :value greater?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     string-greater?("dog", "cat") => #t
     string-greater?("Dog", "cat") => #f
     string-greater?("dogs", "dog") => #t

.. generic-function:: string-greater-ic?
   :sealed:

   Return ``#t`` if ``string1`` is greater than ``string2``, using
   *case insensitive* comparison.

   :signature: string-greater-ic? (string1 string2 #key start1 end1 start2 end2) => (greater?)
   :parameter string1: An instance of ``<string>``.
   :parameter string2: An instance of ``<string>``.
   :parameter #key start1: An instance of ``<integer>``, default 0.  The index in
     ``string1`` at which to start the comparison.
   :parameter #key end1: An instance of ``<integer>``, default ``string1.size``.
     The index in ``string1`` before which to stop the comparison.
   :parameter #key start2: An instance of ``<integer>``, default 0.  The index in
     ``string2`` at which to start the comparison.
   :parameter #key end2: An instance of ``<integer>``, default ``string2.size``.
     The index in ``string2`` before which to stop the comparison.
   :value greater?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     string-greater-ic?("dog", "cat") => #t
     string-greater-ic?("Dog", "cat") => #t
     string-greater-ic?("DOGS", "dog") => #t

.. generic-function:: string-less?
   :sealed:

   Return ``#t`` if ``string1`` is less than ``string2``, using
   *case sensitive* comparison.

   :signature: string-less? (string1 string2 #key start1 end1 start2 end2 test) => (less?)
   :parameter string1: An instance of ``<string>``.
   :parameter string2: An instance of ``<string>``.
   :parameter #key start1: An instance of ``<integer>``, default 0.  The index in
     ``string1`` at which to start the comparison.
   :parameter #key end1: An instance of ``<integer>``, default ``string1.size``.
     The index in ``string1`` before which to stop the comparison.
   :parameter #key start2: An instance of ``<integer>``, default 0.  The index in
     ``string2`` at which to start the comparison.
   :parameter #key end2: An instance of ``<integer>``, default ``string2.size``.
     The index in ``string2`` before which to stop the comparison.
   :parameter #key test: An instance of ``<function>``, default ``char-compare``.
   :value less?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     string-less?("dog", "cat") => #f
     string-less?("Dog", "cat") => #t
     string-less?("dogs", "dog") => #f

.. generic-function:: string-less-ic?
   :sealed:

   Return ``#t`` if ``string1`` is less than ``string2``, using
   *case insensitive* comparison.

   :signature: string-less-ic? (string1 string2 #key start1 end1 start2 end2) => (less?)
   :parameter string1: An instance of ``<string>``.
   :parameter string2: An instance of ``<string>``.
   :parameter #key start1: An instance of ``<integer>``, default 0.  The index in
     ``string1`` at which to start the comparison.
   :parameter #key end1: An instance of ``<integer>``, default ``string1.size``.
     The index in ``string1`` before which to stop the comparison.
   :parameter #key start2: An instance of ``<integer>``, default 0.  The index in
     ``string2`` at which to start the comparison.
   :parameter #key end2: An instance of ``<integer>``, default ``string2.size``.
     The index in ``string2`` before which to stop the comparison.
   :value less?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     string-less-ic?("cat", "dog") => #t
     string-less-ic?("cat", "Dog") => #t
     string-less-ic?("dog", "DOGS") => #t

.. generic-function:: starts-with?
   :sealed:

   Return ``#t`` if ``string1`` is starts with ``string2``, using
   *case sensitive* comparison.

   :signature: starts-with? (string pattern #key test) => (starts-with?)
   :parameter string: An instance of ``<string>``.
   :parameter pattern: An instance of ``<string>``.
   :parameter #key test: An instance of ``<function>``, default ``char-compare``.
     For *case insensitive* comparison pass ``char-compare-ic`` here.
   :value starts-with?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     starts-with?("Watermelon", "water") => #f
     starts-with?("Watermelon", "water", test: char-compare-ic) => #t

.. generic-function:: ends-with?
   :sealed:

   Return ``#t`` if ``string1`` is ends with ``string2``, using *case
   sensitive* comparison.

   :signature: ends-with? (string pattern #key test) => (ends-with?)
   :parameter string: An instance of ``<string>``.
   :parameter pattern: An instance of ``<string>``.
   :parameter #key test: An instance of ``<function>``, default ``char-compare``.
     For *case insensitive* comparison pass ``char-compare-ic`` here.
   :value ends-with?: An instance of ``<boolean>``.
   :example:

   .. code-block:: dylan

     ends-with?("Open Dylan", "dylan") => #f
     ends-with?("Open Dylan", "dylan", test: char-compare-ic) => #t


Miscellaneous Functions
-----------------------

.. generic-function:: pad
   :sealed:

   Add a character to *both sides* of a string until it reaches the
   given width.

   :signature: pad (string width #key fill) => (padded-string)
   :parameter string: An instance of ``<string>``.  The string to pad.
   :parameter width: An instance of ``<integer>``.  The final width of the result string.
   :parameter #key fill: An instance of ``<character>``.  The character to pad with.
   :value padded-string: An instance of ``<string>``.
   :example:

   .. code-block:: dylan

     pad("foo", 5) => " foo "
     pad("foo", 5, fill: '*') => "*foo*"

.. generic-function:: pad-left
   :sealed:

   Add a character to the left side of a string until it reaches the
   given width.

   :signature: pad-left (string width #key fill) => (padded-string)
   :parameter string: An instance of ``<string>``.  The string to pad.
   :parameter width: An instance of ``<integer>``.  The final width of the result string.
   :parameter #key fill: An instance of ``<character>``.  The character to pad with.
   :value padded-string: An instance of ``<string>``.
   :example:

   .. code-block:: dylan

     pad-left("foo", 5) => "  foo"
     pad-left("foo", 5, fill: '*') => "**foo"

.. generic-function:: pad-right
   :sealed:

   Add a character to the right side of a string until it reaches the
   given width.

   :signature: pad-right (string width #key fill) => (padded-string)
   :parameter string: An instance of ``<string>``.  The string to pad.
   :parameter width: An instance of ``<integer>``.  The final width of the result string.
   :parameter #key fill: An instance of ``<character>``.  The character to pad with.
   :value padded-string: An instance of ``<string>``.
   :example:

   .. code-block:: dylan

     pad-right("foo", 5) => "foo  "
     pad-right("foo", 5, fill: '*') => "foo**"

.. function:: split-lines

   Split a string on line boundaries, which may be CR alone, CRLF, or LF alone.

   :signature: split-lines (string #key remove-if-empty?) => (lines)
   :parameter string: An instance of ``<string>``.
   :parameter #key remove-if-empty?: An instance of ``<boolean>``.
     If true, the result will not contain any empty strings.
   :value lines: An instance of ``<sequence>``.
   :example:

   .. code-block:: dylan

     // Lines are separated by CR, CRLF, or LF, but not LFCR
     split-lines("aa\nbb\r\ncc\rdd\n\ree") => #["aa", "bb", "cc", "dd", "", "ee"]

     split-lines("\nXYZ\n") => #["", "XYZ", ""]

     // Remove empty lines...
     split-lines("abc\r\rdef", remove-if-empty?: #t) => #["abc", "def"]

   See also:  :func:`split`

.. generic-function:: strip
   :sealed:

   Remove characters (whitespace by default) from both sides of a string.

   :signature: strip (string #key test start end) => (new-string)
   :parameter string: An instance of ``<string>``.  The string to strip.
   :parameter #key test: An instance of ``<function>``.  A function that
     accepts a character and returns #t if the character should be
     removed and ``#f`` otherwise.
   :parameter #key start: An instance of ``<integer>``, default 0.  The
     index in ``string`` at which to start stripping.
   :parameter #key end: An instance of ``<integer>``, default ``string.size``.
     The index in ``string`` before which to stop stripping.
   :value new-string: An instance of ``<string>``.
   :example:

   .. code-block:: dylan

     strip(" \tabc\n") => "abc"
     strip("*foo*", test: curry(\=, '*')) => "foo"

.. generic-function:: strip-left
   :sealed:

   Remove characters (whitespace by default) from the beginning of a string.

   :signature: strip-left (string #key test start end) => (new-string)
   :parameter string: An instance of ``<string>``.  The string to strip.
   :parameter #key test: An instance of ``<function>``.  A function that
     accepts a character and returns #t if the character should be
     removed and ``#f`` otherwise.
   :parameter #key start: An instance of ``<integer>``, default 0.  The
     index in ``string`` at which to start stripping.
   :parameter #key end: An instance of ``<integer>``, default ``string.size``.
     The index in ``string`` before which to stop stripping.
   :value new-string: An instance of ``<string>``.
   :example:

   .. code-block:: dylan

     strip-left(" \tabc\n") => "abc\n"
     strip-left("*foo*", test: curry(\=, '*')) => "foo*"

.. generic-function:: strip-right
   :sealed:

   Remove characters (whitespace by default) from the end of a string.

   :signature: strip-right (string #key test start end) => (new-string)
   :parameter string: An instance of ``<string>``.  The string to strip.
   :parameter #key test: An instance of ``<function>``.  A function that
     accepts a character and returns #t if the character should be
     removed and ``#f`` otherwise.
   :parameter #key start: An instance of ``<integer>``, default 0.  The
     index in ``string`` at which to start stripping.
   :parameter #key end: An instance of ``<integer>``, default ``string.size``.
     The index in ``string`` before which to stop stripping.
   :value new-string: An instance of ``<string>``.
   :example:

   .. code-block:: dylan

     strip-right(" \tabc\n") => " \tabc"
     strip-right("*foo*", test: curry(\=, '*')) => "*foo"
