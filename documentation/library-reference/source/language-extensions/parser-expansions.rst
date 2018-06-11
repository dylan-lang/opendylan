*****************
Parser Expansions
*****************

Hash Literals
=============

.. warning:: This is a highly experimental extension. It is not
   well specified, is not supported by any of the editor integrations
   or syntax highlighters, and may change or disappear in the future.

The syntax::

    #<name>:<text>

gets transformed, setter-like, into::

  <name>-parser(<text>)

The ``<text>`` part can be either *delimited* or *undelimited*. Undelimited
text can contain anything but commas, semicolons, brackets of any kind, and
whitespace. There is no ``\`` escape processing. All the following are valid::

    #http://opendylan.org/
    #time:12:30am
    #date:12/3/2000
    #file:D:\dylan\sources\
    #mailto:hackers@lists.opendylan.org

In the delimited form, you have a choice of delimiters: ``"..."``, ``(...)``,
``[...]``, ``{...}``. Within the delimiters, only the matching close delimiter
must be escaped. The selection allows you to choose the delimiter requiring least
escaping for the enclosed data. The text (less the delimiters) is passed
to the parsing function. Examples::

    #file:"C:\Program Files\Open Dylan\."
    #html:{<html>
      <head><title>Foo</title></head>
      <body bgcolor="#FFFFFF">
      </body>
      </html>}

An example parser:

.. code-block:: dylan

    define method html-parser
        (text :: <byte-string>)
     => (doc :: <html-document>)
       make(<html-document>, text: text)
    end method;

If an appropriate function isn't defined, you get a standard unbound variable
reference message indicating the # literal.


Multi-line Strings
==================

Multi-line string literals are delimited by three double quote characters on
each end.  The BNF, which augments the `Character and String Literals
<http://opendylan.org/books/drm/Lexical_Grammar#HEADING-117-38>`_ section in
the DRM, is ::

  MULTI-LINE-STRING:

      """ more-multi-line-string

  MORE-MULTI-LINE-STRING:

      multi-line-string-character more-multi-line-string

      """

  MULTI-LINE-STRING-CHARACTER:

      any printing character (including space) except for \\

      \\ escape-character

Escape characters are interpreted as with the standard STRING production.
Literal end-of-line sequences (\\n and \\r\\n) are always interpreted as a
single Newline (LF) character, regardless of operating system platform.

Examples:

Equivalent to ``"abc"``::

  """abc"""

Equivalent to ``"line one\nline two"``::

  """line one
  line two"""

Equivalent to ``"let x = \"foo\";"``::

  """let x = "foo";"""

Equivalent to ``"\nfoo\nbar\n"``::

  """
  foo
  bar
  """

Raw String Literals
===================

Raw string literals are string literals with no backslash escape
processing.  The hash literal syntax described above is used, in
conjunction with a ``string-parser`` function exported from the
``common-extensions`` module (and therefore also from the
``common-dylan`` module).  See :ref:`string-parser <string-parser>`
for details.
