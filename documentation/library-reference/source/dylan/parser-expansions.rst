*****************
Parser Expansions
*****************

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
