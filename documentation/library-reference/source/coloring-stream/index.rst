***************************
The coloring-stream Library
***************************

.. current-library:: coloring-stream
.. current-module:: coloring-stream

The ``coloring-stream`` module provides a means for writing text to
the console or terminal with colors and varying intensity.

On Unix, this is typically done via ANSI codes.  Windows support is
yet to be implemented.

Usage
*****

It is implemented as a :class:`<wrapper-stream>`, so it can wrap any
given stream. While it can be created via :drm:`make`, it is easiest
to just call :gf:`colorize-stream` on a stream to get back a
:class:`<coloring-stream>` wrapper that can be used to output color.

If the underlying stream doesn't support color, then the returned
:class:`<coloring-stream>` will silently drop all text attributes.

Once you have a :class:`<coloring-stream>`, you can set text attributes
by writing instances of :class:`<text-attributes>`. Text attributes
are created with :gf:`text-attributes`. Text can have a foreground
color, a background color and an intensity:

.. code-block:: dylan

   let error-attributes = text-attributes(foreground: $color-red);
   let blinding-attributes = text-attributes(foreground: $color-green,
                                             background: $color-red,
                                             intensity: $bright-intensity);

To reset text back to the defaults, use the predefined
:const:`$reset-attributes` constant.

Text attributes can be stored and used repeatedly. This lets you set up
some stylized text attributes and re-use them.

The text attributes can be written to the stream in two different ways:

* Via :gf:`format`
* Via :gf:`print` or :gf:`print-object`

Once you have a :class:`<coloring-stream>`, then you can write colorized
text to it with :gf:`format`:

.. code-block:: dylan

   let error-attributes = text-attributes(foreground: $color-red);
   let cs = colorize-stream(*standard-output*);
   format(cs, "%=Error:%= %s", error-attributes, $reset-attributes, error-message);

This is implemented by providing an implementation of :gf:`print`
specialized on :class:`<text-attributes>` and :class:`<coloring-stream>`.

This can be used directly as well:

.. code-block:: dylan

   let error-attributes = text-attributes(foreground: $color-red);
   let cs = colorize-stream(*standard-output*);
   print(error-attributes, cs);
   print("Error:", cs);
   print($reset-attributes, cs);
   print(' ', cs);
   print(error-message, cs);

The coloring-stream Module
**************************

.. class:: <coloring-stream>
   :abstract:

   :superclasses: :class:`<wrapper-stream>`

   :description:

     :class:`<coloring-stream>` is the abstract wrapper stream
     that is used to determine whether or not a stream supports
     color output to avoid having to check at every write.

.. generic-function:: colorize-stream

   :signature: colorize-stream (stream #key force-ansi?) => (coloring-stream)

   :parameter stream: An instance of :class:`<stream>`.
   :parameter #key force-ansi?: An instance of :drm:`<boolean>`.
   :value coloring-stream: An instance of :class:`<coloring-stream>`.

   :description:

     Wrap a *stream* with an appropriate instance of
     :class:`<coloring-stream>`. It uses :gf:`stream-supports-color?`
     to determine whether or not the underlying stream supports
     color output.

     When *force-ansi?* is :drm:`#t`, then the usual checks are skipped
     and a coloring stream that generates ANSI output is created.
     This is useful when outputting to a string prior to writing the
     text to :var:`*standard-output*` or when writing a network server
     where the user may have an ANSI-capable client.

     When called on a :class:`<coloring-stream>`, if *force-ansi?* is
     set and the stream is not an ANSI coloring stream, then the stream
     will be unwrapped and a new ANSI coloring stream wrapper will
     be created. Otherwise, calling ``colorize-stream`` on a
     :class:`<coloring-stream>` will return the same stream.

   :example:

     .. code-block:: dylan

        let stream = colorize-stream(*standard-output*);

     Or, using ``force-ansi?``:

     .. code-block:: dylan

        let text
          = with-output-to-string (s :: <byte-string-stream>)
              let force-ansi? = stream-supports-color?(*standard-output*);
              let s = colorize-stream(s, force-ansi?: force-ansi?);
              ...
            end with-output-to-string;
        write(*standard-output*, text);

.. generic-function:: stream-supports-color?
   :open:

   :signature: stream-supports-color? (stream) => (well?)

   :parameter stream: An instance of :class:`<stream>`.
   :value well?: An instance of :drm:`<boolean>`.

   :description:

     Return whether or not the underlying stream supports color
     output.

     This checks that:

     * The stream is a :class:`<file-stream>`.
     * :gf:`stream-console?` is true. (On Unix, this means that ``isatty``
       is true for the stream.)
     * The ``TERM`` environment variable is not ``"dumb"``.
     * The ``EMACS`` environment variable is not ``"t"``.

.. generic-function:: stream-supports-ansi-color?
   :open:

   :signature: stream-supports-ansi-color? (stream) => (well?)

   :parameter stream: An instance of :class:`<stream>`.
   :value well?: An instance of :drm:`<boolean>`.

   :description:

     Return whether or not the underlying stream might support
     ANSI color output, assuming that the underlying stream
     supports color output at all.

     On Unix, this will always return :drm:`#t`.

     On Windows, this attempts to detect situations where ANSI
     output would be permissible, such as running within an
     alternate console window like ConEMU.

     .. note:: This does NOT check to see if the stream actually
        supports coloring. It is meant to be used in conjunction
        with :gf:`stream-supports-color?`.

Text Attributes
===============

.. class:: <text-attributes>

   :superclasses: :drm:`<object>`

   :keyword background: An instance of ``false-or(<text-color>)``.
   :keyword foreground: An instance of ``false-or(<text-color>)``.
   :keyword intensity: An instance of ``false-or(<text-intensity>)``.

   :description:

     Instances of ``<text-attributes>`` are used for representing the
     desired text appearance. They can be passed to :gf:`format` when
     writing to a :class:`<coloring-stream>`.

     *background* and *foreground*, if given, should be one of the color
     constants like :const:`$color-red`, :const:`$color-green`, etc.

     *intensity*, if given, should be one of :const:`$bright-intensity`,
     :const:`$dim-intensity` or :const:`$normal-intensity`.

     Values that are omitted are set to the default values for the terminal.

.. function:: text-attributes

   :signature: text-attributes (#key foreground background intensity) => (attributes)

   :parameter #key foreground: An instance of ``false-or(<text-color>)``.
   :parameter #key background: An instance of ``false-or(<text-color>)``.
   :parameter #key intensity: An instance of ``false-or(<text-intensity>)``.
   :value attributes: An instance of :class:`<text-attributes>`.

   :description:

     ``text-attributes`` provides an easy wrapper for creating instances
     of :class:`<text-attributes>`.

     *background* and *foreground*, if given, should be one of the color
     constants like :const:`$color-red`, :const:`$color-green`, etc.

     *intensity*, if given, should be one of :const:`$bright-intensity`,
     :const:`$dim-intensity` or :const:`$normal-intensity`.

     Values that are omitted are set to the default values for the terminal.

   :example:

     .. code-block:: dylan

        let error-attributes = text-attributes(foreground: $color-red);

.. constant:: $reset-attributes

   :type: :class:`<text-attributes>`

   :description:

     A predefined constant to reset the text back to the default attributes.

     This is equivalent to ``text-attributes(intensity: $normal-intensity)``.

Text Intensity
--------------

.. constant:: $bright-intensity

.. constant:: $dim-intensity

   :description:

     .. note:: Not all terminals support dimmed text.

.. constant:: $normal-intensity

Text Colors
-----------

.. constant:: $color-black

.. constant:: $color-blue

.. constant:: $color-cyan

.. constant:: $color-default

.. constant:: $color-green

.. constant:: $color-magenta

.. constant:: $color-red

.. constant:: $color-white

.. constant:: $color-yellow
