*******************
The logging library
*******************

.. current-library:: logging
.. current-module:: logging

Overview
--------

This is the logging library reference.  The logging library exports
a single module named "logging".


.. contents::



Quick Start
-----------

Log to stdout:

.. code-block:: dylan

  define constant $log
    = make(<logger>,
           name: "my-app",
           formatter: "%{millis} %{level} [%{thread}] - %{message}");
  add-target($log, $stdout-log-target);
  log-info($log, "My-app starting with args %s", application-arguments());

The above results in log lines like this::

  12345 INFO [Main Thread] - My-app starting with args blah

Make another logger for debugging server requests:

.. code-block:: dylan

  define constant $request-log
    = make(<logger>, name: "my-app.debug.request");

There are several things to notice about the above setup:

* Loggers have no log targets by default.  The simplest way to add a
  target is to add a pre-existing target such as ``$stdout-log-target`` or 
  ``$stderr-log-target`` using :gf:`add-target`.

* Different loggers are associated by name.  In this example the logger
  named ``"my-app"`` is an ancestor of the one named ``"my-app.debug.request"``
  because the first dotted name component matches.

* No targets were added to the ``my-app.debug.request`` logger.  Since
  all log messages sent to a child are also sent to its ancestors (but
  see :gf:`logger-additive?-setter`), anything logged to the
  ``my-app.debug.request`` logger will be passed along to the ``my-app``
  logger.

  So what's the benefit of having both loggers?  You can enable/disable
  them separately at runtime.  Also, if for example you wanted to log
  debug messages to a separate file you could add a target to the
  ``my-app.debug`` logger.

Log to a file:

.. code-block:: dylan

  add-target($log, make(<rolling-file-log-target>,
                        pathname: "/var/log/my-app.log"));

The log file will be rolled immediately if it exists and is not zero length.
If you don't want it to be rolled on startup, pass ``roll: #f`` to ``make``
in the above call.

Loggers may be disabled with ``logger-enabled?(logger) := #f``.  When
disabled, no messages will be logged to the logger's local targets,
but the value of :gf:`logger-additive?` will still be respected.  In other
words, logging to a disabled logger will still log to ancestor loggers
if they are themselves enabled.


Errors
------

If there is an error when parsing a :class:`<log-formatter>` format
control string or in finding a :class:`<logger>` object by name, a
:class:`<logging-error>` will be signaled.

.. class:: <logging-error>
   :open:

   :superclasses: :drm:`<error>`, :class:`<simple-condition>`


Log Levels
----------

There are five log levels which may be used to affect the way logs are
formatted and to include/exclude logs of different severity levels.
When configuring logging, set the log level to the least severe level
you want to see.  "Trace" logs are the least severe (or most verbose).
"Error" logs are the most severe.  The distinctions are somewhat
arbitrary, but it is hoped that five levels is enough for even the
most compulsive taxonomists.

.. class:: <log-level>
   :open:
   :abstract:
   :primary:

   Each of the log level constants documented below is an instance of
   this class.

   :superclasses: :drm:`<object>`

   :keyword name:
      The name used to display this log level.  For example,
      "INFO", "DEBUG", etc.

.. constant:: $trace-level

   The most verbose log level.  Generally use this to generate an
   absurd amount of debug output that you would never want generated
   by (for example) a production server.

.. constant:: $debug-level

   For debug messages.  Usually for messages that are expected to be
   temporary, while debugging a particular problem.

.. constant:: $info-level

   For messages about relatively important events in the normal
   operation of a program.

.. constant:: $warn-level

   For out-of-the-ordinary events that may warrant extra attention,
   but don't indicate an error.

.. constant:: $error-level

   For errors.


.. Note: I am explicitly not documenting the subclasses of <log-level>
   here because it's an implementation detail that could change.  For
   example if we decided that numeric log levels were more efficient
   than using subclassing, or that subclassing is too inflexible
   because it doesn't allow the user to easily insert new levels.

.. generic-function:: level-name

   :signature: level-name (level) => (name)

   :parameter level: An instance of :class:`<log-level>`.
   :value name: An instance of :drm:`<string>`.


Logging Functions
-----------------

.. generic-function:: log-message

   :signature: log-message (level logger object #rest args) => ()

   This is the most basic logging function.  All of the logging
   functions below simply call this with a specific
   :class:`<log-level>` object.

   :parameter level: An instance of :class:`<log-level>`.
   :parameter logger: An instance of :class:`<logger>`.
   :parameter object: An instance of :drm:`<object>`.  Normally this is
     a format control string, but it is also possible (for example) to log 
     objects to a database back-end.
   :parameter #rest args: Instances of :drm:`<object>`.  These are normally
     format arguments to be interpolated into the above format control string.

.. function:: log-error

   Equivalent to ``log-message($log-error, ...)``.

   See :func:`log-message`.

.. function:: log-warning

   Equivalent to ``log-message($log-warn, ...)``.

   See :func:`log-message`.

.. function:: log-info

   Equivalent to ``log-message($log-info, ...)``.

   See :func:`log-message`.

.. function:: log-debug

   Equivalent to ``log-message($log-debug, ...)``.

   See :func:`log-message`.

.. function:: log-debug-if

   :signature: log-debug-if (test logger object #rest args) => ()

   Equivalent to::

     if (test)
       log-message($log-debug, ...)
     end

   See :func:`log-message`.

.. function:: log-trace

   Equivalent to ``log-message($log-trace, ...)``.

   See :func:`log-message`.

.. generic-function:: log-level-applicable?

   :signature: log-level-applicable? (given-level logger-level) => (applicable?)

   :parameter given-level: An instance of :class:`<log-level>`.
   :parameter logger-level: An instance of :class:`<log-level>`.
   :value applicable?: An instance of :drm:`<boolean>`.


Loggers
-------

.. class:: <abstract-logger>
   :abstract:

   :superclasses: :drm:`<object>`

   :keyword name:
      *(required)*  The dotted name of this logger.  A :drm:`<string>`.
   :keyword additive?:
      A :drm:`<boolean>` specifying whether log messages sent to this
      logger should be passed along to its parent logger.  The default
      is ``#t``.
   :keyword children:
      A :drm:`<sequence>` of :class:`<logger>` objects.
   :keyword enabled?:
      :drm:`<boolean>` specifying whether this logger is enabled.
      Note that the value of *additive?* will be respected even if the
      logger is disabled.  The default is ``#t``.
   :keyword parent:
      The parent of this logger.

.. class:: <logger>
   :open:

   :superclasses: :class:`<abstract-logger>`

   :keyword formatter:
      An instance of :class:`<log-formatter>`.
   :keyword level:
      An instance of :class:`<log-level>`.
   :keyword targets:
      A collection of :class:`<log-target>` objects, each of which
      receives log messages sent to this logger.

.. generic-function:: get-logger

   :signature: get-logger (name) => (abstract-logger or #f)

   :parameter name:
      An instance of :drm:`<string>`.  This is normally a
      dotted path name like "http.server.queries".
   :value logger:
      An instance of :class:`<abstract-logger>` or ``#f``.

.. generic-function:: get-root-logger

   :signature: get-root-logger () => (logger)

   :value logger:
      An instance of :class:`<logger>`.

.. generic-function:: log-level

   :signature: log-level (logger) => (level)

   :parameter logger:
      An instance of :class:`<logger>`.
   :value level:
      An instance of :class:`<log-level>`.

.. generic-function:: log-level-setter

   :signature: log-level-setter (new-level logger) => (new-level)

   :parameter new-value: An instance of :class:`<log-level>`.
   :parameter logger: An instance of :class:`<logger>`.
   :value new-value: An instance of :class:`<log-level>`.

.. generic-function:: log-targets

   :signature: log-targets (logger) => (targets)

   :parameter logger: An instance of :class:`<logger>`.
   :value targets: An instance of :drm:`<stretchy-vector>`.

.. generic-function:: logger-additive?

   :signature: logger-additive? (logger) => (additive?)

   :parameter logger: An instance of :class:`<logger>`.
   :value additive?: An instance of :drm:`<boolean>`.

.. generic-function:: logger-additive?-setter

   :signature: logger-additive?-setter (new-value logger) => (new-value)

   :parameter new-value: An instance of :drm:`<boolean>`.
   :parameter logger: An instance of :class:`<logger>`.
   :value new-value: An instance of :drm:`<boolean>`.

.. generic-function:: logger-enabled?

   :signature: logger-enabled? (logger) => (enabled?)

   :parameter logger: An instance of :class:`<logger>`.
   :value enabled?: An instance of :drm:`<boolean>`.

.. generic-function:: logger-enabled?-setter

   :signature: logger-enabled?-setter (new-value logger) => (new-value)

   :parameter new-value: An instance of :drm:`<boolean>`.
   :parameter logger: An instance of :class:`<logger>`.
   :value new-value: An instance of :drm:`<boolean>`.

.. generic-function:: logger-name

   :signature: logger-name (logger) => (name)

   :parameter logger: An instance of :class:`<logger>`.
   :value name: An instance of :drm:`<string>`.

.. generic-function:: add-target

   :signature: add-target (logger target) => ()

   :parameter logger: An instance of :class:`<logger>`.
   :parameter target: An instance of :class:`<log-target>`.

.. generic-function:: remove-all-targets

   :signature: remove-all-targets (logger) => ()
   :parameter logger: An instance of :class:`<logger>`.

.. generic-function:: remove-target

   :signature: remove-target (logger target) => ()
   :parameter logger: An instance of :class:`<logger>`.
   :parameter target: An instance of :class:`<log-target>`.

.. generic-function:: log-formatter

   :signature: log-formatter (logger) => (formatter)

   :parameter logger: An instance of :class:`<logger>`.
   :value formatter: An instance of :class:`<log-formatter>`.

.. generic-function:: log-formatter-setter

   :signature: log-formatter-setter (formatter logger) => (formatter)

   :parameter formatter: An instance of :class:`<log-formatter>`.
   :parameter logger: An instance of :class:`<logger>`.
   :value formatter: An instance of :class:`<log-formatter>`.


Log Targets
-----------

.. class:: <log-target>
   :open:
   :abstract:

   :superclasses: <closable-object>:common-extensions:common-dylan


.. class:: <null-log-target>

   :superclasses: <log-target>

   A log target that discards all messages.


.. class:: <file-log-target>

   :superclasses: <log-target>

   :keyword pathname:
      *(required)* An instance of :class:`<pathname>`.

   A log target that logs to a single, monolithic file.  You probably
   want :class:`<rolling-file-log-target>` instead.

.. generic-function:: target-pathname

   :signature: target-pathname (file-log-target) => (pathname)

   :parameter target: An instance of :class:`<file-log-target>`.
   :value pathname: An instance of :class:`<pathname>`.

.. generic-function:: open-target-stream
   :open:

   This should not be called except by the logging library itself.
   Implementers of new log target classes may override it.

   :signature: open-target-stream (target) => (stream)

   :parameter target: An instance of ``<file-log-target>``.
   :value stream: An instance of :class:`<stream>`.

.. class:: <rolling-file-log-target>

   :superclasses: <file-log-target>

   :keyword max-size:
      An :drm:`<integer>`.  The size in bytes at which to roll the file.
      The default size is 100MB.  Note that the actual size of the file
      when it rolls may be slightly larger, depending on the size of the
      last message logged.
   :keyword roll:
      A :drm:`<boolean>` specifying whether to roll the log file at the
      time this log target is created, if it already exists and is not
      empty.

.. class:: <stream-log-target>
   :open:

   A log target that sends all messages to a stream.

   :superclasses: <log-target>

   :keyword stream:
      *(required)* An instance of :class:`<stream>`.

.. generic-function:: target-stream

   :signature: target-stream (target) => (stream)

   :parameter target: An instance of :class:`<stream-log-target>`.
   :value stream: An instance of :class:`<stream>`.

   
.. generic-function:: log-to-target
   :open:

   This should not be called except by the logging library itself.
   Implementers of new log target classes may override it.

   :signature: log-to-target (target formatter object #rest args) => ()

   :parameter target: An instance of ``<log-target>``.
   :parameter formatter: An instance of ``<log-formatter>``.
   :parameter object: An instance of :drm:`<object>`.
   :parameter #rest args: An instance of :drm:`<object>`.

.. generic-function:: write-message
   :open:

   This should not be called except by the logging library itself.
   Implementers of new log target classes may override it.

   :signature: write-message (target object #rest args) => ()

   :parameter target: An instance of ``<log-target>``.
   :parameter object: An instance of :drm:`<object>`.
   :parameter #rest args: An instance of :drm:`<object>`.

.. constant:: $null-log-target

   An predefined instance of :class:`<null-log-target>`.

.. constant:: $stderr-log-target

   An predefined instance of :class:`<stream-log-target>` that sends
   log messages to ``*standard-error*``.

.. constant:: $stdout-log-target

   An predefined instance of :class:`<stream-log-target>` that sends
   log messages to ``*standard-output*``.



Log Formatting
--------------

Each ``<logger>`` has a ``<log-formatter>`` that determines how to format
each log message.  Make one like this::

  make(<log-formatter>, pattern: "...");

The log formatter pattern is similar to a format control string except it
has a short and long form for each format directive.  Here are the defined
format directives:

=====  ===========  ===================================================
Short  Long         Description
=====  ===========  ===================================================
%d     %{date:fmt}  Current date.  In the long form, fmt is any string
                    acceptable as the first argument to :func:`format-date`.
%l     %{level}     Log level.  e.g., INFO, DEBUG, ERROR, etc
%m     %{message}   Log message, as passed to log-info, log-debug etc.,
                    with format arguments already interpolated.
%p     %{pid}       Current process ID.  (Not yet implemented.)
%r     %{millis}    Milliseconds since application started.
%t     %{thread}    Current thread name.
%%     None         The % character.
=====  ===========  ===================================================

.. TODO(cgay): %{micros}

All format directives, in either short or long form, accept a numeric
argument immediately following the % character.  If provided, the numeric
argument specifies the minimum width of the field.  If the numeric argument
is positive then the displayed value will be left justified and padded
with spaces on the right if necessary.  If negative, the displayed value
will be right justified and padded with spaces on the left if needed.

.. constant:: $default-log-formatter

   Formatter used if none is specified when a :class:`<logger>` is
   created.  Has this pattern::

     "%{date:%Y-%m-%dT%H:%M:%S.%F%z} %-5L [%t] %m"

.. class:: <log-formatter>
   :open:

   :superclasses: :drm:`<object>`

   :keyword pattern:
      An instance of :drm:`<string>`.
