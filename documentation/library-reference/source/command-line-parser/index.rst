*******************************
The command-line-parser Library
*******************************

.. current-library:: command-line-parser
.. current-module:: command-line-parser

.. contents::  Contents
   :local:

The *command-line-parser* library provides a facility to parse the
command line.  It exports two modules:

* *command-line-parser* - Main API module
* *option-parser-protocol* - For extending the API

Overview
========

Here's a quick list of features to get a sense of what's available:

* Various option types

  * :class:`<flag-option>` for simple ``--boolean`` flags.
  * :class:`<parameter-option>` for ``--foo=bar`` options.
  * :class:`<repeated-parameter-option>` options may be repeated: ``-r a -r b``
  * :class:`<optional-parameter-option>` in which the value is optional:
    ``--foo`` or ``--foo bar``
  * :class:`<keyed-option>` fills a table with key/value pairs, for example:
    ``-Dk1=v1 -Dk2=v2``
  * :class:`<choice-option>` the value may be chosen from a predefined set.
  * :class:`<positional-option>` to receive positional arguments.

* Automatic and extensible conversion of arguments to other types. For example,
  an option with ``type: <integer>`` is automatically converted to integer by
  the parser.
* Subcommands
* Automatic usage and ``--help`` generation.
* A convenient and readable syntax for creating the command line.
* Extensibility. Add your own argument converters with :gf:`parse-option-value`
  or create entirely new option types.


Quick Start
===========

The best way to build a command line parser is with
:macro:`command-line-definer`, since it is far more concise and readable than
building one "by hand" with ``make(<command-line-parser>, options: ...)``.  It
also provides convenient methods to retrieve command-line option values rather
than accessing them by name, with strings.

.. note:: :macro:`command-line-definer` does not currently support subcommands.
          If you need subcommands you will have to build the parser by hand for
          now.

Let's say you want to parse a command line that looks like this::

  frob --name=zoo --debug -r a -r b -r c --choice=foo one two three

The "frob" command accepts a ``--name`` option that takes a value, a boolean
``--debug`` (or ``--nodebug``) a repeatable ``-r`` option, a ``--choice``
option that accepts one of several values, and then at least one positional
argument (here "one", "two", and "three").  Here's what that parser looks like:

.. code-block:: dylan

  define command-line <frob-command-line> ()
    option frob-name :: <string>,
      names: #("name"),
      help: "Name of the frob",
      kind: <parameter-option>;
    option frob-debug? :: <boolean>,
      names: #("debug"),
      negative-names: #("nodebug"),
      help: "Enable or disable debugging",
      kind: <flag-option>;   // This is the default.
    option frob-radicals :: <sequence>,
      names: #("r"),
      kind: <repeated-parameter-option>,
      variable: "RAD",       // Makes --help show "-r RAD"
      help: "Free radicals";
    option frob-choice :: <string>,
      names: #("choice"),
      choices: #("foo", "bar", "baz"),
      default: "foo",
      help: "Your choice";
    option frob-filenames :: <sequence>,
      names: #("filenames"),
      kind: <positional-option>,
      repeated?: #t,
      help: "One or more filenames";
  end command-line;

Now parse the command line:

.. code-block:: dylan

  block ()
    let cmd = make(<frob-command-line>, help: "frob things");
    parse-command-line(cmd, application-arguments());
    // Now execute your main program code with cmd containing
    // the parsed argument values.
    frob(cmd);
  exception (err :: <abort-command-error>)
    // This condition is signaled by parse-command-line and also if
    // your own code calls abort-command().
    format-err("%s", err);
    exit-application(err.exit-status);
  end;

To access the option values simply read the ``<frob-command-line>`` slot
values. Assuming ``cmd`` is the command parsed above:

.. code-block:: dylan

   for (file in cmd.frob-filenames)
     if (cmd.frob-debug?)
       format-out(...);
     end;
     ...more...
   end;

Of course, it is also possible to make a command line parser without the macro
above, but doing so is much more verbose and requires accessing the option
values by calling ``get-option-value(cmd, "option-name")``. Briefly, just call
:drm:`make`, like this:

.. code-block:: dylan

   let cmd
     = make(<command-line-parser>,
            help: "a most excellent program",
            options: list(make(<flag-option>,
                               names: #("name"),
                               help: "provide a name"),
                          ...,
                          make(<positional-option>,
                               names: #("filenames"),
                               repeated?: #t,
                               help: "one or more filenames")),
            subcommands: list(make(<my-subcommand>, ...)));
   parse-command-line(cmd, application-arguments());
   let filenames = get-option-value(cmd, "filenames");
   ...etc...


Reference
=========


The command-line-parser Module
------------------------------

.. class:: <command>
   :abstract:
   :sealed:

   Abstract superclass of :class:`<command-line-parser>` and
   :class:`<subcommand>`.

   :keyword options:

      A sequence of :class:`<option>` instances. Note that
      :class:`<positional-option>` instances must follow all other options, and
      there may be only a single :class:`<positional-option>` that specifies
      ``repeated?: #t`` and it must be the last option in the sequence.

   :keyword help:

      Required. A :drm:`<string>` to display when help is requested via the
      ``--help`` option or the ``help`` subcommand.

.. class:: <subcommand>
   :open:
   :abstract:

   A named subcommand. Subcommands have their own set of command-line
   options. They may not contain other subcommands.

   :superclasses: :class:`<command>`

   :keyword name: Required. The subcommand name, a :drm:`<string>`.

   :description:

      Subclass this for each subcommand you need and implement a method on
      :gf:`execute-subcommand` for each subclass.

.. class:: <command-line-parser>
   :open:

   Encapsulates a set of command-line options. May optionally contain a set of
   subcommands, each of which has its own set of options.

   :superclasses: :class:`<command>`

   :keyword help-option?:

      A boolean specifying whether the parser should automatically add the
      default help option.  By default, help can be requested via ``--help`` or
      ``-h``.  If false, no help option will be added to the parser, and you
      must explicitly handle any request for help yourself. The default is
      true. See :class:`<help-option>`.

   :keyword help-subcommand?:

      A boolean specifying whether the parser should automatically add the
      default help subcommand. The default is true if the command-line has any
      subcommands. Set to false if you prefer to call the subcommand something
      else (e.g., non-English). See :class:`<help-subcommand>`.

   :keyword subcommands: A sequence of :class:`<subcommand>`.

.. class:: <help-subcommand>

   Implements the ``help`` subcommand. Normally there is no need to use this
   since the command line parser implements the ``help`` subcommand itself.
   However, if you wanted to implement the ``help`` subcommand differently, or
   just give it a different (or an additional) name, this is how to do it:

   .. code-block:: dylan

      define class <my-help-subcommand> (<help-subcommand>) end;

      let p = make(<command-line-parser>,
                   help-subcommand?: #f,
                   ...
                   subcommands: list(make(<my-help-subcommand>,
                                          names: #("ayuda"))
                                     ...));

      define method execute-subcommand
          (parser, sub :: <my-help-subcommand>) => (s :: false-or(<integer>))
        ...etc...
      end

.. class:: <command-line-parser-error>
   :open:

   Superclass of all errors signaled by this library.

   :superclasses: :class:`<format-string-condition>`, :drm:`<error>`

.. class:: <abort-command-error>
   :sealed:

   Provides a standard way for program code to indicate that the application
   should exit. Signaled by the command line parser itself after the standard
   ``--help`` option or ``help`` subcommand have completed, so programs should
   always handle this at top level.

   :superclasses: :class:`<command-line-parser-error>`

   :keyword status: Required. A status code to pass to
                    :func:`exit-application`. An :drm:`<integer>`.

   :description:

     This is commonly handled by calling ``exit-application(err.exit-status)``
     after printing the error.

.. function:: exit-status

   Returns the exit status associated with an error.

   :parameter error: An instance of :class:`<abort-command-error>`
   :value status: An instance of :drm:`<integer>`

.. class:: <usage-error>
   :open:

   Signaled when a command-line cannot be parsed.

   :superclasses: :class:`<abort-command-error>`

   :description:

     This is commonly handled by calling ``exit-application(err.exit-status)``.
     This condition need not be handled specially if the application already
     handles :class:`<abort-command-error>`.

.. function:: add-option

   Add an option to a command-line parser.

   :signature: add-option (parser option) => ()
   :parameter parser: An instance of :class:`<command-line-parser>`.
   :parameter option: An instance of :class:`<option>`.
   :description:

     If any of the option names specified are already used by other
     options then :class:`<command-line-parser-error>` is signaled.

.. function:: parse-command-line

   Parses the command line in ``argv`` and side-effects ``parser``
   accordingly.

   :signature: parse-command-line (parser argv) => ()
   :parameter parser: An instance of :class:`<command-line-parser>`.
   :parameter argv: An instance of :drm:`<sequence>`.  Normally the value
     returned by :func:`application-arguments` is passed here.
   :description:

     By default, the ``--help`` flag and the "help" subcommand are handled
     automatically by calling :gf:`print-synopsis` and then signaling
     :class:`<abort-command-error>`, so the caller should handle that
     condition.

     If ``argv`` isn't a valid set of options as described by the
     ``parser`` then :class:`<usage-error>`.

     See `Quick Start`_ for an example.

.. generic-function:: execute-command

   When using subcommands, call this to execute the parsed command line.

   :signature: execute-command (parser) => (false-or(<integer>))
   :parameter parser: An instance of :class:`<command-line-parser>`.

   :description:

      Call this after calling :gf:`parse-command-line`, if your command-line
      has subcommands. (If not using subcommands there is no need to call this;
      just read option values from parser directly.) It determines the
      subcommand indicated by the user and invokes :gf:`execute-subcommand` on
      it. The ``help`` subcommand is handled specially.

.. generic-function:: execute-subcommand

   Implement a method on this generic for each subclass of
   :class:`<subcommand>` you create.

   :signature: execute-subcommand (parser subcommand) => (false-or(<integer>))
   :parameter parser: An instance of :class:`<command-line-parser>`.
   :parameter subcommand: An instance of :class:`<subcommand>`.

   :description:

      This is the implementation of your subcommand. Read command-line options
      from ``subcommand`` and read global options (if any) from ``parser``.

.. generic-function:: print-synopsis
   :open:

   Display a synopsis of the command line options.

   :signature: print-synopsis (parser subcommand #key stream) => ()
   :parameter parser: An instance of :class:`<command-line-parser>`.
   :parameter subcommand: An instance of :class:`<subcommand>` or ``#f``.
   :parameter #key stream: An instance of :class:`<stream>`.

   :description:

      It is not normally necessary to call this function since the ``--help``
      option and ``help`` subcommand are handled automatically by the parser.

.. function:: option-present?

   Returns ``#t`` if this option was supplied on the command line.

   :signature: option-present? (parser name) => (present?)
   :parameter parser: An instance of :class:`<command-line-parser>`.
   :parameter name: An instance of :drm:`<string>`.
   :value present?: An instance of :drm:`<boolean>`.
   :description:

     If called before :func:`parse-command-line` has been called on the
     associated parser, this will always return ``#f``.

.. function:: get-option-value

   Retrieves an option from a :class:`<command>` by name.

   :signature: get-option-value (parser name) => (value)

   :parameter parser: An instance of :class:`<command>`.
   :parameter name: An instance of :drm:`<string>`.
   :value value: An instance of :drm:`<object>`.

.. generic-function:: parse-option-value
   :open:

   Convert a command line argument from a :drm:`<string>` to the
   :class:`<option>` type.

   :signature: parse-option-value(argument, type) => (value)
   :parameter argument: An instance of :drm:`<string>`.
   :parameter type: An instance of :drm:`<type>`.
   :value value: An instance of :drm:`<object>`.

   :description:

      Convert a command-line argument (a string) to the type specified in the
      corresponding :class:`<option>` instance. For example, given the
      following code, the "version" option's :gf:`option-value` slot would be
      set to an instance of ``<version>`` by the command line parser so that
      you don't have to manually do it in your application.

      .. code-block:: dylan

         make(<parameter-option>,
              names: #("version"),
              help: "A version specifier. Ex: v1.2.3",
              type: <version>)

         define method parse-option-value
             (arg :: <string>, type :: <version>) => (v :: <version>)
           parse-version(arg)
         end;

      There are predefined methods that convert to :drm:`<number>`,
      :drm:`<boolean>`, :drm:`<symbol>`, and :drm:`<sequence>`. For
      :drm:`<boolean>`, valid values are yes/no, on/off, true/false.
      For :drm:`<sequence>`, strings are simply split around commas,
      without any attempt to be smart about quoting.


Option Classes
~~~~~~~~~~~~~~

.. class:: <option>
   :abstract:
   :open:

   Superclass of all other option types.

   :superclasses: :drm:`<object>`

   :keyword names:

     Names for this option; a sequence of strings.  For convenience a single
     string may be given if the option only has one name.  Strings of length 1
     are considered to be short options, i.e., they are prefixed by a single
     dash on the command line.

     The first name in the list is considered the "canonical" name of the
     option and is used in various parts of the auto-generated ``--help``
     message.

   :keyword type:

     The kind of value represented by this option.  That is, the string passed
     on the command line will be coerced to this type via
     :gf:`parse-option-value` if a relevant method exists.  Clients may
     implement methods on that function for their own types to extend the
     parser.

     Predefined types include :drm:`<integer>`, ``subclass(<float>)``,
     ``subclass(<sequence>)``.

   :keyword help:

     A string documenting the option.  Displayed in ``--help`` output.
     Some automatic substitutions are performed:

       1. "%default%" => The string representation of the default value
          for the option.

       2. "%app%" => The basename of the executable program.

     For example, given this option specification:

     .. code-block:: dylan

        make(<parameter-option>,
             names: #("v", "version"),
             help: "Package version [%default%]",
             default: "latest")

     it will be displayed as::

       --version V   Package version [latest]

   :keyword variable:

     A string to stand in for the option value in ``--help`` output.
     For example, if the option name is ``--database`` this might be
     "URL", which would display as::

       --database URL  A database URL.

     When not specified, the first name of the option is used. For example:

     .. code-block:: dylan

        make(<parameter-option>,
             names: #("n", "name"),
             help: "A name")

     And on the command-line this will be displayed as::

       -n, --name N   A name

   :keyword default:

     A default value for the option that will be used if the option isn't
     specified by the user. The default value should be a member of the type
     specified with the ``type:`` keyword. ``#f`` is the default default value.

.. class:: <flag-option>
   :sealed:

   Defines a simple flag option, i.e., one that specifies a boolean value.

   :superclasses: :class:`<option>`

   :keyword negative-names:

     Same as ``names``, but specifies the negative forms.

   :description:

     They default to ``#f`` and may exist in both positive and negative forms:
     ``--foo`` and ``--no-foo``.  In the case of conflicting options, the
     rightmost takes precedence to allow for abuse of the shell's "alias"
     command.

     For example, a single instance of this class could be used to specify
     *all* of the following command-line options::

         -q, -v, --quiet, --verbose


.. class:: <parameter-option>
   :sealed:

   Defines an option that requires a value be specified.

   :superclasses: :class:`<option>`

   :description:

     If the option appears more than once, the rightmost value takes
     precedence. If the option never appears, these will default to
     ``#f``.

     Examples::

       -cred, -c=red, -c = red, --color red, --color=red


.. class:: <optional-parameter-option>
   :sealed:

   Similar to :class:`<parameter-option>`, but the parameter is
   optional.

   :superclasses: :class:`<option>`

   :description:

     The parameter must directly follow the option with no intervening
     whitespace, or follow an "=" token. The value is ``#f`` if the option
     never appears, ``#t`` if the option appears but the parameter does
     not, and the value of the parameter otherwise.

     Examples::

       -z, -z3, -z=3, -z = 3, --zip, --zip=3, --zip = 3

     Invalid examples::

       -z 3, --zip 3, --zip3

.. class:: <repeated-parameter-option>
   :sealed:

   Similar to :class:`<parameter-option>`, but may appear more than
   once.

   :superclasses: :class:`<option>`

   :description:

     The final value is a deque of parameter values in the order they
     appeared on the command line. It defaults to the empty deque.

     Examples::

       -wall, -w=all, -w = all, --warnings all, --warnings=all


.. class:: <choice-option>
   :sealed:

   Similar to :class:`<parameter-option>`, but provides a restricted
   set of values to choose from.

   :superclasses: :class:`<parameter-option>`

   :keyword choices:

     A sequence of objects (usually strings).  If the value supplied
     on the command line isn't one of these objects then
     :class:`<usage-error>` is signaled.  If you supply a sequence of
     non-string choices you will also need to supply the ``test:``
     init keyword since all command-line arguments are strings and
     won't compare equal with the default test, ``=``.

   :keyword test:

     A function to test whether the value supplied on the command line
     is the same as one of the choices.  The default is ``=``.  Another
     commonly used value is ``string-equal-ic?``, to ignore case in the
     comparison.

   :description:

     Example::

       make(<choice-option>,
            names: #("foo"),
            help: "a or b",
            choices: #("a", "b"),
            test: string-equal-ic?)


.. class:: <keyed-option>
   :sealed:

   Each occurrence of this type of option defines a key => value
   mapping.

   :superclasses: :class:`<option>`

   :description:

     These are a bit obscure. The best example is gcc's ``-D`` option.
     The final value is a :class:`<string-table>` containing each specified
     key, with one of the following values:

     * ``#t``: The user specified "-Dkey"
     * a string: The user specified "-Dkey=value"

     You can read this with ``element(table, key, default: #f)`` to get a
     handy lookup table.

     Examples::

       -Dkey, -Dkey=value, -D key = value, --define key = value

.. class:: <positional-option>
   :sealed:

   Accepts an argument that is passed by possition on the command line.

   :superclasses: :class:`<option>`

   :description:

      If you want your command-line parser to accept positional arguments you
      must add :class:`<positional-option>` instances to it. Positional options
      must come after all non-positional options.

      By default, positional options are marked as required but you may pass
      ``required?: #f`` to make them optional. *Note that it is an error to add
      a required positional option after an optional positional option since
      there is no way to parse that unambiguously.*

      A :class:`<positional-option>` may be marked as accepting any number of
      arguments by passing ``repeated?: #t`` when calling :drm:`make`. An
      option marked as repeated *must* be the last option in the parser's list
      of options.

      In this example the command line requires one filename to be passed on
      the command line, and one or more words after that:

      .. code-block:: dylan

         add-option(parser, make(<positional-option>,
                                 names: #("filename"),
                                 help: "A filename"));
         add-option(parser, make(<positional-option>,
                                 names: #("words"),
                                 help: "One or more words",
                                 repeated?: #t));

         // Usage: app [options] FILENAME WORDS...

.. class:: <help-option>
   :open:

   The standard ``--help`` option.

   :superclasses: :class:`<flag-option>`

   :description:

      May be subclassed if you want to implement your own help option instead
      of the default one.


The option-parser-protocol Module
---------------------------------

This module exports an API that can be used to extend the existing command line
parser without modifying the source in this library.  It shouldn't be common to
need this and it is likely to be incomplete.  See the source code for details.
