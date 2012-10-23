*******************************
The command-line-parser Library
*******************************

.. TODO:
   * List init arguments for all classes.
   * parse-command-line doesn't talk about <usage-error>
   * Document error classes.
   * Link <string-table>.

.. current-library:: command-line-parser
.. current-module:: command-line-parser

.. contents::  Contents
   :local:

The *command-line-parser* library provides a facility to parse the
command line.  It exports two modules:

* *command-line-parser* - Main API module
* *option-parser-protocol* - For extending the API


Quick Start
===========

Let's say you want to parse a command line that looks like this::

  frob --name=zoo --debug -r a -r b -r c one two three

The "frob" command accepts a --name argument that takes a value, a
boolean --debug (or --nodebug) a -r argument that may be repeated, and
then any number of positional arguments (here "one", "two", and
"three").  Here's how to create a parser for the frob command:

.. code-block:: dylan

  let parser = make(<command-line-parser>);
  add-option(parser,
             make(<optional-parameter-option>,
                  names: #("name"),
                  help: "A name"));
  add-option(parser,
             make(<flag-option>,
                  names: #("debug"),
                  negative-names: #("nodebug"),
                  default: #f,
                  help: "Enable or disable debugging"));
  add-option(parser,
             make(<repeated-parameter-option>,
                  names: #("r"),
                  variable: "RAD",  // shows up in --help output
                  help: "Free radicals"));

There is also a :class:`<keyed-option>` which is not shown here.
See the reference section for more info.

Now parse the command line:

.. code-block:: dylan

  block ()
    parse-command-line(parser, application-arguments());
  exception (ex :: <usage-error>)
    format-out("%s\n", condition-to-string(ex));
    exit-application(2);
  end;

And to access the option values:

.. code-block:: dylan

  let debug? :: <boolean> = get-option-value(parser, "debug");
  let name :: false-or(<string>) = get-option-value(parser, "name");
  let dash-r :: <deque> = get-option-value(parser, "r");
  let args :: <sequence> = positional-options(parser);


Reference
=========


The COMMAND-LINE-PARSER module
------------------------------

.. class:: <command-line-parser>
   :open:

   Encapsulates a set of command-line options.

   :superclasses: <object>

   :keyword provide-help-option?:

     A boolean specifying whether the parser should automatically add
     the default help option.  By default, help may be requested via
     ``--help`` or ``-h``.  If ``#f``, no help option will be added to
     the parser, and you must explicitly handle any request for help
     yourself.

   :keyword help-option:

     A ``<flag-option>`` that will be added to the parser as the
     option that signals a request for help.  The main purpose of this
     init keyword is to make it possible to use something other than
     ``--help`` and ``-h`` to request help.  This keyword has no
     effect if ``provide-help-option?`` is ``#f``.

.. function:: add-option

   Add an option to a command-line parser.

   :signature: add-option (parser option) => ()
   :parameter parser: An instance of :class:`<command-line-parser>`.
   :parameter option: An instance of :class:`<option>`.
   :description:

     If any of the option names specified are already used by other
     options then ``<command-line-parser-error>`` is signaled.

.. function:: parse-command-line

   Parses the command line in ``argv`` and side-effects ``parser``
   accordingly.

   :signature: parse-command-line (parser argv) => (success?)
   :parameter parser: An instance of :class:`<command-line-parser>`.
   :parameter argv: An instance of ``<sequence>``.  Normally the value
     returned by ``application-arguments()`` is passed here.

.. generic-function:: print-synopsis
   :open:

   Display a synopsis of the command line described by ``parser`` on
   ``stream``.

   :signature: print-synopsis (parser stream) => ()
   :parameter parser: An instance of :class:`<command-line-parser>`.
   :parameter stream: An instance of :class:`<stream>`.
   :parameter #key usage: An instance of ``<string>`` or ``#f``.  If provided,
     this is displayed before the command-line options.  This is
     intended to be a one-line summary of the command-line format.
   :parameter #key description: An instance of ``<string>`` or ``#f``.  If
     provided, this is displayed after ``usage`` and before the
     command-line options.  This is intended to be a sentence or short
     paragraph.

.. generic-function:: positional-options

   Returns the sequence of command line arguments that remain after
   all optional arguments have been consumed.

   :signature: positional-options (parser) => (args :: ``<sequence>``)
   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. function:: option-present?

   Returns ``#t`` if this option was supplied on the command line.

   :signature: option-present? (parser name) => (present?)
   :parameter parser: An instance of :class:`<command-line-parser>`.
   :parameter name: An instance of ``<string>``.
   :value present?: An instance of ``<boolean>``.
   :description:

     If called before :func:`parse-command-line` has been called on the
     associated parser, this will always return ``#f``.

.. function:: get-option-value

   Retrieves an option from an :class:`<command-line-parser>` by its
   long name.

   :signature: get-option-value (parser long-name) => (value)

   :parameter parser: An instance of :class:`<command-line-parser>`.
   :parameter long-name: An instance of ``<string>``.
   :value value: An instance of ``<object>``.



Option Classes
~~~~~~~~~~~~~~

.. class:: <option>
   :abstract:
   :open:
   :primary:

   Superclass of all other option types.

   :superclasses: <object>

   :keyword names:

     Names for this option; a sequence of strings.  For convenience a
     single string may also be specified.  Strings of length 1 are
     considered to be short options, i.e., they are prefixed by a
     single dash on the command line.

   :keyword type:

     The kind of value represented by this option.  That is, the
     string passed on the command line will be coerced to this type
     via the ``parse-option-parameter`` generic function.  Clients may
     implement that function for their own types to extend the parser.

     Predefined types include ``<integer>``, ``subclass(<float>)``,
     ``subclass(<sequence>)``.

   :keyword help:

     A string documenting the option.  Displayed in ``--help`` output.
     Some automatic substitutions are performed:

       1. "%default" => the string representation of the default value
          for the option.

       2. "%app" => the basename of the executable program.

       3. "%%" => "%"

   :keyword variable:

     A string to stand in for the option value in ``--help`` output.
     For example, if the option name is ``--database`` this might be
     "URL", which would display as::

       --database  URL  A database URL.
   
   :keyword default:

     A default value for the option that will be used if the option
     isn't specified by the user.


.. class:: <flag-option>
   :sealed:

   Defines a flag option, i.e., one defines a boolean value.

   :superclasses: :class:`<option>`

   :keyword negative-names:

     Same as ``names``, but specifies the negative forms.

   :description:

     They default to ``#f`` and exist in both positive and negative forms:
     "--foo" and "--no-foo".  In the case of conflicting options, the
     rightmost takes precedence to allow for abuse of the shell's
     "alias" command.

     For example, a single instance of this class could be used to
     specify *all* of the following command-line options::

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

     A sequence of objects.  If the value supplied on the command line
     isn't one of these objects then :class:`<usage-error>` is
     signaled.

   :keyword test:

     A function to test whether the value supplied on the command line
     is the same as one of the choices.  The default is ``=``.  Another
     commonly used value is ``string-equal-ic?``, to ignore case in the
     comparison.

   :description:

     Example::

       make(<choice-option>,
            names: #("foo"),
            choices: #("a", "b"),
            test: string-equal-ic?)
       


.. class:: <keyed-option>
   :sealed:

   Each occurrence of this type of option defines a key => value
   mapping.

   :superclasses: :class:`<option>`

   :description:

     These are a bit obscure. The best example is gcc's ``-D`` option.
     The final value is a ``<string-table>`` containing each specified
     key, with one of the following values:

     * ``#t``: The user specified "-Dkey"
     * a string: The user specified "-Dkey=value"

     You can read this with ``element(table, key, default: #f)`` to get a
     handy lookup table.

     Examples::

       -Dkey, -Dkey=value, -D key = value, --define key = value


.. macro:: option-parser-definer


The OPTION-PARSER-PROTOCOL module
---------------------------------

This module exports an API that can be used to extend the existing
command line parser without modifying the source in this library.  It
shouldn't be common to need this.  See the source code for details.
