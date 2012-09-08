*******************************
The command-line-parser Library
*******************************

.. current-library:: command-line-parser
.. current-module:: command-line-parser

.. contents::  Contents
   :local:

The *command-line-parser* library provides a facility to parse the
command line.  It exports one module named *command-line-parser*.


Quick Start
===========

Let's say you want to parse a command line that looks like this::

  frob --name=zoo --debug -r a -r b -r c one two three

The "frob" command accepts a --name argument that takes a value, a
boolean --debug (or --nodebug) a -r argument that may be repeated, and
then any number of positional arguments (here "one", "two", and
"three").  Here's how to create a parser for the frob command:

.. code-block:: dylan

  let parser = make(<argument-list-parser>);
  add-option-parser-by-type(parser, <optional-parameter-option-parser>,
                            long-options: #("name"),
                            description: "A name");
  add-option-parser-by-type(parser, <simple-option-parser>,
                            long-options: #("debug"),
                            negative-long-options: #("nodebug"),
                            default: #f,
                            description: "Enable or disable debugging");
  add-option-parser-by-type(parser, <repeated-parameter-option-parser>,
                            short-options: #("r"),
                            description: "Dash R");
  add-option-parser-by-type(parser, <simple-option-parser>,
                            long-options: #("help"),
                            description: "Display this help message");

There is also a ``<keyed-option-parser>`` which is not shown here.
See the reference section for more info.

Note: *Currently you must implement support for ``--help`` yourself.
This will be fixed soon.*

Now parse the command line:

.. code-block:: dylan

  block ()
    parse-arguments(parser, application-arguments());
  exception (ex :: <usage-error>)
    format-out("%s\n", condition-to-string(ex));
    exit-application(2);
  end;
  if (option-value-by-long-name(parser, "help"))
    print-synopsis(parser, *standard-out*)
    exit-application(0);
  end;

And to access the option values:

.. code-block:: dylan

  let debug? :: <boolean> = option-value-by-long-name(parser, "debug");
  let name :: false-or(<string>) = option-value-by-long-name(parser, "name");
  let dash-r :: <sequence> = option-value-by-short-name(parser, "r");
  let args :: <sequence> = regular-arguments(parser);


Reference
=========

**TODO**

