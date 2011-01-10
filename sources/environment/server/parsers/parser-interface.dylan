Module:    parser-interface
Author:    Hugh Greene
Synopsis:  Controlling the Environment from external sources.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Parse some input object into a sequence of environment commands
/// and then call the appropriate environment functions.

define sealed generic parse-then-call
    (command-string :: <string>, client-id)
 => ();

/// Parse some input object into a sequence of environment commands.

define sealed generic parse-commands
    (input)
 => (commands :: <sequence>);

/// Condition classes for parsing.

// Superclass of all <condition>s here, to facilitate handling.
define class <parse-condition> (<condition>)
end class;

// Conditions of this class can safely be caught and ignored -- it
// just means that a client command couldn't be parsed coorectly.
// (You probably want to inform the user, though.)
//
// WARNING: The parser always signals such warnings using "error"
// rather than "signal", since it wants to be sure it won't continue
// after the error.  So even though this is a <warning>, someone has
// to handle it.
define class <parse-warning> (<simple-warning>, <parse-condition>)
end class;

// Conditions of this class indicate internal errors in the parser
// code.  They can be safely caught and ignored, but they should be
// reported as bugs by any users.
define class <parse-error> (<simple-error>, <parse-condition>)
end class;
