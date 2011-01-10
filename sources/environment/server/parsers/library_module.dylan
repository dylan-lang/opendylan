Module:    Dylan-User
Author:    Hugh Greene
Synopsis:  Parsers for environment commands from external sources.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library environment-server-parsers
  use functional-dylan;
  use io;

  use channels;
  use environment-manager;

  export parser-interface;
  export string-parser;
/* ---*** Expect to add:
	 stream-parser // for TCP, later?; maybe subsumes string parser
	 ole-automation-parser // if needed
*/
end library environment-server-parsers;



/// Command parsers.

define module parser-interface
  use functional-dylan;
  use threads;

  export parse-then-call,
	 parse-commands,
	 <parse-condition>,
	 <parse-warning>,
	 <parse-error>;
end module parser-interface;

// Servers should use exactly one of the following parsers.
// They should rely on the definition of parser-user for an interface.

define module string-parser
  use functional-dylan;
  use threads;
  use format;

  use environment-command-calling;

  use parser-interface, export: all;
end module string-parser;
