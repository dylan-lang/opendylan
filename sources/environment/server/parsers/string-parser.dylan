Module:    string-parser
Author:    Hugh Greene
Synopsis:  Controlling the Environment from external sources.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Global state for the parser functions below.
// The string doesn't need to be global, but it keeps the state together.

define thread variable *string* :: <string> = "";
define thread variable *string-size* :: <integer> = 0;
define thread variable *index* :: <integer> = 0;


// Parse a string containing one or more commands, of the form
// 
//	[_command-name_(_argument_,...)]
//
// *No whitespace* is allowed, except within _argument_s.  If an
// _argument_ is to contain ',' or ')', it must be enclosed in '"'s.
// Any pair of characters "\X" will be interpreted as 'X', within a
// string delimited by '"'.  Several commands may be contained in one
// string, with no whitespace between them.
//
// Examples:
//	[foo(bar,baz)]		--> "foo", "bar", "baz"
//	[foo(bar, baz)]		--> "foo", "bar", " baz"
//	[foo("(1,2)",\]		--> "foo", "(1,2)", "\\"
//	[foo("(1,2)","\\"]	--> "foo", "(1,2)", "\\"
//	[foo("(1,2)","\b"]	--> "foo", "(1,2)", "b" // NOT "\b"
//	[foo((1,2),"\\"]	--> <parse-warning>
//	[foo("(1,2)","\"]	--> <parse-warning>
//	foo(bar,baz)		--> <parse-warning>
//	[foo(1)][bar(2,3)]	--> "foo", "1"; "bar", "2", "3"
//	[foo(1)] [bar(2,3)]	--> <parse-warning>
//
// Any problems will result in a <parse-warning> being signalled.
// Some internal bugs might result in a <parse-error> being
// signalled.  See parser-interface.dylan for what to do with these.
//
// NOTE: This syntax is roughly what appears to be the typical format
// of Win32 DDE "XTYP_EXECUTE" command strings.
//
// See [some as-yet-unwritten bit of environment doc] for the format
// of commands accepted by the DylanWorks servers.
//
// ** MECHANISM
//
// 1. Parse first word out of command string
// 2. Parse remaining arguments as strings, possibly in '"'.
// 3. Use lookup-and-coerced-apply from the environment-commands
//    module of the environment-manager module to coerce the strings
//    to the correct type and apply the correct registered function to
//    the result.
//---*** cpage: 1998.04.10 I changed this to accept some additional forms:
//
//              []         // An empty command, which is ignored
//              [command]  // A command that takes no arguments
//
//              We also need to loosen this up to allow whitespace around
//              '[', ']', '(', and ')'. I'm not sure whether white space
//              around ',' should be ignored -- currently, it is made
//              part of the adjoining command string.

// -=- Main Entry Point -=-
//
// Takes a string containing commands and an object identifying the
// caller.
// Parses the commands and calls (environment) functions as
// appropriate.
// Returns nothing.
define method parse-then-call
    (command-string :: <string>, client-id) => ()
  debug-message("  Parsing commands from string %= {", command-string);
  let commands = parse-commands(command-string);
  debug-message("  } got %d; calling commands {", commands.size);
  for (cmd in commands)
    lookup-and-coerced-apply
      (head(cmd), concatenate(vector(client-id), tail(cmd)));
  end for;
  debug-message("  }");
end method;


// This, a lower-level entry point, parses a string containing a
// sequence of commands, with no whitespace between them.
define method parse-commands
    (input-string :: <string>)
 => (commands :: <sequence>);
  let commands :: <stretchy-vector> = make(<stretchy-vector>);
  *string* := input-string;
  *string-size* := *string*.size;
  *index* := 0;
  block ()
    while(*index* < *string-size*)
      let (cmd-name, cmd-args) = parse-1-command();
      if (cmd-name ~= #"")
	commands := add!(commands, pair(cmd-name, cmd-args));
      end if;
    end while;
  exception (<parse-warning>)
    // Do nothing -- just ignore the warning
    //---*** cpage: 1998.04.09 I added this exception handler in order
    //              to be lenient of parsing errors. This may not be
    //              correct in the long term, but until we can provide
    //              better handling and UI for errors, this prevents
    //              simple string formatting errors from crashing
    //              the environment.
  end;
  commands
end method;

// Parse one whole command from *string*, updating *index* to point past
// the end if successful.
define method parse-1-command
    ()
 => (command-name :: <symbol>,
     command-args :: limited(<sequence>, of: <string>))
  debug-message("    Parsing command {");
  if ( *string*[*index*] ~== '[' )
    not-found-error($command-place-name, "[");
  end if;
  *index* := *index* + 1;

  let command-name = as(<symbol>, parse-word("(]"));
  debug-message("      command-name = %=", command-name);

  let args = as(limited(<vector>, of: <string>), #[]);
  if (command-name ~= #"" & *string*[*index*] = '(')
    *index* := *index* + 1;
    args := parse-arguments();
    debug-message("      args = %=", as(<simple-vector>, args));
  end if;

  if ( *string*[*index*] ~== ']' )
    not-found-error($command-place-name, "]");
  end if;
  *index* := *index* + 1;

  debug-message("    }");
  values(command-name, args)
end method;



/// -=- Mini-parsers for bits of the command string format -=-

// Parses the sequence "<some-string> ',' ... ')'".
// Returns a sequence of strings.
// Updates the global *index* to point past the end of the matched section.
//
// Each <some-string> may or may not be delimited by '"'; i.e.
//   [do-thing(1,"foo,baz,zom",bar)]
// is a valid command, with the argument sequence
//   "1", "\"foo,baz,zom\"", "bar"
define method parse-arguments
    ()
 => (args :: limited(<stretchy-vector>, of: <string>))
  let args :: limited(<stretchy-vector>, of: <string>)
    = make(limited(<stretchy-vector>, of: <string>), size: 0);

  until ( *index* >= *string-size* | *string*[*index*] == ')' )
    let arg = parse-string();
    debug-message
      ("      parsed arg string %=, *index* = %=", arg, *index*);
    add!(args, arg);
    if ( *index* >= *string-size*
         | ( *string*[*index*] ~== ','
             & *string*[*index*] ~== ')' ) )
      not-found-error($args-place-name, ",)");
    end if;
    if ( *string*[*index*] == ',' )
      *index* := *index* + 1; // Move past the ','.
    end if;
  end until;
  if ( *index* >= *string-size* | *string*[*index*] ~== ')' )
    not-found-error($args-place-name, ")");
  else
    *index* := *index* + 1;
    args
  end if;
end method;



/// -=- Auxilliary functions for the parser -=-

// A function like find-key which lets you search a particular
// subsequence, instead of allowing you to skip N occurences.
// ---*** Need proper tests for this function.
define function string-parser-find-key
    (sequence :: <sequence>, pred :: <function>,
     #key start: _start = 0, end: _end = sequence.size,
          failure = #f)
 => (found-key :: <object>)
  // Range checking.
  let sequence-size = sequence.size;
  if ( _start < 0 | _end > sequence-size )
    error(make(<parse-error>,
      format-string:
        "Bounds out of range in parsing %=:\n"
        "start = %=, end = %=, size = %=",
      format-arguments:
        vector(*string*, _start, _end, sequence-size)));
  end if;
  if ( _end <= _start )
    error(make(<parse-error>,
      format-string:
        "Impossible bounds in parsing %=\n: start = %=, end = %=",
      format-arguments:
        vector(*string*, _start, _end)));
  end if;

  // Now the real work.
  block(return)
    for ( key from _start below _end )
      pred(sequence[key]) & return(key);
    end for;
    failure
  end
end function;


// Parses a "<word> C" sequence, for any character "C".
// Returns the <word> as a <string>.
// Updates the global *index* to point at "C".
define method parse-word
    (end-markers :: <string>)
 => (command-name :: <string>)
  let word-end
    = string-parser-find-key
        (*string*, rcurry(member?, end-markers), start: *index*);
  if (word-end)
    block()
      copy-sequence(*string*, start: *index*, end: word-end)
    afterwards
      *index* := word-end;
    end block;
  else
    // Change *index*, to get a more sensible error message.
    *index* := *string-size* - 1;
    not-found-error($args-place-name, end-markers);
  end if;
end method;


// Parse a string which may or may not be '"'-delimited.
// Leaves *index* pointing just after the string.
define function parse-string () => (string :: <string>)
  if ( *string*[*index*] == '"' )
    let (string-start, string-end)
      = parse-delimited-string(*string*, start: *index*);
    *index* := string-end + 1; // Skip past the closing '"'.
    copy-sequence(*string*, start: string-start, end: string-end)
  else
    parse-word(",)")
  end if
end function;


// Parse a (sub-)sequence of characters within a "string", using given
// delimiter and escape characters (i.e., the escape character makes the
// the parser skip the following character, allowing delimiters to
// appear within the parsed subsequence.
//
// Returns the (inclusive) start and (exclusive) end of a sequence
// matching the pattern of a string, starting at the given position, or
// (#f, #f) if there is none.
// The delimiter characters are NOT included in the matched sequence.
//
// This function does not relate to the global state.
define method parse-delimited-string
    (string :: <string>,
     #key start: _start = 0, end: _end = string.size,
          delimiter = '"', escape = '\\')
 => (match-start :: false-or(<integer>), match-end :: false-or(<integer>))
  local method process-char (i)
          if (i < _end)
            select (string[i])
              delimiter => values(_start + 1, i);
              escape    =>
                if ((i + 1) < _end)
                  process-char(i + 2);
                else
                  error(make(<parse-warning>,
                    format-string:
                      "Escape character at end of delimited string."));
                end if;
              otherwise => process-char(i + 1);
            end select;
          else
            values(#f, #f);
          end if;
        end method;
  if (string[_start] == delimiter)
    process-char(_start + 1);
  else
    values(#f, #f);
  end if;
end method;



/// -=- Simplified error handling -=-

// Quick way to signal a <parse-warning> for some unfound symbol.
// This never returns.

define constant $command-place-name :: <string> = "command";
define constant $args-place-name :: <string> = "argument list";

define function not-found-error
    (where :: <string>, what :: <string>)
// NOTE: No return value declaration, to avoid type warnings.
  // Build up the format string.
  let first-phrase :: <string> = "";
  let what-format :: <string> = "";
  if ( what.size == 1 )
    what-format := "%="
  else
    first-phrase := "one of";
    what-format := "%= or %=";
    for ( i from 2 below what.size )
      what-format := concatenate("%=, ", what-format);
    end for;
  end;

  // Signal the error.
  error
    (make
       (<parse-warning>,
	format-string:
	  concatenate("%s %s ", what-format, " not found at position %=."),
	format-arguments:
	  concatenate(vector(first-phrase, where), what, vector(*index*))));
end function;
