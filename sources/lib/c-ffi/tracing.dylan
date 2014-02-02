Module:    c-ffi-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Dylan Hackers.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// If this is set to #t, all calls to foreign functions are traced.
// This is done by calling a function log-entry(c-function-name, #rest
// args) on function entry, and log-exit(c-function-name, #rest
// results) on function exit.  You need to provide these functions in
// the lexical scope of the "define C-function".  Empty stubs are
// provided here, in case you don't want to trace all your FFI
// libraries.
//
//You can easily use those definitions in a client library and
//exclude log-entry and log-exit from c-ffi.
/*
define inline-only function log-entry(c-function-name, #rest args) => ();
  format-out("entering %s %=", c-function-name, args);
end;
define inline-only function log-exit(c-function-name, #rest results) => ();
  format-out(" => %=\n", results);
end;
*/

define constant $trace-ffi-calls = #f;

define inline-only function log-entry(c-function-name, #rest args) => (); end;
define inline-only function log-exit(c-function-name, #rest results) => (); end;
