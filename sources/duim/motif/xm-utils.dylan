Module:    motif-duim
Synopsis:  Motif back-end utilities
Author:    Scott McKay, Stuart Croy
	   Based on work by John Aspinall and Richard Billington
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///---*** Hacked version of size-of to avoid inlining

define function safe-size-of (value) => (size :: <integer>)
  size-of(value)
end function safe-size-of;


/// Error handling

define function not-yet-implemented
    (format-message :: <string>, #rest format-args)
  apply(error, 
	concatenate(format-message, " not yet implemented!"),
	format-args)
end function not-yet-implemented;

define function ignoring
    (format-message :: <string>, #rest format-args)
  apply(debug-message, 
	concatenate("Ignoring ", format-message),
	format-args)
end function ignoring;

// A simpler debug message since the console debugger is too slow!
define function motif-debug-message
    (format-string :: <string>, #rest format-arguments) => ()
  let message
    = block ()
	apply(format-to-string, format-string, format-arguments)
      exception (error :: <error>)
	block ()
	  format-to-string("*** debug-message crashed: %s", error)
	exception (error :: <error>)
	  "*** debug-message crashed"
	end
      end;
  OutputDebugString(concatenate(message, "\n"))
end function motif-debug-message;


/// String conversion utilities

// We're running on Unix where the newline convention is '\n'
define sealed inline method convert-to-native-newlines
    (string :: <byte-string>)
 => (new-string :: <byte-string>)
  string
end method convert-to-native-newlines;

define method convert-from-native-newlines
    (string :: <byte-string>)
 => (new-string :: <byte-string>)
  string
end method convert-from-native-newlines;
