Module:       carbon-duim
Synopsis:     Macintosh back-end utilities
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Useful constants

// define constant $TRUE  :: <integer> = 1;
// define constant $FALSE :: <integer> = 0;

// define constant $null-gpointer = null-pointer(<gpointer>);


/// Sealing

define sealed class <sealed-constructor-mixin> (<object>) end;

define sealed domain make (subclass(<sealed-constructor-mixin>));
define sealed domain initialize (<sealed-constructor-mixin>);


/// Error handling

define method ensure-no-error
    (name :: <string>, status :: <ffi-integer>)
 => ()
  unless (zero?(status))
    error("%s error %d", name, status)
  end
end method ensure-no-error;

define method ensure-no-error
    (name :: <string>, pointer :: <C-pointer>)
 => (pointer :: <C-pointer>)
  if (null-pointer?(pointer))
    error("%s error: null pointer returned", name)
  end;
  pointer
end method ensure-no-error;

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

// define constant *carbon-debug* = #f;
define variable *carbon-debug* = #t;

define inline-only function carbon-debug (#rest args)
  *carbon-debug* & apply(debug-message, args)
end;


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
