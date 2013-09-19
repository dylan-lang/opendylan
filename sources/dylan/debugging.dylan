Module:    internal
Synopsis:  Debugging support for the Dylan library
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// DEBUGGING

define variable *debugging?* :: <boolean>      = #f;
define variable *debug-parts* :: <list>        = #();

define sealed inline method debugging?
    () => (debugging? :: <boolean>)
  *debugging?*
end method debugging?;

define sealed inline method debugging?-setter
    (debugging? :: <boolean>) => (debugging? :: <boolean>)
  *debugging?* := debugging?
end method debugging?-setter;

define sealed inline method debug-parts
    () => (parts :: <list>)
  *debug-parts*
end method debug-parts;

define sealed inline method debug-parts-setter
    (parts :: <list>) => (parts :: <list>)
  *debug-parts* := parts
end method debug-parts-setter;

define sealed inline method debugging-part?
    (part :: <symbol>) => (debugging? :: <boolean>)
  debugging?() & ~empty?(debug-parts()) & member?(part, debug-parts())
end method debugging-part?;

/// DEBUG MESSAGE
///
/// This is a function and is set up to avoid any generic dispatch
/// so that it can be used at any point in time, including inside
/// dispatch.
define function debug-message
    (format-string :: <byte-string>, #rest format-args) => ()
  primitive-debug-message(format-string, format-args);
end function debug-message;

/// DEBUG OUT
///
/// Conditional debugging output.  E.g. debug-out(#"linker", "Foo")
/// will output "Foo" only if debugging the part #"linker".

define variable *debug-out-function* :: <function> = default-debug-out;

define macro debug-out
  { debug-out (?key:expression, ?args:*) }
    => { if (debugging-part?(?key))
           debug-out-function()(method () vector(?args) end)
         end }
end macro debug-out;

define sealed inline method debug-out-function
    () => (function :: <function>)
  *debug-out-function*
end method debug-out-function;

define sealed inline method debug-out-function-setter
    (function :: <function>) => (function :: <function>)
  *debug-out-function* := function
end method debug-out-function-setter;

define function default-debug-out
    (closure :: <function>) => ()
  let arguments = closure();
  apply(debug-message, arguments)
end function default-debug-out;

/// ASSERTIONS

define macro assert
 { assert(?value:expression) }
    => { unless (?value)
           assertion-failure("no reason supplied")
         end }
 { assert(?value:expression, ?format-string:expression, ?format-arguments:*) }
    => { unless (?value)
           assertion-failure(?format-string, ?format-arguments)
         end }
end macro assert;

define macro debug-assert
 { debug-assert(?value:expression) }
    => { if (debugging?() & ~?value)
           debug-assertion-failure("no reason supplied")
         end }
 { debug-assert(?value:expression, ?format-string:expression, ?format-arguments:*) }
    => { if (debugging?() & ~?value)
           debug-assertion-failure(?format-string, ?format-arguments)
         end }
end macro debug-assert;

/// ASSERTION CONDITIONS

define class <assert-error> (<simple-error>)
end class <assert-error>;

define function assertion-failure
    (format-string :: <string>, #rest format-arguments)
  let format-string
    = concatenate-as(<string>, "Assertion failed: ", format-string);
  error(make(<assert-error>,
             format-string: format-string,
             format-arguments: format-arguments))
end function assertion-failure;

define function debug-assertion-failure
    (format-string :: <string>, #rest format-arguments) => ()
  let format-string
    = concatenate-as(<string>, "Debug assertion failed: ", format-string);
  cerror("Carry on regardless",
         make(<assert-error>,
              format-string: format-string,
              format-arguments: format-arguments))
end function debug-assertion-failure;

