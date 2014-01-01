Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// HANDLERS

define primary class <handler> (<object>)
  constant slot handler-type, init-keyword: type:;
  constant slot handler-function, init-keyword: function:;
  constant slot handler-test, init-keyword: test:;
  constant slot handler-init-arguments, init-keyword: init-arguments:;
end class <handler>;

define sealed domain make (singleton(<handler>));
define sealed domain initialize (<handler>);

define thread variable *current-handlers* :: <list> = #();

define variable *last-handlers* :: <list> = #();

define variable *last-handler* :: false-or(<handler>) = #f; // ---*** maintained for patching

define function add-last-handler (_handler :: <handler>) => ()
  *last-handlers* := pair(_handler, *last-handlers*)
end function;

define function remove-last-handler ()
  unless (empty?(*last-handlers*))
    *last-handlers* := tail(*last-handlers*)
  end unless;
end function;

define inline function make-handler (type, function, #key test, init-arguments)
  make(<handler>,
       type: type,
       function: function,
       test: test,
       init-arguments: init-arguments | #[])
end function make-handler;

////
//// CONDITION CLASSES.
////

define open abstract class <condition> (<object>)
end class <condition>;

define generic signal (cond-or-string, #rest arguments);

define generic error (cond-or-string, #rest arguments)
 => (will-never-return :: <bottom>);

define open generic default-handler (condition :: <condition>);

/// CONDITIONS

define open abstract primary class <simple-condition> (<condition>)
  constant slot condition-format-string, init-keyword: format-string:, init-value: "";
  constant slot condition-format-arguments, init-keyword: format-arguments:, init-value: #[];
end class <simple-condition>;

define constant <format-string-condition> = <simple-condition>;

// debug-message and its C primitive require the arguments as a vector.
define function condition-format-arguments-vector
    (condition :: <simple-condition>) => (vector :: <simple-object-vector>)
  as(<simple-object-vector>, condition.condition-format-arguments)
end function;

define method signal (condition :: <condition>, #rest noise)
  unless (empty?(noise))
    error("Can only supply format arguments when supplying a format string.")
  end unless;
  let done-last? = #f;
  iterate search (handlers = *current-handlers*)
    if (empty?(handlers))
      if (done-last?)
        default-handler(condition)
      else
        done-last? := #t;
        search(*last-handlers*)
      end if;
    else
      let _handler = head(handlers);
      let remaining = tail(handlers);
      if (handler-matches?(_handler, condition))
        handler-function(_handler)
          (condition, method () search(remaining) end method)
      else
        search(remaining)
      end if
    end if
  end iterate
end method signal;

define method handler-matches? (_handler :: <handler>, condition :: <condition>)
  => (matches? :: <boolean>)
  instance?(condition, handler-type(_handler))
  & begin
      let test = handler-test(_handler);
      ~test | test(condition)
    end
end method handler-matches?;

define method error (condition :: <condition>, #rest noise)
 => (will-never-return :: <bottom>)
  unless (noise.empty?)
    error("Can only supply format arguments when supplying a format string.")
  end unless;
  signal(condition);
  // (invoke-debugger cond)
  error("Error may not return");
end method error;

define method default-handler (condition :: <condition>)
  #f
end method default-handler;

/// SERIOUS CONDITIONS

define open abstract class <serious-condition> (<condition>)
end class <serious-condition>;

define method default-handler (condition :: <serious-condition>)
  condition.invoke-debugger;
  #f
end method default-handler;

/// ERRORS

define open abstract class <error> (<serious-condition>)
end class <error>;

define open class <simple-error> (<error>, <simple-condition>)
end class <simple-error>;

define method error (string :: <string>, #rest arguments)
 => (will-never-return :: <bottom>)
  error(make(<simple-error>,
             format-string: string,
             format-arguments: arguments))
end method error;

/// WARNINGS

define open abstract class <warning> (<condition>)
end class <warning>;

define open class <simple-warning> (<warning>, <simple-condition>)
end class <simple-warning>;

define method signal (string :: <string>, #rest arguments)
  signal(make(<simple-warning>,
              format-string: string,
              format-arguments: arguments))
end method signal;


/// LAST-HANDLER
///
/// ---*** put in "*last-handler* := #f;" for patching

define macro last-handler-definer
  { define last-handler (?condition:expression, ?args:*) = ?handler:expression }
    => { *last-handler* := #f;
         add-last-handler(make-last-handler(?condition, ?handler, ?args)) }

  { define last-handler ?condition:* = ?handler:expression }
    => { *last-handler* := #f;
         add-last-handler(make-last-handler(?condition, ?handler)) }

  { define last-handler }
    => { *last-handler* := #f;
         remove-last-handler() }

  condition: // hack to avoid eating "=" and RHS
  { ?expression:expression }
    => { ?expression }
end macro last-handler-definer;

define function default-last-handler-test
    (condition :: <condition>) => (handle? :: <boolean>)
  ignore(condition);
  ~inside-debugger?()
end function default-last-handler-test;

define function make-last-handler
    (type, function,
     #key test = default-last-handler-test, init-arguments)
  make-handler(type, function,
               test: test,
               init-arguments: init-arguments)
end function make-last-handler;
