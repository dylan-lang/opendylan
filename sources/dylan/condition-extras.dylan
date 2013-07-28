Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// BREAKS

define generic %break (cond-or-string, #rest arguments);

define method break (#rest arguments)
  block ()
    if (arguments.empty?)
      %break("Break.")
    else
      apply(%break, arguments)
    end if
  exception (<simple-restart>,
             init-arguments: vector(format-string: "Return from break."))
  end block;
  #f;
end method break;

define method %break (condition :: <condition>, #rest noise)
  unless (noise.empty?)
    error("Can only supply format arguments when supplying a format string.")
  end unless;
  condition.invoke-debugger
end method %break;

define method %break (string :: <string>, #rest arguments)
  %break(make(<simple-warning>,
              format-string: string,
              format-arguments: arguments))
end method %break;

/// RESTARTS

define open generic restart-query (restart);

define method restart-query (restart :: <restart>)
  #f
end method restart-query;

define open generic return-query (condition);

define open abstract class <restart> (<condition>)
end class <restart>;

define open class <simple-restart> (<restart>, <simple-condition>)
end class <simple-restart>;

define method cerror (restart-descr, cond-or-string, #rest arguments)
  block ()
    apply(error, cond-or-string, arguments)
  exception (<simple-restart>,
             init-arguments:
               vector(format-string: restart-descr,
                      format-arguments: arguments))
    #f
  end block
end method cerror;

define method default-handler (restart :: <restart>)
  error("No restart handler for %=", restart);
  #f
end method default-handler;

/// RETURN ALLOWED?

define open generic return-allowed? (condition);

define method return-allowed? (condition :: <condition>)
  #f
end method return-allowed?;

define open generic return-description (condition);

/// ABORTS

define open class <abort> (<restart>)
end class <abort>;

define method abort ()
 => (will-never-return :: <bottom>)
  error(make(<abort>))
end method abort;

/// TYPE-ERRORS

define open class <type-error> (<error>, <format-string-condition>)  // Should be sealed?
  constant slot type-error-value, init-keyword: value:;
  constant slot type-error-expected-type :: <type>, init-keyword: type:;
end class <type-error>;

define method make
    (class == <type-error>, #rest keys, #key value, type)
 => (error :: <type-error>)
  apply(next-method, class,
        format-string: "%= is not of type %=",
        format-arguments: vector(value, type),
        keys)
end method make;

define inline method check-type (value, type)
  unless (instance?(value, type))
    error(make(<type-error>, value: value, type: type))
  end unless;
  value
end method check-type;

/// SEALED-OBJECT ERRORS

define class <sealed-object-error> (<error> /* <warning> */) end;

//// Introspection.

define method do-handlers (function)
  local method always-true (x)
          #t
        end method;
  local method do-with-handlers (handlers :: <list>)
          for (_handler in handlers)
            function(_handler.handler-type,
                     _handler.handler-test | always-true,
                     _handler.handler-function,
                     _handler.handler-init-arguments);
          end for;
        end method;
  do-with-handlers(*current-handlers*);
  do-with-handlers(*last-handlers*);
end method do-handlers;
