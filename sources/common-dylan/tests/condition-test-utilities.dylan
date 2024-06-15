Module: common-dylan-test-utilities
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define open generic test-condition-class
    (class :: subclass(<condition>), #key, #all-keys) => ();

define method test-condition-class
    (class :: subclass(<condition>), #key abstract?, #all-keys) => ()
  unless (abstract?)
    test-condition(make-condition(class))
  end
end method;

define constant $condition-string    = "%d ~= %d";
define constant $condition-arguments = #(1, 2);

define method make-condition
    (class :: subclass(<condition>))
 => (condition :: <condition>)
  make(class)
end method;

define method make-condition
    (class :: subclass(<simple-condition>))
 => (condition :: <simple-condition>)
  make(class,
       format-string: $condition-string,
       format-arguments: $condition-arguments)
end method;


/// Condition test functions

define method test-condition (condition :: <condition>) => ()
  do(method (function) function(condition) end,
     vector(// Functions on <condition>
            do-test-signal,
            do-test-error,
            do-test-cerror,
            do-test-break,

            // Generic functions on <condition>
            do-test-default-handler,
            do-test-return-query,
            do-test-return-allowed?,
            do-test-return-description
            ))
end method test-condition;

define method test-condition (condition :: <simple-condition>) => ()
  next-method();
  do(method (function) function(condition) end,
     vector(// Functions on <simple-condition>
            do-test-condition-format-string,
            do-test-condition-format-arguments
            ))
end method test-condition;

define method test-condition (condition :: <type-error>) => ()
  // The purpose of this method is to prevent next-method() from being called.
  // do-test-condition-format-{string,arguments} don't work for <type-error> because
  // its format string and args are computed from the value and type init args.

  //next-method();
end method test-condition;

define method make-condition (class == <type-error>) => (c :: <type-error>)
  make(<type-error>,
       value: #"type-error-value",
       type: <string>)
end method;

define method test-condition (condition :: <restart>) => ()
  next-method();
  do(method (function) function(condition) end,
     vector(// Generic functions on <restart>
            do-test-restart-query
            ))
end method test-condition;

define method do-test-signal (condition :: <condition>) => ()
  //---*** Fill this in...
end method;

define method do-test-error (condition :: <condition>) => ()
  //---*** Fill this in...
end method;

define method do-test-cerror (condition :: <condition>) => ()
  //---*** Fill this in...
end method;

define method do-test-break (condition :: <condition>) => ()
  //---*** Fill this in...
end method;

define method do-test-default-handler (condition :: <condition>) => ()
  //---*** Fill this in...
end method;

define method do-test-return-query (condition :: <condition>) => ()
  //---*** Fill this in...
end method;

define method do-test-return-allowed? (condition :: <condition>) => ()
  //---*** Fill this in...
end method;

define method do-test-return-description (condition :: <condition>) => ()
  //---*** Fill this in...
end method;

define method do-test-condition-format-string
    (condition :: <simple-condition>) => ()
  let name = format-to-string("%= condition-format-string matches specified format string",
                              condition);
  check-equal(name,
              condition-format-string(condition),
              $condition-string)
end method;

define method do-test-condition-format-arguments
    (condition :: <simple-condition>) => ()
  let name = format-to-string("%= condition-format-arguments match specified format arguments",
                              condition);
  check-equal(name,
              condition-format-arguments(condition),
              $condition-arguments)
end method;

define method do-test-restart-query (condition :: <condition>) => ()
  //---*** Fill this in...
end method;
