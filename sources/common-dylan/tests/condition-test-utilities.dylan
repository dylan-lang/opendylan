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
  make(class,
       format-string: $condition-string,
       format-arguments: $condition-arguments)
end method make-condition;


/// Condition test functions

define method test-condition (condition :: <condition>) => ()
  test-output("test-condition(<condition>)\n");
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

define method test-condition (condition :: <simple-error>) => ()
  test-output("test-condition(<simple-error>)\n");
  next-method();
  do(method (function) function(condition) end,
     vector(// Functions on <simple-error>
            do-test-condition-format-string,
            do-test-condition-format-arguments
            ))
end method test-condition;

define method test-condition (condition :: <type-error>) => ()
  next-method();
  do(method (function) function(condition) end,
     vector(// Functions on <type-error>
            do-test-type-error-value,
            do-test-type-error-expected-type
            ))
end method test-condition;

define method test-condition (condition :: <simple-warning>) => ()
  next-method();
  do(method (function) function(condition) end,
     vector(// Functions on <simple-warning>
            do-test-condition-format-string,
            do-test-condition-format-arguments
            ))
end method test-condition;

define method test-condition (condition :: <restart>) => ()
  next-method();
  do(method (function) function(condition) end,
     vector(// Generic functions on <restart>
            do-test-restart-query
            ))
end method test-condition;

define method test-condition (condition :: <simple-restart>) => ()
  next-method();
  do(method (function) function(condition) end,
     vector(// Functions on <simple-restart>
            do-test-condition-format-string,
            do-test-condition-format-arguments
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
    (condition :: <condition>) => ()
  let name = format-to-string("%= condition-format-string matches specified format string",
                              condition);
  check-equal(name,
              condition-format-string(condition),
              $condition-string)
end method;

define method do-test-condition-format-arguments
    (condition :: <condition>) => ()
  let name = format-to-string("%= condition-format-arguments match specified format arguments",
                              condition);
  check-equal(name,
              condition-format-arguments(condition),
              $condition-arguments)
end method;

define method do-test-type-error-value (condition :: <condition>) => ()
  //---*** Fill this in...
end method;

define method do-test-type-error-expected-type (condition :: <condition>) => ()
  //---*** Fill this in...
end method;

define method do-test-restart-query (condition :: <condition>) => ()
  //---*** Fill this in...
end method;
