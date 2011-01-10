Module:       dylan-test-suite
Synopsis:     Dylan test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Core functionality tests

define dylan class-test <boolean> ()
  //---*** Fill this in...
end class-test <boolean>;

define dylan class-test <character> ()
  //---*** Fill this in...
end class-test <character>;

define dylan class-test <class> ()
  //---*** Fill this in...
end class-test <class>;

define dylan class-test <object> ()
  //---*** Fill this in...
end class-test <object>;

define dylan class-test <singleton> ()
  //---*** Fill this in...
end class-test <singleton>;

define dylan class-test <symbol> ()
  //---*** Fill this in...
end class-test <symbol>;

define dylan class-test <type> ()
  //---*** Fill this in...
end class-test <type>;


define dylan function-test make ()
  //---*** Fill this in...
end function-test make;

define dylan function-test initialize ()
  //---*** Fill this in...
end function-test initialize;

define dylan function-test slot-initialized? ()
  //---*** Fill this in...
end function-test slot-initialized?;

define dylan function-test list ()
  //---*** Fill this in...
end function-test list;

define dylan function-test pair ()
  //---*** Fill this in...
end function-test pair;

define dylan function-test range ()
  //---*** Fill this in...
end function-test range;

define dylan function-test singleton ()
  //---*** Fill this in...
end function-test singleton;

define dylan function-test limited ()
  //---*** Fill this in...
end function-test limited;

define dylan function-test type-union ()
  let union = #f;
  check-instance?("type-union(<string>, <integer>) returns a type",
		  <type>,
		  union := type-union(<string>, <integer>));
  check-instance?("\"string\" is an instance of type-union(<string>, <integer>)",
		  union, "string");
  check-instance?("10 is an instance of type-union(<string>, <integer>)",
		  union, 10);
  check-false("instance?(#t, type-union(<string>, <integer>))",
	      instance?(#t, union));
end function-test type-union;

define dylan function-test vector ()
  //---*** Fill this in...
end function-test vector;


/// Function tests
define dylan class-test <function> ()
  //---*** Fill this in...
end class-test <function>;

define dylan class-test <generic-function> ()
  //---*** Fill this in...
end class-test <generic-function>;

define dylan class-test <method> ()
  //---*** Fill this in...
end class-test <method>;


define dylan function-test compose ()
  //---*** Fill this in...
end function-test compose;

define dylan function-test complement ()
  //---*** Fill this in...
end function-test complement;

define dylan function-test disjoin ()
  //---*** Fill this in...
end function-test disjoin;

define dylan function-test conjoin ()
  //---*** Fill this in...
end function-test conjoin;

define dylan function-test curry ()
  //---*** Fill this in...
end function-test curry;

define dylan function-test rcurry ()
  //---*** Fill this in...
end function-test rcurry;

define dylan function-test always ()
  check-false("always(#f)(#t)",
	      always(#f)(#t));
  check-false("always(#f)(10)",
	      always(#f)(10));
  check-true("always(#t)(#t)",
	     always(#t)(#t));
  check-true("always(#t)(10)",
	     always(#t)(10));
end function-test always;



/// Condition tests

define sideways method class-test-function
    (class :: subclass(<condition>)) => (function :: <function>)
  test-condition-class
end method class-test-function;

define open generic test-condition-class
    (class :: subclass(<condition>), #key, #all-keys) => ();

define method test-condition-class
    (class :: subclass(<condition>), #key name, abstract?, #all-keys) => ()
  unless (abstract?)
    test-condition(name, make-condition(class))
  end
end method test-condition-class;

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

define method test-condition
    (name :: <string>, condition :: <condition>) => ()
  // next-method();
  do(method (function) function(name, condition) end,
     vector(// Functions on <condition>
            test-signal,
            test-error,
            test-cerror,
            test-break,

            // Generic functions on <condition>
            test-default-handler,
            test-return-query,
            test-return-allowed?,
            test-return-description
            ))
end method test-condition;

define method test-condition
    (name :: <string>, condition :: <simple-error>) => ()
  // next-method();
  do(method (function) function(name, condition) end,
     vector(// Functions on <simple-error>
            test-condition-format-string,
            test-condition-format-arguments
            ))
end method test-condition;

define method test-condition
    (name :: <string>, condition :: <type-error>) => ()
  // next-method();
  do(method (function) function(name, condition) end,
     vector(// Functions on <type-error>
            test-type-error-value,
            test-type-error-expected-type
            ))
end method test-condition;

define method test-condition
    (name :: <string>, condition :: <simple-warning>) => ()
  // next-method();
  do(method (function) function(name, condition) end,
     vector(// Functions on <simple-warning>
            test-condition-format-string,
            test-condition-format-arguments
            ))
end method test-condition;

define method test-condition
    (name :: <string>, condition :: <restart>) => ()
  // next-method();
  do(method (function) function(name, condition) end,
     vector(// Generic functions on <restart>
            test-restart-query
            ))
end method test-condition;

define method test-condition
    (name :: <string>, condition :: <simple-restart>) => ()
  // next-method();
  do(method (function) function(name, condition) end,
     vector(// Functions on <simple-restart>
            test-condition-format-string,
            test-condition-format-arguments
            ))
end method test-condition;

define method test-signal
    (name :: <string>, condition :: <condition>) => ()
  //---*** Fill this in...
end method test-signal;

define method test-error
    (name :: <string>, condition :: <condition>) => ()
  //---*** Fill this in...
end method test-error;

define method test-cerror
    (name :: <string>, condition :: <condition>) => ()
  //---*** Fill this in...
end method test-cerror;

define method test-break
    (name :: <string>, condition :: <condition>) => ()
  //---*** Fill this in...
end method test-break;

define method test-default-handler
    (name :: <string>, condition :: <condition>) => ()
  //---*** Fill this in...
end method test-default-handler;

define method test-return-query
    (name :: <string>, condition :: <condition>) => ()
  //---*** Fill this in...
end method test-return-query;

define method test-return-allowed?
    (name :: <string>, condition :: <condition>) => ()
  //---*** Fill this in...
end method test-return-allowed?;

define method test-return-description
    (name :: <string>, condition :: <condition>) => ()
  //---*** Fill this in...
end method test-return-description;

define method test-condition-format-string
    (name :: <string>, condition :: <condition>) => ()
  let name
    = format-to-string("%s condition-format-string matches specified format string",
                       name);
  check-equal(name,
              condition-format-string(condition),
              $condition-string)
end method test-condition-format-string;

define method test-condition-format-arguments
    (name :: <string>, condition :: <condition>) => ()
  let name
    = format-to-string("%s condition-format-arguments match specified format arguments",
                       name);
  check-equal(name,
              condition-format-arguments(condition),
              $condition-arguments)
end method test-condition-format-arguments;

define method test-type-error-value
    (name :: <string>, condition :: <condition>) => ()
  //---*** Fill this in...
end method test-type-error-value;

define method test-type-error-expected-type
    (name :: <string>, condition :: <condition>) => ()
  //---*** Fill this in...
end method test-type-error-expected-type;

define method test-restart-query
    (name :: <string>, condition :: <condition>) => ()
  //---*** Fill this in...
end method test-restart-query;


/// Don't test the functions we're already testing... there must be a better way!

/// Equality and comparison functions
define dylan function-test \~ () end;
define dylan function-test \== () end;
define dylan function-test \~== () end;
define dylan function-test \= () end;
define dylan function-test \~= () end;
define dylan function-test \< () end;
define dylan function-test \> () end;
define dylan function-test \<= () end;
define dylan function-test \>= () end;
define dylan function-test min () end;
define dylan function-test max () end;

/// Coercing and copying functions
define dylan function-test identity () end;
define dylan function-test values () end;
define dylan function-test as () end;
define dylan function-test as-uppercase () end;
define dylan function-test as-uppercase! () end;
define dylan function-test as-lowercase () end;
define dylan function-test as-lowercase! () end;
define dylan function-test shallow-copy () end;
define dylan function-test type-for-copy () end;

/// Type functions
define dylan function-test instance? () end;
define dylan function-test subtype? () end;
define dylan function-test object-class () end;
define dylan function-test all-superclasses () end;
define dylan function-test direct-superclasses () end;
define dylan function-test direct-subclasses () end;

/// Function handling functions
define dylan function-test apply () end;
define dylan function-test generic-function-methods () end;
define dylan function-test add-method () end;
define dylan function-test generic-function-mandatory-keywords () end;
define dylan function-test function-specializers () end;
define dylan function-test function-arguments () end;
define dylan function-test function-return-values () end;
define dylan function-test applicable-method? () end;
define dylan function-test sorted-applicable-methods () end;
define dylan function-test find-method () end;
define dylan function-test remove-method () end;

// Condition functions
define conditions function-test signal () end;
define conditions function-test error () end;
define conditions function-test cerror () end;
define conditions function-test break () end;
define conditions function-test check-type () end;
define conditions function-test abort () end;
define conditions function-test default-handler () end;
define conditions function-test restart-query () end;
define conditions function-test return-query () end;
define conditions function-test do-handlers () end;
define conditions function-test return-allowed? () end;
define conditions function-test return-description () end;
define conditions function-test condition-format-string () end;
define conditions function-test condition-format-arguments () end;
define conditions function-test type-error-value () end;
define conditions function-test type-error-expected-type () end;
