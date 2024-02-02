Module:       dylan-test-suite
Synopsis:     Dylan test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Core functionality tests (for some arbitrary value of "core").

define test test-<boolean> ()
  //---*** Fill this in...
end test;

define test test-<character> ()
  //---*** Fill this in...
end test;

define test test-<class> ()
  //---*** Fill this in...
end test;

define test test-<object> ()
  //---*** Fill this in...
end test;

define test test-<singleton> ()
  //---*** Fill this in...
end test;

define test test-<symbol> ()
  //---*** Fill this in...
end test;

define test test-<type> ()
  //---*** Fill this in...
end test;


define test test-make ()
  //---*** Fill this in...
end test;

define test test-initialize ()
  //---*** Fill this in...
end test;

define test test-slot-initialized? ()
  //---*** Fill this in...
end test;

define test test-list ()
  //---*** Fill this in...
end test;

define test test-pair ()
  //---*** Fill this in...
end test;

define test test-range ()
  //---*** Fill this in...
end test;

define test test-singleton ()
  //---*** Fill this in...
end test;

define test test-limited ()
  //---*** Fill this in...
end test;

define test test-type-union ()
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
end test;

define test test-vector ()
  //---*** Fill this in...
end test;


/// Function tests
define test test-<function> ()
  //---*** Fill this in...
end test;

define test test-<generic-function> ()
  //---*** Fill this in...
end test;

define test test-<method> ()
  //---*** Fill this in...
end test;


define test test-compose ()
  //---*** Fill this in...
end test;

define test test-complement ()
  //---*** Fill this in...
end test;

define test test-disjoin ()
  //---*** Fill this in...
end test;

define test test-conjoin ()
  //---*** Fill this in...
end test;

define test test-curry ()
  //---*** Fill this in...
end test;

define test test-rcurry ()
  //---*** Fill this in...
end test;

define test test-always ()
  check-false("always(#f)(#t)",
	      always(#f)(#t));
  check-false("always(#f)(10)",
	      always(#f)(10));
  check-true("always(#t)(#t)",
	     always(#t)(#t));
  check-true("always(#t)(10)",
	     always(#t)(10));
end test;



/// Condition tests

define test test-<condition> ()
    test-condition-class(<condition>, abstract?: #t);
end;

define test test-<error> ()
    test-condition-class(<error>, abstract?: #t);
end;

define test test-<sealed-object-error> ()
    test-condition-class(<sealed-object-error>);
end;

define test test-<serious-condition> ()
    test-condition-class(<serious-condition>, abstract?: #t);
end;

define test test-<simple-error> ()
    test-condition-class(<simple-error>);
end;

define test test-<simple-warning> ()
    test-condition-class(<simple-warning>);
end;

define test test-<type-error> ()
    test-condition-class(<type-error>);
end;

define test test-<warning> ()
    test-condition-class(<warning>, abstract?: #t);
end;

define suite dylan-conditions-test-suite ()
  test test-<condition>;
  test test-<error>;
  test test-<sealed-object-error>;
  test test-<serious-condition>;
  test test-<simple-error>;
  test test-<simple-warning>;
  test test-<type-error>;
  test test-<warning>;
end;



/// Equality and comparison functions
define test test-~ () end;
define test test-== () end;
define test test-~== () end;
define test test-= () end;
define test test-~= () end;
define test test-< () end;
define test test-> () end;
define test test-<= () end;
define test test->= () end;
define test test-min () end;
define test test-max () end;

/// Coercing and copying functions
define test test-identity () end;
define test test-values () end;
define test test-as () end;
define test test-as-uppercase () end;
define test test-as-uppercase! () end;
define test test-as-lowercase () end;
define test test-as-lowercase! () end;
define test test-shallow-copy () end;
define test test-type-for-copy () end;

/// Type functions
define test test-instance? () end;
define test test-subtype? () end;
define test test-object-class () end;
define test test-all-superclasses () end;
define test test-direct-superclasses () end;
define test test-direct-subclasses () end;

/// Function handling functions
define test test-generic-function-methods () end;
define test test-add-method () end;
define test test-generic-function-mandatory-keywords () end;
define test test-function-specializers () end;
define test test-function-arguments () end;
define test test-function-return-values () end;
define test test-applicable-method? () end;
define test test-sorted-applicable-methods () end;
define test test-find-method () end;
define test test-remove-method () end;

define suite dylan-core-test-suite ()
  test test-<boolean>;
  test test-<character>;
  test test-<class>;
  test test-<object>;
  test test-<singleton>;
  test test-<symbol>;
  test test-<type>;
  test test-make;
  test test-initialize;
  test test-slot-initialized?;
  test test-list;
  test test-pair;
  test test-range;
  test test-singleton;
  test test-limited;
  test test-type-union;
  test test-vector;
  test test-<function>;
  test test-<generic-function>;
  test test-<method>;
  test test-compose;
  test test-complement;
  test test-disjoin;
  test test-conjoin;
  test test-curry;
  test test-rcurry;
  test test-always;
  test test-~;
  test test-==;
  test test-~==;
  test test-=;
  test test-~=;
  test test-<;
  test test->;
  test test-<=;
  test test->=;
  test test-min;
  test test-max;
  test test-identity;
  test test-values;
  test test-as;
  test test-as-uppercase;
  test test-as-uppercase!;
  test test-as-lowercase;
  test test-as-lowercase!;
  test test-shallow-copy;
  test test-type-for-copy;
  test test-instance?;
  test test-subtype?;
  test test-object-class;
  test test-all-superclasses;
  test test-direct-superclasses;
  test test-direct-subclasses;
  test test-generic-function-methods;
  test test-add-method;
  test test-generic-function-mandatory-keywords;
  test test-function-specializers;
  test test-function-arguments;
  test test-function-return-values;
  test test-applicable-method?;
  test test-sorted-applicable-methods;
  test test-find-method;
  test test-remove-method;

  suite dylan-conditions-test-suite;
end suite dylan-core-test-suite;
