module:       common-dylan-test-suite
Synopsis:     testing of threads
Author:       Tony Mann, Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//////////////
// Testing dynamic-binding
//
// Creates a number of threads which do dynamic-bindings on the same 
// variable. The functions return #t or #f to indicate a good or
// bad result respectively.
//

define thread variable *tv1* :: <symbol> = #"global1";
define thread variable *tv2* :: <symbol> = #"global2";

// Some blocking support to wait for a number of events

define constant *lock* = make(<lock>);
define constant *is-zero* = make(<notification>, lock: *lock*);

define method set-resource-count (n :: <integer>)
  *counter* := - n;
end method;


define method set-resource-count-adding (n :: <integer>, m :: <integer>)
  *counter* := n + m;
end method;


define method free-resource ()
  with-lock (*lock*)
    set-resource-count-adding(*counter*, 1);  // Code around typist bug
    if (*counter* == 0) release-all(*is-zero*) end;
  end with-lock;
end method;

/* 
// Original version which the compiler barfs on

define method free-resource ()
  with-lock (*lock*)
    *counter* := *counter* + 1;
    if (*counter* == 0) release-all(*is-zero*) end;
  end with-lock;
end method;
*/

define method wait-for-resources ()
  with-lock (*lock*)
    while (*counter* ~= 0) 
      wait-for(*is-zero*) 
    end;
  end with-lock;
end method;


define test dynamic-binds-test (description: "Test dynamic bindings")
  let *lock* = make(<lock>);
  let n = 10;
  let iterations = 10;
  let vec = make(<vector>, size: n);

  let make-value
    = method (varnum, threadnum)
        as(<symbol>,
           concatenate(concatenate("local", integer-to-string(varnum)),
                                 integer-to-string(threadnum)));
      end method;
  
  let thread-function 
    = method (val1, val2)
        method ()
          let result1 = #t;
          let result2 = #t;
          let result3 = #t;
          let result4 = #t;
          for (i from 1 to iterations)
            result1 := (result1 & (*tv1* == #"global1"));
            result2 := (result2 & (*tv2* == #"global2"));
            dynamic-bind(*tv1* = val1, *tv2* = val2)
              result3 := (result3 & (*tv1* == val1));
              result4 := (result4 & (*tv2* == val2));
            end dynamic-bind;
          end for;
          free-resource();
          values(result1, result2, result3, result4);
        end method;
      end method;

  set-resource-count(n);

  // Start by binding one of the variables, and assigning the other
  *tv1* := #"master1";
  dynamic-bind (*tv2* = #"master2")
    // Create a whole load of threads and let them do some testing
    for (i from 0 below n)
      vec[i] := make(<thread>, function: thread-function(make-value(1, i),
                                                         make-value(2, i)));
    end for;
    check-true("TV2 in master thread dynamic-bind", *tv2* == #"master2");
    wait-for-resources();
  end dynamic-bind;

  // Check the assignments and binding now have the right value
  check-true("TV1 assignment from master thread", *tv1* == #"master1");
  check-true("TV2 outside master thread dynamic bind", *tv2* == #"global2");

  // Check the results from the threads
  for (i from 0 below n)
    let (rthread, result1, result2, result3, result4) = join-thread(vec[i]);
    check-true(concatenate("Outside bind *tv1*: thread ", integer-to-string(i)),
               result1);
    check-true(concatenate("Outside bind *tv2*: thread ", integer-to-string(i)),
               result2);
    check-true(concatenate("Inside bind *tv1*: thread ", integer-to-string(i)),
               result3);
    check-true(concatenate("Inside bind *tv2*: thread ", integer-to-string(i)),
               result4);
  end for;

end test;


define suite thread-variables-suite (description: "Thread variables")
  test dynamic-binds-test;
end suite thread-variables-suite;
