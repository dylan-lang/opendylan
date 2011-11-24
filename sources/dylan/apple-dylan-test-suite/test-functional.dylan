Module:    apple-dylan-test-suite
Filename:  test-functional.dylan
Summary:   Apple Dylan test suite, test-functional
Version:   29-Oct-93
Copyright: (c) 1993 Apple Computer, Inc.

/*---------------------------------------------
Modified by: Shri Amit(amit)
Date: August 24 1996
Summary: Converted to new testworks protocol
Copyright: (c) 1996 Functional Objects, Inc. 
           All rights reserved.  
----------------------------------------------*/

// Chapter 17. Functional Operations

define test compose-0 (description: "")
  check-true("", instance?(first.compose, <function>));
  check-true("", compose(head, tail)(#(1, 2, 3, 4)) = second(#(1, 2, 3, 4)));
end test compose-0;

define test complement-0 (description: "")
  check-true("", instance?(even?.complement, <function>));
  check-true("", even?.complement(52) = odd?(52));
end test complement-0;

define test disjoin-0 (description: "")
  check-true("", instance?(\>.disjoin, <function>));
  check-true("", disjoin(negative?, zero?, positive?)(0));
end test disjoin-0;

define test conjoin-0 (description: "")
  check-true("",instance?(\>.conjoin, <function>));
  check-false("",conjoin(zero?, negative?, positive?)(0));
end test conjoin-0;

define test curry-0 (description: "")
  check-true("",instance?(curry(\==, #"x"), <function>));
  check-true("",method (f)
      instance?(f, <function>)
    end method
      (curry(\==, #"x")));
  check-equal("", map(curry(\-, 10), list(1, 2, 3)), list(9, 8, 7));
  check-equal("", map(curry(list, #"oh", #"bla"), list(#"di", #"da")),
       #(#(#"oh", #"bla", #"di"), #(#"oh", #"bla", #"da")));
end test curry-0;

define test rcurry-0 (description: "")
  check-true("", instance?(rcurry(\==, #"x"), <function>));
  check-equal("", map(rcurry(\-, 1), list(10, 9, 8)), list(9, 8, 7));
  check-equal("", map(rcurry(list, 2, 3), list(#"stretch", #"toes", #"again")),
       #(#(#"stretch", 2, 3), #(#"toes", 2, 3), #(#"again", 2, 3)));
end test rcurry-0;

define test always-0 (description: "")
  check-true("",instance?(always(1), <function>));
  check-true("",always(1)() = 1);
  check-true("",always(1)(99) = 1);
  check-true("",always(1)(99, 98) = 1);
end test always-0;

define suite test-functional-suite () 
  test compose-0;
  test complement-0;
  test disjoin-0;
  test conjoin-0;
  test curry-0;
  test rcurry-0;
  test always-0;
end suite;
