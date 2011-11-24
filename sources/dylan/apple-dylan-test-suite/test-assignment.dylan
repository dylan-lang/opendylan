Module:    apple-dylan-test-suite
Filename:  test-assignment.dylan
Summary:   Apple Dylan test suite, test-assignment
Version:   29-Oct-93
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*---------------------------------------------
Modified by: Shri Amit(amit) &
	     James Kirsch(jkirsch)
Date: August 24 1996
Summary: Converted to new testworks protocol
Copyright: (c) 1996 Functional Objects, Inc. 
           All rights reserved.  
----------------------------------------------*/

// initialize
// chapter 7. Assignment

define constant *var-for-set* = 0;

define test set!-1 ()
    let foo = #(#"a", #"b", #"c", #"d");
    foo[2] := #"sea";
    check-equal("", #"sea", foo[2]);
    let *var-for-set* = 100;
    *var-for-set* := 5;
    check-equal("", *var-for-set*, 5);
    check("", \~=, *var-for-set*, 0);
end test;


define test setter-type ()
    check("", instance?, element-setter, <generic-function>);
end test;

define test setter-1 ()
  let foo = as(<vector>, #(#"a", #"b", #"c", #"d"));
  element-setter(#"sea", foo, 2);
  check-equal("", instance?(element-setter, <generic-function>) & foo[2], #"sea");
end test;

define suite test-assignment-suite ()
  test set!-1;
  test setter-type;
  test setter-1;
end suite;























