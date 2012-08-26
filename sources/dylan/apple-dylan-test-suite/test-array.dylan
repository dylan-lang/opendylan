Module:    apple-dylan-test-suite
Filename:  test-array.dylan
Summary:   Apple Dylan test suite, test-array
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

// array operations

define test aref-type ()
  check("", instance?, aref, <function>);
  check-false("", instance?(aref, <generic-function>));
end test;

define test aref-1 ()
  check-equal("", #[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0][2], 2);
  let t = stretchy-vector-instance(1, 2, 3);
  check-equal("", t[2], 3);
  let t = "abcdefg";
  check-equal("", t[2], 'c');
  check-equal("", "abc"[1], 'b');
end test;

define test aref-2 ()
  check-equal("", (make(<array>, dimensions: #(3), fill: 5))[1], 5);
end test;

define test aref-set! ()
  let t = #[1, 2, 3];
  t[1] := 0;
  check-equal("", t[1], 0);
  let t = #[1, 2, 3];
  t[1] := 0; 
  check-equal("", t, #[1, 0, 3]);
  check-equal("",  "abc"[1] := as(<character>, 100), 'd');
  let t = make(<stretchy-vector>);
  let t1 = make(<stretchy-vector>);
  map(method (item) add!(t, item) end method, #(1, 0, 0, 0));
  map(method (item) add!(t1, item) end method, #(1, 0, 1, 0));
  t[2] := 1;
  check-equal("", t, t1);
end test;

// aref-setter

define test aref-setter-type ()
  check("instance of function?", instance?, aref-setter, <function>);
  check("instance of generic-function?", instance?, aref-setter, <generic-function>);
end test;

define test aref-setter-1 ()
  let t = #[7, 8, 9];
  aref-setter(5, t, 1);
  check-equal("", t, #[7, 5, 9]);
  let t = "abc";
  t[0] := as(<character>, 100);
  check-equal("", t, "dbc");
end test;

define test dimensions-type (description: "")
  check("", instance?, dimensions, <function>);
  check-false("", instance?(dimensions, <generic-function>));
end test;

define test dimensions-1 ()
  check-equal("", dimensions(#[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]), #(10));
  check-equal("", dimensions("abc"), #(3));
  let t = make(<stretchy-vector>);
  map(method (item) add!(t, item) end method, #(1, 0, 1, 0));
  check-equal("", t.size, 4);
  check-equal("", simple-object-vector-instance().dimensions, #(0));
end test;

define suite test-array-suite ()
 test aref-type;
 test aref-1;
 test aref-2;
 test aref-set!;
 test aref-setter-type;
 test aref-setter-1;
 test dimensions-type;
 test dimensions-1;
end suite; 
