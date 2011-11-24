Module:    apple-dylan-test-suite
Filename:  test-intro-mop.dylan
Summary:   Apple Dylan test suite, test-intro-mop
Version:   29-Oct-93
Copyright: (c) 1993 Apple Computer, Inc.

/*---------------------------------------------
Modified by: Shri Amit(amit)
Date: August 24 1996
Summary: Converted to new testworks protocol
Copyright: (c) 1996 Functional Objects, Inc. 
           All rights reserved.  
----------------------------------------------*/

// Chapter 6: Introduction to Functions and Classes
// test define-class, setters & getters (below).

define class t-c (<object>)
  slot name :: <string>, setter: set-name, init-keyword: name:;
  slot balance :: <integer>,
    init-value: 0,
    setter: set-balance,
    getter: get-balance,
    init-keyword: balance:;
  slot interest-rate, init-value: 6, setter: set-interest-rate;
end class t-c;

define class t-subc1 (t-c)
  slot password :: <string>, setter: set-password, 
		getter: get-password, init-keyword: password:;
end class t-subc1;

// KJP: Put set-initial-limit before its ref

define method set-initial-limit (#rest args)
  200
end method set-initial-limit;

define class t-subc2 (t-c)
  class slot limit :: <integer>,
    init-function: set-initial-limit,
    setter: set-limit,
    getter: get-limit,
    init-keyword: limit:;
end class t-subc2;

define class t-sub-sub (t-subc1, t-subc2)
end class t-sub-sub;

define method withdraw (acct :: t-subc1, amt, #key passwd)
  if (passwd ~= acct.get-password)
    #"wrong-password"
  else
    next-method()
  end if
end method withdraw;

define method withdraw (acct :: t-subc2, amt, #key passwd)
  if (amt > acct.get-limit)
    #"over-limit"
  else
    next-method()
  end if
end method withdraw;

define method withdraw (acct :: t-c, amt, #key passwd)
  if (amt > acct.get-balance)
    #"insufficient-funds"
  else
    set-balance((acct.get-balance - amt), acct);
  end if
end method withdraw;

define method deposit (acct :: t-c, amt)
  set-balance((acct.get-balance + amt), acct);
end method deposit;

define constant cust1
  = make(t-subc1, name: "Joe Nickson", balance: 210, password: "Nickson");

define constant cust2
  = make(t-subc2, name: "Ann Rosenburg", balance: 300, limit: 100);

define constant cust3
  = make(t-sub-sub,
         name: "Bruce Lee",
         balance: 2000,
         limit: 800,
         password: "7654321");

define test slot-operations (description: "")
  check-true("", 180 = withdraw(cust1, 30, passwd: "Nickson"));
  check-true("", 250 = withdraw(cust2, 50));
  check-true("", 320 = deposit(cust2, 70));
  check-true("", 1980 = withdraw(cust3, 20, passwd: "7654321"));
  check-true("", withdraw(cust1, 220, passwd: "wow") = #"wrong-password");
  check-true("", withdraw(cust1, 220, passwd: "Nickson") = #"insufficient-funds");
  check-true("", cust1.get-balance = 180);
  check-true("", withdraw(cust2, 150) = 170);
  check-true("", cust2.get-balance = 170);
  check-true("", withdraw(cust3, 200) = #"wrong-password");
  check-true("", withdraw(cust3, 900, passwd: "7654321") = #"over-limit");
  check-true("", cust3.get-balance = 1980);
end test slot-operations;

define test method-type (description: "")
  check-false("", subtype?(<method>, <generic-function>));

// This is most likely a bug as <g-f> is of type <function>
//
  check-false("", subtype?(<generic-function>, <method>));
end test method-type;

define test method-0 (description: "simple method syntax")
  check-true("", method (a :: <number>, b :: <number>)
    list(a - b, a + b)
  end method
    (3, 4)
  = #(-1, 7));
  check-true("",method (x, y)
      x * (y + 5)
    end method
      (3, 9)
    = 42);
end test method-0;

define test method-1 (description: "with keys")
  check-true("",method (x, #key y = 3)
    x * (y + 5)
  end method
    (4)
  = 32);
  check-true("",method (x, #key y = 3)
      x * (y + 5)
    end method
      (9, y: 4)
    = 81);
  check-true("",method (#key x, y)
      x * (y + 5)
    end method
      (x: 3, y: 7)
    = 36);
  check-true("",method (a, b, #key c)
      list(a, b, c)
    end method
      (#"one", #"two", c: #"three")
    = #(#"one", #"two", #"three"));
  check-true("",method (a, b, #key c)
      list(a, b, c)
    end method
      (c: #"three")
    = #(#"c", #"three", #f));
  check-true("",method (a, b, #key c, d)
      list(a, b, c, d)
    end method
      (10, 20, c: 30)
    = #(10, 20, 30, #f));
  check-true("",method (#key a, b, c)
      list(a, b, c)
    end method
      ()
    = #(#f, #f, #f));
  check-true("",method (#key a, b, c)
      list(a, b, c)
    end method
      (a: 10, b: 20, c: 30)
    = #(10, 20, 30));
end test method-1;

define test method-2 (description: "default of keyword is #f")
  check-true("",method (a, b, #key c, d)
    list(a, b, c, d)
  end method
    (10, 20)
  = #(10, 20, #f, #f));
end test method-2;

define test method-3 (description: "method should return a <method>")
  check-true("",instance?
    (method (x, y)
       x + y
     end method,
     <method>));
end test method-3;

define test method-4 (description: "with #rest")
  check-true("",method (a, #rest b)
    list(a, b)
  end method
    (10, 20, 30, 40)
  = #(10, #(20, 30, 40)));
  check-true("",method (a, #rest b)
      list(a, b)
    end method
      (10)
    = #(10, #()));
  check-true("",method (#rest a)
      a.list
    end method
      (10, 20)
    = #(#(10, 20)));
  check-true("",method (fee, fi, #rest all)
      list(all, fee, fi)
    end method
      (10, 20)
    = #(#(), 10, 20));
end test method-4;

define test method-5 (description: "#key and #rest")
  check-true("",method (#rest all, #key fee, fi)
    list(all, fee, fi)
  end method
    (fee: 10, fi: 20)
  = #(#(#"fee", 10, #"fi", 20), 10, 20));
  check-true("",method (the-req, #rest the-rest, #key a, b)
      list(the-req, the-rest, a, b)
    end method
      (1, a: 2, b: 3)
    = #(1, #(#"a", 2, #"b", 3), 2, 3));
end test method-5;

define test keyword? (description: "")

// This seems to be recognizing unique strings also as an
// instance of <keyword> - probably a bug. Also note that
// <keyword> is not mentioned in DRM
//
  check-false("", instance?(#"a", <keyword>));
  check-true("", instance?(foo: <keyword>)); 
  check-false("", instance?(3, <keyword>));
end test keyword?;

// Design note #21 introduces result type declarations

define method test-plus (x, y) => (total :: <integer>)
  x + y
end method test-plus;

define test result-type-1 (description: "test-plus")
  check-true("",25 = test-plus(22, 3));
end test result-type-1;

define test result-type-1a (description: "bind method")
  let foo
    = method (x, y) => (total :: <integer>)
        x + y
      end method;
  check-true("",25 = foo(22, 3));
end test result-type-1a;

define test result-type-1b (description: "bind-methods")
  local method foo (x, y) => (total :: <integer>)
          x + y
        end method foo;
  check-true("",25 = foo(22, 3));
end test result-type-1b;

// Declare return <integer> and try to return a non-integer
// These should signal type-errors.

// The following tests: 2, 2a, 2b, 4, 4a and 4b look buggy
// because the expression returns and instance of RATIO and
// from the previous test-suites we saw that RATIO was not
// implemented
//
define test result-type-2
  (description: "should always return integer")
  check-condition("", <error>, test-plus(22, 1 / 2));
end test result-type-2;

define test result-type-2a (description: "bind method")
  let foo
    = method (x, y) => (total :: <integer>)
        x + y
      end method;
  check-condition("", <error>, foo(22, 1 / 2));
end test result-type-2a;

define test result-type-2b (description: "bind-methods")
  local method foo (x, y) => (total :: <integer>)
          x + y
        end method foo;
  check-condition("", <error>, foo(22, 1 / 2));
end test result-type-2b;

// return multiple values in #values #rest

define method rtype-3-fcn (#rest a)
  apply(values, a)
end method rtype-3-fcn;

define test result-type-3 (description: "return multiple values")
  let (a, b, c, d) = rtype-3-fcn(1, 2, 3, 4);
  check-true("",a  = 1 & b = 2 & c = 3 & d = 4);
end test result-type-3;

define test result-type-3a (description: "bind method")
  let foo
    = method (#rest a)
        apply(values, a)
      end method;
  let (a, b, c, d) = foo(1, 2, 3, 4);
  check-true("",a = 1 & b = 2 & c = 3 & d = 4);
end test result-type-3a;

define test result-type-3b (description: "bind-methods")
  local method foo (#rest a)
          apply(values, a)
        end method foo;
  let (a, b, c, d) = foo(1, 2, 3, 4);
  check-true("",a = 1 & b = 2 & c = 3 & d = 4);
end test result-type-3b;

// signal error on #values #rest when returning the wrong type

// The problem here is with values because it perfectly well
// accepts a string argument and returns it
//
define method rtype-4-fcn (#rest a)
  apply(values, a)
end method rtype-4-fcn;

define test result-type-4
  (description: "have to return just integers")
  check-condition("", <error>, rtype-4-fcn(1, 2, 3, "string"));
end test result-type-4;

define test result-type-4a (description: "bind method")
  let foo
    = method (#rest a)
        apply(values, a)
      end method;
  check-condition("", <type-error>, foo(1, 2, 3, "string"));
end test result-type-4a;

define test result-type-4b (description: "bind-methods")
  local method foo (#rest a)
          apply(values, a)
        end method foo;
  check-condition("", <error>, foo(1, 2, 3, "string"));
end test result-type-4b;

// return fewer values than specified

define method rtype-5-fcn () => (x, y, z)
  values(1, 2)
end method rtype-5-fcn;

// Am not sure about this, rtype-5-fcn seems to
// be returning only 1 and 2 and no #f or maybe
// that is a NULL and so it does not match against
// #f ??

define test result-type-5 (description: "")
  check-true("", rtype-5-fcn() = #(1, 2, #f));
end test result-type-5;

define test result-type-5a (description: "bind method")
  let foo
    = method () => (x, y, z)
        values(1, 2)
      end method;
  let (#rest result) = foo();
  check-true("",result = #(1, 2, #f));
end test result-type-5a;

define test result-type-5b (description: "bind-methods")
  local method foo () => (x, y, z)
          values(1, 2)
        end method foo;
  let (#rest result) = foo();
  check-true("",result = #(1, 2, #f));
end test result-type-5b;

// check types when returning fewer values than specified

// Well I guess this should have raised an error then?
//
define method rtype-6-fcn (foo :: <integer>)
    => (x :: <integer>, y :: <integer>, z :: <integer>)
  values(1, 2)
end method rtype-6-fcn;

define test result-type-6 (description: "")
  check-condition("", <type-error>, rtype-6-fcn(1));
end test result-type-6;

define test result-type-6a (description: "bind method")
  let foo
    = method () => (x :: <integer>, y :: <integer>, z :: <integer>)
        values(1, 2)
      end method;
  check-condition("", <error>, foo(1));
end test result-type-6a;

define test result-type-6b (description: "bind-methods")
  local method foo () => (x :: <integer>, y :: <integer>, z :: <integer>)
          values(1, 2)
        end method foo;
  check-condition("", <error>, foo(1));
end test result-type-6b;

// Test generic-functions

// Generic function without #value #rest declaration

define generic gfun-test-1 (a, b) => (c :: <integer>, d :: <integer>);

define test generic-fcn-values-1a
  (description: "can't add a method with #rest return vals if the gf doesn't have them")
  check-condition("", <error>,
		  add-method(gfun-test-1,
			     method
				 (a, b)
			      => (c :: <integer>, d :: <integer>, #rest e)
			       values(a + b, 2, 3, 4)
			     end method));
end test generic-fcn-values-1a;

define test generic-fcn-values-1b
  (description: "can't add a method with different number of required return vals")
  check-condition("", <error>,
		  add-method(gfun-test-1,
			     method (a, b) => (c :: <integer>)
			       a + b
			     end method));
end test generic-fcn-values-1b;

define test generic-fcn-values-1c
  (description: "can't add a method with different number of required return vals")
  check-condition("", <error>,
		  add-method(gfun-test-1,
			     method
				 (a, b)
			      => (c :: <integer>, d :: <integer>, 
				  e :: <integer>)
			       values(a + b, 2, 3)
			     end method));
end test generic-fcn-values-1c;

define test generic-fcn-values-1d
  (description: "can't add a method returning types which are not subtypes")
  check-condition("", <error>,
		  add-method(gfun-test-1,
			     method (a, b) => (c, d)
			       values(a, b)
			     end method));
end test generic-fcn-values-1d;

define test generic-fcn-values-1e
  (description: "can't add a method returning types which are not subtypes")
  check-condition("", <error>, 
		  add-method(gfun-test-1,
			     method (a, b) => (c :: <integer>, d :: <string>)
			       values(a, b)
			     end method));
end test generic-fcn-values-1e;

// Generic function with #value #rest declaration

define generic gfun-test-2 (a, b) => (c :: <integer>, #rest d);

// This should theoretically work because the method creates
// and instance of <method> and gfun-test-2 is defined as
// generic..dunno what is up with this?
//
define test generic-fcn-values-2a
  (description: "permitted, but not required, to add methods which have #rest")
    add-method (gfun-test-2,
       method (a == #"test1", b) => (c :: <integer>, d :: <integer>, #rest e)
         values(1, b, 3, 4, 5)
       end method);
    add-method (gfun-test-2,
       method (a == #"test2", b) => (c :: <integer>, d :: <integer>)
         values(2, b, 3, 4, 5)
       end method);
    let (c1, d1, e1) = gfun-test-2(#"test1", 7);
    let (c2, d2, e2) = gfun-test-2(#"test2", 7);
    check-true("", list(c1, d1, e1) = #(1, 7, 3)); 
    check-true("", list(c2, d2, e2) = #(2, 7, #f));
end test generic-fcn-values-2a;

define suite test-intro-mop-suite () 
  test slot-operations;
  test method-type;
  test method-0;
  test method-1;
  test method-2;
  test method-3;
  test method-4;
  test method-5;
  test keyword?;
  test result-type-1;
  test result-type-1a;
  test result-type-1b;
  test result-type-2;
  test result-type-2a;
  test result-type-2b;
  test result-type-3;
  test result-type-3a;
  test result-type-3b;
  test result-type-4;
  test result-type-4a;
  test result-type-4b;
  test result-type-5;
  test result-type-5a;
  test result-type-5b;
  test result-type-6;
  test result-type-6a;
  test result-type-6b;
  test generic-fcn-values-1a;
  test generic-fcn-values-1b;
  test generic-fcn-values-1c;
  test generic-fcn-values-1d;
  test generic-fcn-values-1e;
  test generic-fcn-values-2a;
end suite;
