Module:       dylan-test-suite
Synopsis:     Dylan test suite
Author:       Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Some control structures are tested in macros.dylan.

define test test-truth ()
  check-true("Boolean logic correct", #t = #t & ~(#t = #f) & #"a" & 0 & #() & #[] & #t);
end test;

define test test-not ()
  check-true("not on simple objects",
	     ~(~100) & ~100 = #f & ~(~#()) & ~#f & ~#t = #f);
end test;

define test test-conditionals ()
  check-equal("cond stop when test is five = five",
	     if (2 < 2)
	       "2 less than 2"
	     elseif (3 > 3)
	       "3 greater than 3"
	     elseif (5 = 5)
	       "5 equals 5"
	     else
	       "none of above"
	     end if,
	     "5 equals 5");
  check-equal("cond else clause is catch all",
	      if (2 < 2)
		"2 less than 2"
	      elseif (3 > 3)
		"3 greater than 3"
	      elseif (5 = 1)
		"5 equals 1"
	      else
		"none of above"
	      end if,
	      "none of above");
  check-equal("cond remaining tests not evaluated",
	      begin
		let x = 0;
		if (100 = 100)
		  #t
		elseif ((x := 100) = #())
		  #f
		end if;
		x
	      end,
	      0);
  check-false("cond returns false if no test evals to true",
	      if (2 = 3)
		#f
	      elseif (#t = #f)
		#f
	      end);
  check-equal("cond no consequents, returns 1st value of test case",
	      begin
		let (a, b, c)
		  = if (3 = 2)
		      #"foo"
		    elseif (values(3, 4, 5))
		      #t;
		      values(9, 8, 7)
		    else
		      #t
		    end if;
		list(a, b, c)
	      end,
	      #(9, 8, 7));
end test;

define function no-param-function () 1 end;
define function one-param-function (x) x end;

define test test-required-calls (expected-failure?: #t)
  check-equal("no param call",
              (method () 1 end)(), 1);
  check-equal("one param call one arg",
              (method (x) x end)(1), 1);
  check-condition("no param call one arg", <error>,
                  apply(no-param-function, #[1]));
  check-condition("one param call no args", <error>,
                  apply(one-param-function, #[]));
  check-condition("one param call two args", <error>,
                  apply(one-param-function, #[1, 2]));
  check-equal("two args call",
              (method (x, y) x + y end)(1, 2), 3);
  check-equal("lots args call",
              (method (x1, x2, x3, x4, x5, x6, x7, x8, x9)
                 x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
               end)(1, 2, 3, 4, 5, 6, 7, 8, 9),
	      45);
end test;

define test test-rest-calls ()
  check-equal("no args rest call",
              (method (#rest x) x end)(), #[]);
  check-equal("one arg rest call",
              (method (#rest x) x end)(1), #[1]);
  check-equal("two arg rest call",
              (method (#rest x) x end)(1, 2), #[1, 2]);
  check-equal("one req rest call",
              (method (x, #rest y) y end)(1, 2), #[2]);
end test;

define test test-keyword-calls (expected-failure?: #t)
  check-equal("one key no args call",
	      (method (#key x) x end)(), #f);
  check-equal("one key defaulted no args call",
	      (method (#key x = 0) x end)(), 0);
  check-equal("one key call supplied",
	      (method (#key x) x end)(x: 1), 1);
  check-condition("one key call wrong supplied", <error>,
                  begin
                    local method m (#key x) x end;
                    // Stifle compiler warning we get if called directly.
                    let methods = list(m);
                    methods[0](y: 1)
                  end);
  check-false("one key call wrong supplied but all-keys",
	      (method (#key x, #all-keys) x end)(y: 1));
  check-equal("two key call first supplied",
	      (method (#key x, y) x end)(x: 1), 1);
  check-equal("two key call second supplied",
	      (method (#key x, y) y end)(y: 1), 1);
  check-equal("two key call both supplied",
	      (method (#key x, y) x + y end)(x: 1, y: 2), 3);
end test;

define test test-rest-keyword-calls (expected-failure?: #t)
  check-equal("rest no key call no args",
	      (method (#rest keys, #key) keys end)(), #[]);
  check-equal("rest one key call no args",
	      (method (#rest keys, #key x) keys end)(), #[]);
  check-condition("rest one key call one arg", <error>,
                  begin
                    local method m (#rest k, #key) k end;
                    // Stifle compiler warning we get if called directly.
                    let methods = list(m);
                    methods[0](y: 1)
                  end);
  check-equal("rest one key call one arg keys",
	      (method (#rest keys, #key x) keys end)(x: 1), #[#"x", 1]);
  check-equal("rest one key call one arg key",
	      (method (#rest keys, #key x) x end)(x: 1), 1);
end test;

define test test-apply ()
  check-equal("apply sov size 1",
	      apply(method (x) x end, #[1]), 1);
  check-equal("apply arg + sov size 0",
	      apply(method (x) x end, 1, #[]), 1);
  check-equal("apply arg + sov size 1 arg 1",
	      apply(method (x, y) x end, 1, #[2]), 1);
  check-equal("apply arg + sov size 1 arg 2",
	      apply(method (x, y) y end, 1, #[2]), 2);
  check-equal("apply sov size 2 arg 1",
	      apply(method (x, y) x end, #[1, 2]), 1);
  check-equal("apply sov size 2 arg 2",
	      apply(method (x, y) y end, #[1, 2]), 2);
  check-equal("apply sov size 1 rest",
	      apply(method (#rest x) x end, #[1]), #[1]);
  check-equal("apply arg + sov size 1 rest",
	      apply(method (#rest x) x end, 1, #[2]), #[1, 2]);
  check-equal("apply list size 1 rest",
	      apply(method (#rest x) x end, #(1)), #[1]);
  check-equal("apply arg + list size 1 rest",
	      apply(method (#rest x) x end, 1, #(2)), #[1, 2]);
end test;

define test test-labels ()
  check-equal("two mutally recursive locals",
	      begin
		local
		  method f () g() end method,
		  method g () 1 end method;
		f()
	      end, 1);
  check-equal("two mutally recursive locals with args",
	      begin
		local
		  method f (x) g(x) end method,
		  method g (y) y end method;
		f(1)
	      end, 1);
  check-equal("recursive factorial",
	      begin
		local method f (n) if (n < 1) 1 else n * f(n - 1) end end;
		f(4)
	      end,
	      24);
  check-equal("iterative factorial",
	      begin
		local method f (n, r)
			if (n < 1) r else f(n - 1, n * r) end
		      end method;
		f(4, 1)
	      end,
	      24);
  check-equal("bounce back",
	      begin
		local
		  method f (x) if (x < 10) g(x + 2) else x end end method,
		  method g (y) f(y - 1) end method;
		f(1)
	      end,
	      10);
  check-true("funky",
	     begin
	       local method f (n, x, y)
		       if (n > 0) f(n - 1, y, x) else x end
		     end method;
	       f(0, 1, 2) = 1 & f(1, 1, 2) = 2 & f(2, 1, 2) = 1
	     end);
  check-equal("continuation passing",
	      begin
		let f = method (self, x, y)
			  if (x = 0)
			    y
			  else
			    self(self, x - 1, y + 1)
			  end if
			end method;
		f(f, 4, 0)
	      end,
	      4);
end test;

define test test-closures ()
  check-equal("exec closure inside",
	      (method (x) (method () x end)() end)(1), 1);
  check-equal("exec returned closure",
	      ((method (x) (method () x end) end)(1))(), 1);
  check-equal("exec side-effecting closure inside",
	      (method (x) (method () x := x + 1 end)() end)(0), 1);
  check-equal("exec returned side-effecting closure",
	      ((method (x) (method () x := x + 1 end) end)(0))(), 1);
end test;

define test test-bind-exits ()
  check-equal("simple bind-exit",
	      block (return) return(1) end, 1);
  check-equal("simple premature returning bind-exit",
	      block (return) return(1); 2 end, 1);
  check-equal("simple premature returning bind-exit in call",
 	      block (return) return(1) + 2 end, 1);
  check-equal("bind-exit passing return",
	      block (return) (method (r) r(1) end)(return) end, 1);
end test;

define test test-unwind-protects ()
  check-equal("unwind-protect returns protected",
	      block () 1 cleanup 2 end, 1);
  check-equal("unwind-protect returns protected even with throw",
	      block (return) return(1) cleanup 2 end, 1);
  check-equal("unwind-protect cleanup takes precedence",
	      block (return)
		return(1)
	      cleanup
		block ()
		  return(1)
		cleanup
		  return(3)
		end
	      end,
	      3);
  check-equal("unwind-protect cleanup happens",
	      begin
		let a = 1;
		block (return)
		  return(1)
		cleanup
		  a := 2
		end;
		a
	      end,
	      2);
  check-equal("unwind-protect last cleanup wins",
	      block (a)
		block (b)
		  block (c)
		    c(0)
		  cleanup
		    a(13)
		  end
		cleanup
		  b(42)
		end
	      end,
	      42);
end test;

define test test-afterwards ()
  check-equal("afterwards happens",
	      begin
		let a = 0;
		block ()
		  1
	        afterwards
		  a := 1
		end block;
		a
	      end,
	      1);
  check-equal("afterwards returns protected",
	      block ()
		1
	      afterwards
		2
	      end block,
	      1);
end test;

define test test-multiple-values ()
  check-true("multiple-value binding",
	     begin
	       let (x, y, z) = values(1, 2, 3);
	       x = 1 & y = 2 & z = 3
	     end);
  check-equal("multiple-value rest binding",
	      begin
		let (#rest r) = values(1, 2, 3);
		r
	      end,
	      #[1, 2, 3]);
  check-true("multiple-value rest binding with req",
	     begin
	       let (x, y, z, #rest r) = values(1, 2, 3, 4, 5);
	       x = 1 & y = 2 & z = 3 & r = #[4, 5]
	     end);
  check-true("multiple-value binding excess false",
	     begin
	       let (x, y, z, none) = values(1, 2, 3);
	       x = 1 & y = 2 & z = 3 & none = #f
	     end);
  check-equal("unwind-protect saves values",
	      begin
		let (#rest x)
		  = block ()
		      values(1, 2)
		    cleanup
		      3
		    end block;
                x
	      end,
	      #[1, 2]);
  check-equal("afterwards saves values",
	      begin
		let (#rest x)
		  = block ()
		      values(1, 2)
		    afterwards
		      3
		    end block;
                x
	      end,
	      #[1, 2]);
end test;

define test test-exceptions ()
  //---*** fill this in...
end test;

define suite dylan-control-test-suite ()
  test test-truth;
  test test-not;
  test test-conditionals;
  test test-required-calls;
  test test-rest-calls;
  test test-keyword-calls;
  test test-rest-keyword-calls;
  test test-apply;
  test test-labels;
  test test-closures;
  test test-bind-exits;
  test test-unwind-protects;
  test test-afterwards;
  test test-multiple-values;
  test test-exceptions;
end suite;
