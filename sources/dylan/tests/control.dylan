Module:       dylan-test-suite
Synopsis:     Dylan test suite
Author:       Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test truths ()
  check-true("Boolean logic correct", #t = #t & ~(#t = #f) & #"a" & 0 & #() & #t);
end test truths;

define test nots ()
  check-true("not on simple objects", 
	     ~(~100) & ~100 = #f & ~(~#()) & ~#f & ~#t = #f);
end test nots;

define test conds ()
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

define test cases ()
  check-equal("CASE STOP WHEN TEST IS FIVE = FIVE", 
	      case
		(2 < 2)   => "2 less than 2";
		(3 > 3)   => "3 greater than 3";
		(5 = 5)   => "5 equals 5";
		otherwise => "none of above";
	      end case,
	     "5 equals 5");
  check-equal("CASE ELSE CLAUSE IS CATCH ALL", 
	      case
		(2 < 2)   => "2 less than 2";
		(3 > 3)   => "3 greater than 3";
		(5 = 1)   => "5 equals 1";
		otherwise => "none of above";
	      end case,
	      "none of above");
  check-equal("CASE REMAINING TESTS NOT EVALUATED", 
	      begin
		let x = 0;
		case 
		  (100 = 100)        => #t;
		  ((x := 100) = #()) => #f;
		end case;
		x
	      end,
	      0);
  check-false("CASE RETURNS FALSE IF NO TEST EVALS TO TRUE", 
	      case 
		(2 = 3)   => 55;
		(#t = #f) => 56;
	      end case);
  check-equal("Returns all values from last consequent", 
	      begin
		let (a, b, c)
		  = case
		      (3 = 2)         => #"foo";
		      values(3, 4, 5) => #t; values(9, 8, 7);
		      otherwise       => #t;
		    end case;
		list(a, b, c)
	      end,
	      #(9, 8, 7));
end test;

define function student (career)
  select (career)
    #"art", #"music", #"drama" =>
      "Don't quit your day job.";
    #"literature", #"history", #"linquistics" =>
      "That really is fascinating";
    #"science", #"math", #"engineering" =>
      "Say, can you fix my VCR?";
    otherwise =>
      "I wish you luck."
  end select
end function;

define test selects ()
  check-equal("SELECT SIMPLE", 
	      select (3 + 2)
	        1 => ;
	        2 => ;
	        4, 5, 6 => "5ish";
	        otherwise => "none of above"
	      end select,
	      "5ish");
  check-equal("SELECT OTHERWISE", 
	      select (30 + 2)
		1 => ;
		2 => ;
		5 => ;
		otherwise => "none of above"
	      end select,
	      "none of above");
  check-equal("SELECT STUDENT ART", 
	      student(#"art"), "Don't quit your day job.");
  check-equal("SELECT STUDENT ENGINEERING", 
	      student(#"engineering"), "Say, can you fix my VCR?");
  check-equal("SELECT STUDENT NURSING", 
	      student(#"nursing"), "I wish you luck.");
  check-false("SELECT IF NO CONSEQUENTS FALSE", 
	      select (1 + 1)
		1 => ;
		2 => ;
		5 => ;
		otherwise => "none of above"
	      end select);
  check-false("SELECT IF NO OTHERWISE CONSEQUENTS FALSE", 
	      select (1 + 99)
	        1 => ;
		2 => ;
                5 => ;
		otherwise =>
	      end select);
  check-equal("SELECT RETURNS MULTIPLE VALUES", 
	      begin
		let (a, b, c)
		  = select (3 + 2)
		      1, 2, 3 =>
			values(1, 2, 3);
		      4, 5, 6 =>
			values(4, 5, 6);
		      otherwise =>
			#f
		    end select;
		list(a, b, c)
	      end,
	      #(4, 5, 6));
  check-equal("SELECT RETURNS LAST CONSEQUENT", 
	      select (999)
		1, 2, 999 =>
		  #"oops";
		  #"oops";
		  #"oops";
		  #"ok"
	      end select,
	      #"ok");
  check-equal("SELECT OTHERWISE RETURNS LAST CONSEQUENT", 
	      select (999)
		otherwise =>
		  #"oops";
		  #"oops";
		  #"oops";
		  #"ok"
	      end select,
	      #"ok");
  check-equal("SELECT BY <", 
	      select (10 + 5 by \<)
		12, 12 + 1 =>
		  "12 or 13";
		15 - 1, 15 =>
		  "14 or 15";
		5 * 3, 16 =>
		  "15 or 16";
		99, 100 =>
 		  "99 or 100"
	      end select,
	      "15 or 16");
  check-equal("SELECT BY < OTHERWISE", 
	      select (100 + 5 by \<)
		12, 12 + 1 =>
		  "12 or 13";
		15 - 1, 15 =>
		  "14 or 15";
		5 * 3, 16 =>
		  "15 or 16";
		99, 100 =>
		  "99 or 100";
		otherwise =>
		  "else";
	      end select,
	      "else");
  check-equal("SELECT BY INSTANCE?", 
	      select (5 by instance?)
		<list> =>
		  "list";
		<number>, <sequence> =>
		  "computational";
		otherwise =>
		  "?";
	      end select,
	      "computational");
end test selects;

define test ors ()
  check-equal("OR SIMPLE NUMBERS", 
	      (1 | 2 | 3), 1); 
  check-equal("OR WITH SIMPLE PREDICATES", 
	      (even?(3) | zero?(2) | 0), 0);
  check-false("OR NONE TRUE THEN FALSE", 
	      (even?(3) | odd?(2) | zero?(-1)));
  check-equal("OR NOTHING EVALED AFTER ONE RETURNS TRUE", 
	      begin
		let x = 0;
		#f | 1 | (x := 999);
		x
	      end,
	      0);
  check-equal("OR MULTIPLE VALUES BEFORE LAST FIRST VAL RETURNED", 
	      begin
		let (a, b, c) = values(1, 2, 3) | #f;
		list(a, b, c);
	      end,
	      #(1, #f, #f));
  check-equal("OR MULTIPLE VALUES IN LAST, ALL VALS RETURNED", 
	      begin
		let (a, b, c) = #f | values(1, 2, 3);
		list(a, b, c)
	      end,
	      #(1, 2, 3));
  check-equal("OR 1ST VALUE ONLY THING THAT MATTERS TO JUDGE TRUTH", 
	      (even?(1) | values(#f, #t) | 3), 3);
end test ors;

define test ands (description: "Simple cases")
  check-equal("AND SIMPLE NUMBER", (1 & 2 & 3), 3); 
  check-false("AND WITH SIMPLE PREDICATE", 
	      (1 & 2 & even?(3)));
  check-equal("AND MULTIPLE VALUES AS LAST FORM RETURNS ALL VALS", 
	      begin
		let (a, b, c) = 1 & 2 & values(1, #f, 3);
		list(a, b, c)
	      end,
	      #(1, #f, 3));
  check-equal("AND 1ST VALUE ONLY THING THAT MATTERS TO JUDGE TRUTH", 
	      begin
		let (a, b, c) = values(1, #f, 3) & 1 & 2;
		list(a, b, c);
	      end,
	      #(2, #f, #f));
  check-equal("AND THINGS PAST 1ST FALSE DON'T GET EVALED", 
	      begin
		let x = 0;
		values(1, #f) & values(#f, 1) & (x := 999);
		x
	      end,
	      0);
end test ands;

define test fors ()
  check-equal("FOR SIMPLE ITERATION", 
	     begin
	       let v = 0;
	       for (i from 10 to 0 by -1)
		 v := v + i;
	       end for;
	       v
	     end,
	     55);
  check-equal("FOR PARAMETERIZE INT ITERATION", 
	      method (n :: <integer>)
		for (i from n to 1 by -1, v = 1 then v * i)
		finally
		  v;
		end for
	      end method(5),
	      120);
  check-equal("FOR = THEN WITH LISTS", 
	      begin
		let m = #();
		for (l = #(0, 1, 2, 3, 4, 5, 6, 7, 8, 9) then l.tail, until: l.empty?)
		  m := pair(l.head, m);
		end for;
		m
	      end,
	      #(9, 8, 7, 6, 5, 4, 3, 2, 1, 0));
  check-equal("FOR MULTIPLE VARS", 
	      begin
		let i = 0;
		let j = 0;
		let k = 0;
		for (i from 1 to 10, j = i + 1 then i + 1, k = j + 1 then j + 1)
		finally
		  i + (j + k);
		end for
	      end,
	      33);
  check-equal("FOR SUMMING OVER LET VAR", 
	      begin
		let sum = 0;
		for (i from 0 to 10)
		  sum := sum + i;
		end for;
		sum
	      end,
	      55);
  check-equal("FOR SUMMING OVER LET VAR WITH LOTS OF ITERATION VARS", 
	      begin
		let sum = 0;
		for (i from 0,
		     j from 10 by -1,
		     k from 0,
		     l from 10 by -1,
		     until: i = j & j = k & k = l)
		  sum := sum + i + j + k + l;
		end for;
		sum
	      end,
	      100);
  check-equal("FOR DOESN'T DO IT THE FIRST TIME IF END-TEST INITIALLY TRUE", 
	      begin
		let v = 0;
		for (i from 0, j from 10 by -1, until: i < j)
		  v := #f;
		end for;
		v
	      end,
	      0);
  check-equal("FOR MULTIPLE RETURN FORMS, DO ALL, RETURN LAST", 
	      begin
		let v = 0;
		let w = 0;
		list(for (i from 0, j from 10 by -1, until: i > j)
		       v := v + (i + j);
		     finally
		       v;
		       w := 100;
		       #"last";
		     end for,
		     w)
	      end,
	      #(#"last", 100));
  check-equal("multiple values return", 
	      begin
		let v = 0;
		let w = 0;
		let (a, b)
		  = for (i from 0, j from 10 by -1, until: i > j)
		      v := v + (i + j);
		    finally
		      v;
		      w := 100;
		      values(v, w);
		    end for;
		list(a, b)
	      end,
	      #(60, 100));
  check-equal("FOR UNTIL TRUE WITH FINALLY", 
	      for (until: #t)
	      finally
		#"ok";
	      end for,
	      #"ok");
  check-equal("FOR UNTIL TRUE WITH LOTS O FINALLY", 
	      for (until: #t)
	      finally
		#"oops";
		#"oops";
		#"oops";
		#"ok";
	      end for,
	      #"ok");
  check-equal("FOR MULTIPLE RETURNS", 
	       begin
	         let x = 0;
                 list(block (return)
		        for (number in #(5, 3, 2, 4, 7))
		  	  if (number.even?)
			    x := 100;
			    return(2 * number)
			  end if;
		        end for
		      end block,
		      x)
               end,
	       #(4, 100));
  check-equal("FOR MULTIPLE VALUES RETURN", 
	      begin
		let (a, b)
		  = block (return)
		      for (number in #(5, 3, 2, 4, 7))
			if (number.even?)
			  return(2 * number, 2 + number)
			end if;
		      end for
		    end block;
		list(a, b)
	      end,
	      #(4, 4));
  check-false("FOR NEVER TRUE RETURN FALSE", 
	      block (return)
		for (number in #(5, 3, 9, 9, 7))
		  if (number.even?)
		    return(number)
		  end if;
		end for
	      end block);
  check-false("FOR ONE RUNS OUT FIRST RETURN FALSE", 
	      block (return)
		for (number1 in #(5, 3, 9, 2), number2 in #(1, 2, 3))
		  if (number1.even?)
		    return(number1)
		  end if;
		end for
	      end block);
end test fors;

define test begins ()
  check-false("BEGIN: NO BODY IS FALSE",
	      begin end);
  check-equal("BEGIN: RETURN LAST EXPRESSION",
	      begin 1; 2; 3 end, 3);
  check-equal("BEGIN NOT SKIPPING TO LAST FORM I HOPE?",
	      begin
		let x = 0;
		begin
		  x := 1 + x;
		  x := 1 + x;
		  x := 1 + x
		end;
		x
	      end,
	      3);
  check-equal("BEGIN IF LAST FORM IS MULTIPLE VALUES, RETURN THEM ALL",
	      begin
		let (a, b)
		  = begin
		      #"step1";
		      #"step2";
		      values(#"step3", #"step4")
		    end;
		list(a, b)
	      end,
	      #(#"step3", #"step4"));
end test begins;

define function no-param-function () 1 end;
define function one-param-function (x) x end;

define test required-calls ()
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
end test required-calls;

define test rest-calls ()
  check-equal("no args rest call",
	     (method (#rest x) x end)(), #[]);
  check-equal("one arg rest call",
	     (method (#rest x) x end)(1), #[1]);
  check-equal("two arg rest call",
	     (method (#rest x) x end)(1, 2), #[1, 2]);
  check-equal("one req rest call",
	     (method (x, #rest y) y end)(1, 2), #[2]);
end test rest-calls;

define test keyword-calls ()
  check-equal("one key no args call", 
	      (method (#key x) x end)(), #f);
  check-equal("one key defaulted no args call",
	      (method (#key x = 0) x end)(), 0);
  check-equal("one key call supplied",
	      (method (#key x) x end)(x: 1), 1);
  check-condition("one key call wrong supplied", <error>,
                  (method (#key x) x end)(y: 1));
  check-false("one key call wrong supplied but all-keys",
	      (method (#key x, #all-keys) x end)(y: 1));
  check-equal("two key call first supplied",
	      (method (#key x, y) x end)(x: 1), 1);
  check-equal("two key call second supplied",
	      (method (#key x, y) y end)(y: 1), 1);
  check-equal("two key call both supplied",
	      (method (#key x, y) x + y end)(x: 1, y: 2), 3);
end test keyword-calls;

define test rest-keyword-calls ()
  check-equal("rest no key call no args", 
	      (method (#rest keys, #key) keys end)(), #[]);
  check-equal("rest one key call no args", 
	      (method (#rest keys, #key x) keys end)(), #[]);
  check-condition("rest one key call one arg", <error>,
                  (method (#rest keys, #key) keys end)(x: 1));
  check-equal("rest one key call one arg keys", 
	      (method (#rest keys, #key x) keys end)(x: 1), #[#"x", 1]);
  check-equal("rest one key call one arg key", 
	      (method (#rest keys, #key x) x end)(x: 1), 1);
end test rest-keyword-calls;

define test applys ()
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
end test applys;

define test labelsies ()
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
end test labelsies;

define test closures ()
  check-equal("exec closure inside", 
	      (method (x) (method () x end)() end)(1), 1);
  check-equal("exec returned closure", 
	      ((method (x) (method () x end) end)(1))(), 1);
  check-equal("exec side-effecting closure inside", 
	      (method (x) (method () x := x + 1 end)() end)(0), 1);
  check-equal("exec returned side-effecting closure", 
	      ((method (x) (method () x := x + 1 end) end)(0))(), 1);
end test closures;

define test bind-exits ()
  check-equal("simple bind-exit",
	      block (return) return(1) end, 1);
  check-equal("simple premature returning bind-exit",
	      block (return) return(1); 2 end, 1);
  check-equal("simple premature returning bind-exit in call",
 	      block (return) return(1) + 2 end, 1);
  check-equal("bind-exit passing return",
	      block (return) (method (r) r(1) end)(return) end, 1);
end test bind-exits;

define test unwind-protects ()
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
end test unwind-protects;

define test afterwardsies ()
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
end test afterwardsies;

define test multiple-valuesies ()
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
end test multiple-valuesies;

define test exceptions ()
  //---*** fill this in...
end test exceptions;

define suite dylan-control-suite ()
  test truths;
  test nots;
  test conds;
  test required-calls;
  test rest-calls;
  test keyword-calls;
  test rest-keyword-calls;
  test applys;
  test labelsies;
  test closures;
  test bind-exits;
  test unwind-protects;
  test afterwardsies;
  test multiple-valuesies;
  test exceptions;
end suite dylan-control-suite;
