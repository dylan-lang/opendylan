Module:       dylan-test-suite
Synopsis:     Dylan test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Macro testing

define dylan macro-test begin-test ()
  check-false("begin: no body is false",
	      begin end);
  check-equal("begin: return last expression",
	      begin 1; 2; 3 end, 3);
  check-equal("begin not skipping to last form i hope?",
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
  check-equal("begin if last form is multiple values, return them all",
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
end macro-test begin-test;

define dylan macro-test block-test ()
  //---*** Fill this in...
end macro-test block-test;

define dylan macro-test case-test ()
  check-equal("case stop when test is five = five", 
	      case
		(2 < 2)   => "2 less than 2";
		(3 > 3)   => "3 greater than 3";
		(5 = 5)   => "5 equals 5";
		otherwise => "none of above";
	      end case,
	     "5 equals 5");
  check-equal("case else clause is catch all", 
	      case
		(2 < 2)   => "2 less than 2";
		(3 > 3)   => "3 greater than 3";
		(5 = 1)   => "5 equals 1";
		otherwise => "none of above";
	      end case,
	      "none of above");
  check-equal("case remaining tests not evaluated", 
	      begin
		let x = 0;
		case 
		  (100 = 100)        => #t;
		  ((x := 100) = #()) => #f;
		end case;
		x
	      end,
	      0);
  check-false("case returns false if no test evals to true", 
	      case 
		(2 = 3)   => 55;
		(#t = #f) => 56;
	      end case);
  check-equal("case no consequents, returns 1st value of test case", 
	      begin
		let (a, b, c)
		  = case 
		      (3 = 2)         => #"foo";
		      values(3, 4, 5) => ;
		      otherwise       => #t;
		    end case;
		list(a, b, c)
	      end,
	      #(3, #f, #f));
  check-equal("case returns all values from last consequent", 
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
end macro-test case-test;

define dylan macro-test for-test ()
  check-equal("for simple iteration", 
	     begin
	       let v = 0;
	       for (i from 10 to 0 by -1)
		 v := v + i;
	       end for;
	       v
	     end,
	     55);
  check-equal("for parameterize int iteration", 
	      method (n :: <integer>)
		for (i from n to 1 by -1, v = 1 then v * i)
		finally
		  v;
		end for
	      end method(5),
	      120);
  check-equal("for = then with lists", 
	      begin
		let m = #();
		for (l = #(0, 1, 2, 3, 4, 5, 6, 7, 8, 9) then l.tail, until: l.empty?)
		  m := pair(l.head, m);
		end for;
		m
	      end,
	      #(9, 8, 7, 6, 5, 4, 3, 2, 1, 0));
  check-equal("for multiple vars", 
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
  check-equal("for summing over let var", 
	      begin
		let sum = 0;
		for (i from 0 to 10)
		  sum := sum + i;
		end for;
		sum
	      end,
	      55);
  check-equal("for summing over let var with lots of iteration vars", 
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
  check-equal("for doesn't do it the first time if end-test initially true", 
	      begin
		let v = 0;
		for (i from 0, j from 10 by -1, until: i < j)
		  v := #f;
		end for;
		v
	      end,
	      0);
  check-equal("for multiple return forms, do all, return last", 
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
  check-equal("for until true with finally", 
	      for (until: #t)
	      finally
		#"ok";
	      end for,
	      #"ok");
  check-equal("for until true with lots o finally", 
	      for (until: #t)
	      finally
		#"oops";
		#"oops";
		#"oops";
		#"ok";
	      end for,
	      #"ok");
  check-equal("for multiple returns", 
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
  check-equal("for multiple values return", 
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
  check-false("for never true return false", 
	      block (return)
		for (number in #(5, 3, 9, 9, 7))
		  if (number.even?)
		    return(number)
		  end if;
		end for
	      end block);
  check-false("for one runs out first return false", 
	      block (return)
		for (number1 in #(5, 3, 9, 2), number2 in #(1, 2, 3))
                  ignore(number2);
		  if (number1.even?)
		    return(number1)
		  end if;
		end for
	      end block);
end macro-test for-test;

define dylan macro-test if-test ()
  check-equal("if true", 
              if (#t) #"true" else #"false" end if, 
              #"true");
  check-equal("if false", 
              if (#f) #"true" else #"false" end if,
              #"false");
  check-equal("if empty-list", 
              if (#()) #"true" else #"false" end if, 
              #"true");
  check-equal("if symbol", 
              if (#"anything") #"true" else #"false" end if,
              #"true");
  check-equal("if vector", 
              if (#[1, 2, 3]) #"true" else #"false" end if,
              #"true");
  check-equal("if even 100", 
              if (even?(100)) #"even" else #"odd" end if,
              #"even");
  check-equal("if odd 100", 
              if (odd?(100)) #"true" else #"false" end if,
              #"false");
end macro-test if-test;

define dylan macro-test method-test ()
  //---*** Fill this in...
end macro-test method-test;

define dylan macro-test select-test ()
  local method student (career)
	  select (career)
	    #"art", #"music", #"drama" =>
	      "Don't quit your day job.";
	    #"literature", #"history", #"linquistics" =>
	      "That really is fascinating";
	    #"science", #"math", #"engineering" =>
	      "Say, can you fix my VCR?";
	    otherwise =>
	      "I wish you luck."
	  end
        end;
  check-equal("select simple", 
	      select (3 + 2)
	        1 => ;
	        2 => ;
	        4, 5, 6 => "5ish";
	        otherwise => "none of above"
	      end select,
	      "5ish");
  check-equal("select otherwise", 
	      select (30 + 2)
		1 => ;
		2 => ;
		5 => ;
		otherwise => "none of above"
	      end select,
	      "none of above");
  check-equal("select student art", 
	      student(#"art"), "Don't quit your day job.");
  check-equal("select student engineering", 
	      student(#"engineering"), "Say, can you fix my VCR?");
  check-equal("select student nursing", 
	      student(#"nursing"), "I wish you luck.");
  check-false("select if no consequents false", 
	      select (1 + 1)
		1 => ;
		2 => ;
		5 => ;
		otherwise => "none of above"
	      end select);
  check-false("select if no otherwise consequents false", 
	      select (1 + 99)
	        1 => ;
		2 => ;
                5 => ;
		otherwise =>
	      end select);
  check-equal("select returns multiple values", 
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
  check-equal("select returns last consequent", 
	      select (999)
		1, 2, 999 =>
		  #"oops";
		  #"oops";
		  #"oops";
		  #"ok"
	      end select,
	      #"ok");
  check-equal("select otherwise returns last consequent", 
	      select (999)
		otherwise =>
		  #"oops";
		  #"oops";
		  #"oops";
		  #"ok"
	      end select,
	      #"ok");
  check-equal("select by <", 
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
  check-equal("select by < otherwise", 
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
  check-equal("select by instance?", 
	      select (5 by instance?)
		<list> =>
		  "list";
		<number>, <sequence> =>
		  "computational";
		otherwise =>
		  "?";
	      end select,
	      "computational");
end macro-test select-test;

define dylan macro-test unless-test ()
  check-false("unless true", 
	      unless (#t) 1; 2; 3 end unless);
  check-false("unless even 100", 
	      unless (even?(100)) 1; 2; 3 end unless);
  check-equal("unless true doesnt exec body", 
	      begin
		let x = 0;
		unless (#t) x := 100; #"oops"; #"oops" end unless;
		x
	      end,
	      0);
  check-equal("unless false does exec body", 
	      begin
		let x = 0;
		unless (#f) x := 100; #"oops"; #"oops" end unless;
		x
	      end,
	      100);
  check-equal("unless returns ok", 
	      unless (#f) #"ok" end, #"ok");
  check-equal("unless returns last", 
	      unless (#f) #"oops"; #"oops"; #"oops"; #"oops"; #"ok" end,
	      #"ok");
  check-false("unless no forms returns false", 
	      unless (#t) end);
end macro-test unless-test;

define dylan macro-test until-test ()
  //---*** Fill this in...
end macro-test until-test;

define dylan macro-test while-test ()
  //---*** Fill this in...
end macro-test while-test;


/// Function macro suite

define dylan macro-test colon-equal-test ()
  check-equal("simple assignment",
              begin
                let x = 10;
                x := 20;
                x
              end,
              20);
  check-equal("assignment returns the correct value",
              begin
                let x = 10;
                x := 20
              end,
              20);
  check-equal("vector assignment",
              begin
                let x = make(<vector>, size: 1);
                x[0] := 10;
              end,
              10);
  check-equal("array assignment",
              begin
                let x = make(<array>, dimensions: #(2, 2));
                x[0, 0] := 10;
              end,
              10)
end macro-test colon-equal-test;

define dylan macro-test or-test ()
  check-equal("or simple numbers", 
              1 | 2 | 3,
              1); 
  check-equal("or with simple predicates", 
              even?(3) | zero?(2) | 0, 
              0);
  check-false("or none true then false", 
	      even?(3) | odd?(2) | zero?(-1));
  check-equal("or nothing evaled after one returns true", 
	      begin
		let x = 0;
		#f | 1 | (x := 999);
		x
	      end,
	      0);
  check-equal("or multiple values before last first val returned", 
	      begin
		let (a, b, c) = values(1, 2, 3) | #f;
		list(a, b, c);
	      end,
	      #(1, #f, #f));
  check-equal("or multiple values in last, all vals returned", 
	      begin
		let (a, b, c) = #f | values(1, 2, 3);
		list(a, b, c)
	      end,
	      #(1, 2, 3));
  check-equal("or 1st value only thing that matters to judge truth", 
	      (even?(1) | values(#f, #t) | 3), 3);
end macro-test or-test;

define dylan macro-test and-test ()
  check-equal("and simple number",
              1 & 2 & 3,
              3); 
  check-false("and with simple predicate", 
	      1 & 2 & even?(3));
  check-equal("and multiple values as last form returns all vals", 
	      begin
		let (a, b, c) = 1 & 2 & values(1, #f, 3);
		list(a, b, c)
	      end,
	      #(1, #f, 3));
  check-equal("and 1st value only thing that matters to judge truth", 
	      begin
		let (a, b, c) = values(1, #f, 3) & 1 & 2;
		list(a, b, c);
	      end,
	      #(2, #f, #f));
  check-equal("and things past 1st false don't get evaled", 
	      begin
		let x = 0;
		values(1, #f) & values(#f, 1) & (x := 999);
		x
	      end,
	      0);
end macro-test and-test;


/// Definer tests

define dylan macro-test class-definer-test ()
  //---*** Fill this in...
end macro-test class-definer-test;

define dylan macro-test constant-definer-test ()
  //---*** Fill this in...
end macro-test constant-definer-test;

define dylan macro-test domain-definer-test ()
  //---*** Fill this in...
end macro-test domain-definer-test;

define dylan macro-test generic-definer-test ()
  //---*** Fill this in...
end macro-test generic-definer-test;

define dylan macro-test library-definer-test ()
  //---*** Fill this in...
end macro-test library-definer-test;

define dylan macro-test method-definer-test ()
  //---*** Fill this in...
end macro-test method-definer-test;

define dylan macro-test module-definer-test ()
  //---*** Fill this in...
end macro-test module-definer-test;

define dylan macro-test variable-definer-test ()
  //---*** Fill this in...
end macro-test variable-definer-test;


/// Dylan extension macros

define dylan-extensions macro-test function-definer-test ()
  //---*** Fill this in...
end macro-test function-definer-test;

