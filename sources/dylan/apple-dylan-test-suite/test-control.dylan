Module:    apple-dylan-test-suite
Filename:  test-control.dylan
Summary:   Apple Dylan test suite, test-control
Version:   29-Oct-93
Copyright: (c) 1993 Apple Computer, Inc.

/*---------------------------------------------
Modified by: Shri Amit(amit)
Date: August 24 1996
Summary: Converted to new testworks protocol
Copyright: (c) 1996 Functional Objects, Inc. 
           All rights reserved.  
----------------------------------------------*/

// Chapter 8. Conditionals, Flow of Control, and Evaluation

define test truth (description: "")
  check-true("", #t = #t & ~(#t = #f) & #"a" & 0 & #() & #t);
end test truth;

// not sure what this is supposed to be -it acts wierd

define test not-type-0 (description: "")
  check-true("", instance?(\~, <function>));
end test not-type-0;

define test not-0 (description: "")
  check-true("", ~(~100) & ~100 = #f & ~(~#()) & ~#f & ~#t = #f);
end test not-0;

define test if-0 (description: "Simple cases - true")
  check-true("", if (#t)
    #"true"
  else
    #"false"
  end if
  = #"true");
  check-true("", if (even?(100))
      #"even"
    else
      #"odd"
    end if
    = #"even");
  check-true("", if (#"anything")
      #"true"
    else
      #"false"
    end if
    = #"true");
  check-true("", if (#[1, 2, 3])
      #"true"
    else
      #"false"
    end if
    = #"true");
  check-true("", begin
      let x = 0;
      if (#f)
        x := 100
      else
        #f
      end if;
      x
    end
    = 0);
end test if-0;

define test if-1 (description: "Simple cases false")
  check-true("", if (#f)
    #"true"
  else
    #"false"
  end if
  = #"false");
  check-true("", if (odd?(100))
      #"true"
    else
      #"false"
    end if
    = #"false");
end test if-1;

define test if-2 (description: "Dylan is not lisp!")
  check-true("", if (#())
    #"true"
  else
    #"false"
  end if
  = #"true");
end test if-2;

define test when-0 (description: "Simple cases - true")
  check-true("", if (#t)
    #"ok"
  end if
  = #"ok");
  check-true("", if (odd?(100))
      1;
      2;
      3
    end if
    = #f);
  check-true("", if (#t)
      #"oops";
      #"oops";
      #"oops";
      #"oops";
      #"ok"
    end if
    = #"ok");
  check-true("", if (#"anything")
      1;
      2;
      3
    end if
    = 3);
  check-true("", if (#())
      #"a";
      #"b";
      #"c"
    end if
    = #"c");
  check-true("", begin
      let x = 0;
      if (#f)
        x := 100;
        #"oops";
        #"oops"
      end if;
      x
    end
    = 0);
end test when-0;

define test when-1 (description: "Simple cases - false")
  check-true("", if (#f)
    1;
    2;
    3
  end if
  = #f);
  check-true("", if (odd?(100))
      1;
      2;
      3
    end if
    = #f);
end test when-1;

define test when-2 (description: "If no forms, #f is returned")
  check("", \=, begin if (#t) #(); end if; end, #());
  check("", \=, begin if (3 < 2) #(); end if; end, #f);
end test when-2;

define test unless-0 (description: "Simple cases - true")
  check-true("", unless (#t)
    1;
    2;
    3
  end unless
  = #f);
  check-true("", unless (even?(100))
      1;
      2;
      3
    end unless
    = #f);
  check-true("", unless (#"anything")
      1;
      2;
      3
    end unless
    = #f);
  check-true("", unless (#())
      #"a";
      #"b";
      #"c"
    end unless
    = #f);
  check-true("", begin
      let x = 0;
      unless (#t)
        x := 100;
        #"oops";
        #"oops"
      end unless;
      x
    end
    = 0);
end test unless-0;

define test unless-1 (description: "Simple cases - false")
  check-true("", unless (#f)
    #"ok"
  end unless
  = #"ok");
  check-true("", unless (#f)
      #"oops";
      #"oops";
      #"oops";
      #"oops";
      #"ok"
    end unless
    = #"ok");
end test unless-1;

define test unless-2 (description: "If no forms, #f is returned")
  check-true("", unless (#t)
    #()
  end unless
  = #f);
  check-true("", unless (3 < 2)
      #()
    end unless
    = #());
end test unless-2;

define test cond-0 (description: "Simple cases")
  check-true("", if (2 < 2)
    "2 less than 2"
  elseif (3 > 3)
    "3 greater than 3"
  elseif (5 = 5)
    "5 equals 5"
  else
    "none of above"
  end if
  = "5 equals 5");
  check-true("", if (2 < 2)
      "2 less than 2"
    elseif (3 > 3)
      "3 greater than 3"
    elseif (5 = 1)
      "5 equals 1"
    else
      "none of above"
    end if
    = "none of above");
end test cond-0;

define test cond-1 (description: "Remaining tests not evaluated")
 check-true("", begin
    let x = 0;
    if (100 = 100)
      #t
    elseif ((x := 100) = #())
      #f
    end if;
    x
  end
  = 0);
end test cond-1;

define test cond-2 (description: "Returns #f if no test evals to true")
  check-false("", if (2 = 3)
		    #f
		  elseif (#t = #f)
		    #f
		  end);
end test cond-2;

define test cond-3
  (description: "No consequents, returns 1st value of test case")
  check-true("", begin
    let (a, b, c)
      = if (3 = 2)
          #"foo"
        elseif (values(3, 4, 5))
        else
          #t
        end if;
    list(a, b, c)
  end
  = #(3, #f, #f));
end test cond-3;

define test cond-4 (description: "Returns all values from last consequent")
  check-true("", begin
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
  end
  = #(9, 8, 7));
end test cond-4;

define test cond-5 (description: "Some devious boundaries")
  check-true("", if (#t)
    #"always-true"
  end if
  = #"always-true");
  check-true("", if (#())
      #()
    end if
    = #());
  check-true("", if (#f)
      #"oops"
    end if
    = #f);
  check-true("", if (#f)
    elseif (#"ok")
    end if
    = #f);
  check-true("", if (#t)
      #"ok"
    end if
    = #"ok");
  check-true("", if (#f)
    else
      #"ok"
    end if
    = #"ok");
  check-true("", if (#"oops")
      #"oops";
      #"oops";
      #"ok"
    end if
    = #"ok");
  check-true("", if (#t)
      #"oops";
      #"oops";
      #"oops";
      #"ok"
    end if
    = #"ok");
  check-true("", if (#f)
    elseif (#"oops")
      #"oops";
      #"oops";
      #"ok"
    end if
    = #"ok");
  check-true("", if (#f)
    else
      #"oops";
      #"oops";
      #"oops";
      #"ok"
    end if
    = #"ok");
  check-true("", if (#f)
    else
      #"oops";
      #"oops";
      #"oops";
      #()
    end if
    = #());
end test cond-5;

define test case-0 (description: "Simple cases.")
  check-true("", select (3 + 2)
    1 =>;
    2 =>;
    4, 5, 6 =>
      "5ish";
    otherwise =>
      "none of above"
  end select
  = "5ish");
  check-true("", select (30 + 2)
      1 =>;
      2 =>;
      5 =>;
      otherwise =>
        "none of above"
    end select
    = "none of above");
     begin
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
              end select
            end method student;
      check-true("", student(#"art") = "Don't quit your day job.");
      check-true("", student(#"engineering") = "Say, can you fix my VCR?");
      check-true("", student(#"nursing") = "I wish you luck.");
    end
end test case-0;

define test case-1 (description: "If no consequents, #f")
  check-true("", select (1 + 1)
    1 =>;
    2 =>;
    5 =>;
    otherwise =>
      "none of above"
  end select
  = #f);
  check-true("", select (1 + 99)
      1 =>;
      2 =>;
      5 =>;
      otherwise =>
    end select
    = #f);
end test case-1;

define test case-2 (description: "multiple values return")
  check-true("", begin
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
  end
  = #(4, 5, 6));
end test case-2;

define test case-3 (description: "Some cases")
  check-true("", select (999)
    1, 2, 999 =>
      #"ok"
  end select
  = #"ok");
  /* (= (case 999 (#t)) #f) */
  /* (= (case 999 (#t 'ok)) 'ok) */
  check-true("", select (999)
      otherwise =>
        #"ok"
    end select
    = #"ok");
  check-true("", select (999)
      1, 2, 999 =>
        #"oops";
        #"oops";
        #"oops";
        #"ok"
    end select
    = #"ok");
  /* (= (case 999 (#t 'oops 'oops 'oops 'oops 'ok)) 'ok) */
  check-true("", select (999)
      otherwise =>
        #"oops";
        #"oops";
        #"oops";
        #"ok"
    end select
    = #"ok");
  check-true("", select (#"sym1")
      #"sym2" =>
        #"oops";
      #"sym3", #"sym4" =>
        #"ooops";
      #"sym1" =>
        #"ok"
    end select
    = #"ok");
end test case-3;

define test select-0 (description: "Simple cases")
  check-true("", select (10 + 5 by \<)
    12, 12 + 1 =>
      "12 or 13";
    15 - 1, 15 =>
      "14 or 15";
    5 * 3, 16 =>
      "15 or 16";
    99, 100 =>
      "99 or 100"
  end select
  = "15 or 16");
  check-true("", select (100 + 5 by \<)
      12, 12 + 1 =>
        "12 or 13";
      15 - 1, 15 =>
        "14 or 15";
      5 * 3, 16 =>
        "15 or 16";
      99, 100 =>
        "99 or 100";
      otherwise =>
        "else"
    end select
    = "else");
  check-true("", select (5 by instance?)
      <list> =>
        "list";
      <number>, <sequence> =>
        "computational";
      otherwise =>
        "?"
    end select
    = "computational");
  check-true("", select (999 by instance?)
      <number> =>
        #"oops";
        #"oops";
        #"oops";
        #"ok"
    end select
    = #"ok");
  check-true("", select (999 by instance?)
      otherwise =>
        #"oops";
        #"oops";
        #"oops";
        #"ok"
    end select
    = #"ok");
end test select-0;

define test select-1 (description: "If no consequents, #f")
  check-true("", select (1 + 1 by \=)
    1 =>;
    3 - 1 =>;
    5 =>;
    otherwise =>
      "none of above"
  end select
  = #f);
  check-true("", select (1 + 99 by \=)
      1 =>;
      3 - 1 =>;
      5 =>;
      otherwise =>
    end select
    = #f);
  check-true("", ~select (999 by instance?)
       <number> =>
     end select);
  check-true("", ~select (999 by instance?)
       <string>, <number>, <symbol> =>
     end select);
  check-true("", ~select (999 by instance?)
       otherwise =>
     end select);
  /* (not (select 999 instance? (#t))) */
end test select-1;

define test or-0 (description: "Simple cases")
  check-true("", (1 | 2 | 3) = 1); 
  check-true("", (even?(3) | zero?(2) | 0) = 0);
end test or-0;

define test or-1 (description: "None true, return #f")
  check-true("", (even?(3) | odd?(2) | zero?(-1)) = #f);
end test or-1;

define test or-2 (description: "Nothing evaled after one returns true")
  check-true("", begin
    let x = 0;
    #f | 1 | (x := 999);
    x
  end
  = 0);
end test or-2;

// This looks quite buggy...this should work, looks okay to me
//
define test or-3 (description: "Multiple values before last, 1st val returned")
  check-true("", (values(1, 2, 3) | #t) = 1);
  check-true("", begin
      let (a, b, c) = values(1, 2, 3) | #f;
      list(a, b, c);
    end
    = #(1, #f, #f));
end test or-3;

define test or-4 (description: "Multiple values in last, all vals returned")
  check-true("", begin
    let (a, b, c) = #f | values(1, 2, 3);
    list(a, b, c)
  end
  = #(1, 2, 3));

  check-true("", begin
      let (a, b, c) = values(1, 2, 3);
      list(a, b, c);
    end
    = #(1, 2, 3));
end test or-4;

define test or-5
  (description: "1st value only thing that matters to judge truth")
  check-true("", (even?(1) | values(#f, #t) | 3) = 3);
end test or-5;

define test and-0 (description: "Simple cases")
  check-true("", (1 & 2 & 3) = 3); 
  check-true("", (1 & 2 & even?(3)) = #f);
end test and-0;

define test and-1
  (description: "Multiple values as last form returns all vals")
  check-true("", begin
    let (a, b, c) = 1 & 2 & values(1, #f, 3);
    list(a, b, c)
  end
  = #(1, #f, 3));
end test and-1;

// This looks quite buggy...test should work, looks okay to me
//
define test and-2
  (description: "1st value only thing that matters to judge truth")
  check-true("", begin
    let (a, b, c) = values(1, #f, 3) & 1 & 2;
    list(a, b, c);
  end
  = #(2, #f, #f));
  check-true("", (odd?(1) & values(#f, #t) & 3) = #f);
end test and-2;

define test and-3 (description: "Things past 1st false don't get evaled")
  check-true("", begin
    let x = 0;
    values(1, #f) & values(#f, 1) & (x := 999);
    x
  end
  = 0);
end test and-3;

define test begin-0 (description: "No forms, return #f")
  check-true("", begin
  end
  = #f);
end test begin-0;

define test begin-1 (description: "Returns last form")
  check-true("", begin
    1;
    2;
    3
  end
  = 3);
  check-true("", if (3 = 3)
      #"nothing";
      #"something";
      #"anything"
    else
      #f
    end if
    = #"anything");
end test begin-1;

define test begin-2
  (description: "Not just skipping to the last form, I hope?")
  check-true("", begin
    let x = 0;
    begin
      x := 1 + x;
      x := 1 + x;
      x := 1 + x
    end;
    x
  end
  = 3);
end test begin-2;

define test begin-3
  (description: "If last form is multiple values, return them all")
  check-true("", begin
    let (a, b)
      = begin
          #"step1";
          #"step2";
          values(#"step3", #"step4")
        end;
    list(a, b)
  end
  = #(#"step3", #"step4"));
end test begin-3;

define test for-0 (description: "Simple cases")
  check-true("", begin
    let v = 0;
    for (i from 10 to 0 by -1)
      v := v + i;
    end for;
    v
  end
  = 55);
  check-true("", method (n :: <integer>)
      for (i from n to 1 by -1, v = 1 then v * i)
      finally
        v;
      end for
    end method
      (5)
    = 120);
  check-true("", begin
      let m = #();
      for (l = #(0, 1, 2, 3, 4, 5, 6, 7, 8, 9) then l.tail, until: l.empty?)
        m := pair(l.head, m);
      end for;
      m
    end
    = #(9, 8, 7, 6, 5, 4, 3, 2, 1, 0));
end test for-0;

define test for-1 (description: "Multiple vars")
  check-true("", begin
    let i = 0;
    let j = 0;
    let k = 0;
    for (i from 1 to 10, j = i + 1 then i + 1, k = j + 1 then j + 1)
    finally
      i + (j + k);
    end for
  end
  = 33);
  // !@#$ JB used to be 30
  check-true("", begin
      let sum = 0;
      for (i from 0 to 10)
        sum := sum + i;
      end for;
      sum
    end
    = 55);
  check-true("", begin
      let sum = 0;
      for (i from 0,
           j from 10 by -1,
           k from 0,
           l from 10 by -1,
           until: i = j & j = k & k = l)
        sum := sum + i + j + k + l;
      end for;
      sum
    end
    = 100);
end test for-1;

define test for-2
  (description: "doesn't do it the first time if end-test initially true")
  check-true("", begin
    let v = 0;
    for (i from 0, j from 10 by -1, until: i < j)
      v := #f;
    end for;
    v
  end
  = 0);
end test for-2;

define test for-3 (description: "multiple return forms, do all, return last")
  check-true("", begin
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
  end
  = #(#"last", 100));
end test for-3;

define test for-4 (description: "multiple values return")
  check-true("", begin
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
  end
  = #(60, 100));
end test for-4;

// !@#$ JB used to be 10 100

define test for-5 (description: "Some boundary cases")
//  check-true("", for (until: #f)
//  end for
//  = #f);
  check-true("", for (until: #t)
    finally
      #"ok";
    end for
    = #"ok");
  check-true("", for (until: #t)
    finally
      #"oops";
      #"oops";
      #"oops";
      #"ok";
    end for
    = #"ok");
end test for-5;

/// !@#$ JB all for-each's are very disfunctional

/* check issues
define test for-each (description: "simple case")
  check("", \=, 2, do (method (s :: <sequence>)
    block (return)
      for (number in s)
        if (number.even?)
          return(number)
        end if;
      end for
    end block
  end method,
    #(5, 3, 2, 4, 7)));
end test for-each;
*/
define test for-each-1 (description: "Multiple returns")
  check-true("", begin
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
  end
  = #(4, 100));
end test for-each-1;

define test for-each-2 (description: "Multiple values return")
  check-true("", begin
    let (a, b)
      = block (return)
          for (number in #(5, 3, 2, 4, 7))
            if (number.even?)
              return(2 * number, 2 + number)
            end if;
          end for
        end block;
    list(a, b)
  end
  = #(4, 4));
end test for-each-2;

define test for-each-3 (description: "Never true, return #f")
  check-true("", block (return)
    for (number in #(5, 3, 9, 9, 7))
      if (number.even?)
        return(number)
      end if;
    end for
  end block
  = #f);
end test for-each-3;

define test for-each-4 (description: "One runs out first, return #f")
  check-true("", block (return)
    for (number1 in #(5, 3, 9, 2), number2 in #(1, 2, 3))
      if (number1.even?)
        return(number1)
      end if;
    end for
  end block
  = #f);
end test for-each-4;

/*
;;; !@#$ JB NO MORE DOTIMES

(define-test dotimes ()
  "Simple - runs 0 to n inclusive times"
  (= (bind ((count 1))
       (dotimes (i 10 count)
         (set! count (+ count 1))))
     11))

(define-test DOTIMES-1 ()
  "No result form, return #f"
  (=
   ((method ((n <number>))
     (bind ((v 0)) (dotimes (i (+ n 1)) (set! v (+ v i)))))
    10)
   #f))

(define-test DOTIMES-2 ()
  "0 count, never run body"
  (= (bind ((result 0)) (dotimes (i 0 result) (set! result 100))) 0))

(define-test DOTIMES-3 ()
  "Negative count, never run body"
  (= (bind ((result 0)) (dotimes (i -5 result) (set! result 100))) 0))

(define-test DOTIMES-4 ()
  "multiple body forms"
  (=
   (bind ((result 0))
    (dotimes (i 1 result) (set! result 100) (set! result (* result 2))
     (set! result (+ result 300))))
   500))

(define-test DOTIMES-5 ()
  "Multiple values return"
  (=
   (bind
    ((result 0)
     (a b c
      (dotimes (i 1 (values result (* 2 result) #t)) (set! result 100)
       (set! result (* result 2)) (set! result (+ result 300)))))
    (list a b c))
   '(500 1000 #t)))
*/

define test while-0 (description: "simple")
  check-true("", begin
    let v = 0;
    let n = 10;
    while (n > 0)
      v := v + n;
      n := n - 1
    end while;
    v
  end
  = 55);
end test while-0;

define test while-1 (description: "initially false test, never eval body")
  check-true("", begin
    let v = 1;
    let n = 10;
    while (n < 0)
      v := v + n;
      n := n - 1
    end while;
    v
  end
  = 1);
end test while-1;

define test while-2 (description: "While returns #f")
  check-true("", begin
    let v = 0;
    let n = 10;
    while (n > 0)
      v := v + n;
      n := n - 1
    end while
  end
  = #f);
end test while-2;

define test until-0 (description: "simple")
  check-true("", begin
    let v = 0;
    let n = 10;
    until (n < 0)
      v := v + n;
      n := n - 1
    end until;
    v
  end
  = 55);
end test until-0;

define test until-1 (description: "initially negative test")
  check-true("", begin
    let v = 0;
    let n = 10;
    until (n > 0)
      v := v + n;
      n := n - 1
    end until;
    v
  end
  = 0);
end test until-1;

define test until-2 (description: "until returns #f")
  check-true("", begin
    let v = 0;
    let n = 10;
    until (n < 0)
      v := v + n;
      n := n - 1
    end until
  end
  = #f);
end test until-2;

define test bind-exit-0 (description: "Simple")
  check-true("", begin
    let seq = #(1, 3, 5, 2, 7, 9, 10);
    block (exit)
      map(method (item)
            if (item.even?)
              exit(item)
            else
              item
            end if
          end method,
          seq)
    end block
  end
  = 2);
end test bind-exit-0;

define test bind-exit-1 (description: "Normal return")
  check-true("", begin
    let seq = #(1, 3, 5, 7, 9);
    block (exit)
      map(method (item)
            if (item.even?)
              exit(item)
            else
              item
            end if
          end method,
          seq)
    end block
  end
  = #(1, 3, 5, 7, 9));
end test bind-exit-1;

define test bind-exit-2 (description: "no forms, #f returned")
  check-true("", block (exit)
  end block
  = #f);
end test bind-exit-2;

define test bind-exit-3 (description: "exit proc accepts any number of args")
  check-true("", begin
    let seq = #(1, 3, 5, 2, 7, 9);
    let (a, b, c)
      = block (exit)
          map(method (item)
                if (item.even?)
                  exit(item, #t, 2 * item)
                else
                  item
                end if
              end method,
              seq)
        end block;
    list(a, b, c)
  end
  = #(2, #t, 4));
end test bind-exit-3;

define test bind-exit-4 (description: "exit proc is a first-class value")
  check-true("", block (exit)
    local method e (f :: <function>)
            map(method (item)
                  if (item.even?)
                    apply(f, item.list)
                  else
                    item
                  end if
                end method,
                #(1, 3, 5, 2, 7, 9))
          end method e;
    exit.e
  end block
  = 2);
end test bind-exit-4;

define test unwind-protect-0 (description: "Simple case")
  check-true("", begin
    let v = 0;
    block (exit)
      exit();
      v := 99
    cleanup
      v := 1
    end block;
    v
  end
  = 1);
end test unwind-protect-0;

define test unwind-protect-1
  (description: "If normal exit, returns vals returned by protected-form")
  check-true("", begin
    let v = 0;
    block (exit)
      v := 99
    cleanup
      v := 1
    end block
  end
  = 99);
end test unwind-protect-1;

define test quote-0 (description: "Simple case")
  check-equal("", #(#"+", 1, 2), #(#"+", 1, 2));
end test quote-0;

define test apply-0 (description: "Simple cases")
  check-equal("", apply(\+, 1, #(2, 3)), 6);
  check-equal("", apply(list(\+, \*, \/, \-).second, 1, 2, #(3, 4)), 24);
end test apply-0;

define suite test-control-suite ()
  test truth;
  test not-type-0; 
  test not-0;
  test if-0;
  test if-1;
  test if-2;
  test when-0;
  test when-1;
  test when-2;
  test unless-0;
  test unless-1;
  test unless-2;
  test cond-0;
  test cond-1;
  test cond-2;
  test cond-3;
  test cond-4;
  test cond-5;
  test case-0;
  test case-1;
  test case-2;
  test case-3;
  test select-0;
  test select-1;
  test or-0;
  test or-1;
  test or-2; 
  test or-3;
  test or-4;
  test or-5;
  test and-0;
  test and-1;
  test and-2;
  test and-3;
  test begin-0;
  test begin-1;
  test begin-2;
  test begin-3;
  test for-0;
  test for-1;
  test for-2;
  test for-3;
  test for-4;
  test for-5;
//  test for-each;
  test for-each-1;
  test for-each-2;
  test for-each-3;
  test for-each-4;
  test while-0;
  test while-1;
  test while-2;
  test until-0;
  test until-1;
  test until-2;
  test bind-exit-0;
  test bind-exit-1;
  test bind-exit-2;
  test bind-exit-3;
  test bind-exit-4;
  test unwind-protect-0;
  test unwind-protect-1;
  test quote-0;
  test apply-0;
end suite;
