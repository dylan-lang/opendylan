Module:    apple-dylan-test-suite
Filename:  test-collection2.dylan
Summary:   Apple Dylan test suite, test-collection
Version:   29-Oct-93
Copyright: (c) 1993 Apple Computer, Inc.

/*---------------------------------------------
Modified by: Shri Amit(amit) &
	     James Kirsch(jkirsch)
Date: August 24 1996
Summary: Converted to new testworks protocol
Copyright: (c) 1996 Functional Objects, Inc. 
           All rights reserved.  
----------------------------------------------*/

define test map-into-7 (description: "Simple-object-vector")
  let var = simple-object-vector-instance(1, 2, 3);
  check-equal("", map-into
    (var,
     method (x)
       x * -1;
     end method,
     (simple-object-vector-instance(1, 2, 3))),
   simple-object-vector-instance(-1, -2, -3));
  check-equal("", map-into
      (var,
       method (x)
         x * -1;
       end method,
       (simple-object-vector-instance(1, 2, 3))),
     var);
end test map-into-7;

define test map-into-8 (description: "String")
  let var = "ABC";
  check-equal("", map-into
    (var,
     method (x)
       x;
     end method,
     ("ABC")),
   "ABC");
  check-equal("", map-into
      (var,
       method (x)
         x;
       end method,
       ("ABC")),
     var);
end test map-into-8;

define test map-into-9 (description: "with #rest collections")
    let t = #(100, 100, 200, 200);
    map-into(t, \+, t, #(1, 2, 3, 4));
    check-equal("", t, #(101, 102, 203, 204));
end test map-into-9;

define test map-into-11 (description: "not a new collection")
  let col = #(1, 0, 3, 0);
  check-equal("", col, map-into(col, zero?, col));
end test map-into-11;

define test any?-type ()
  check-true("", instance?(any?, <generic-function>));
end test any?-type;

define test any?-0 (description: "list")
  check-equal("", any?(method (e)
         if (e > 5)
           e;
         else
           #f;
         end if;
       end method,
       (#(1, 2, 3, 4, 5, 6, 7))),
   6);
  check-equal("", any?(method (e)
           e > 5;
         end method,
         (#(1, 2, 3, 4, 5))),
    #f);
end test any?;

define test any?-1 (description: "empty-list")
  check-equal("", any?(method (e)
         if (e > 5)
           e;
         else
           #f;
         end if;
       end method,
       (#())),
   #f);
end test any?-1;

define test any?-2 (description: "range")
  check-equal("", any?(method (e)
         if (e > 5)
           e;
         else
           #f;
         end if;
       end method,
       (range(from: -5, below: 7))),
   6);
  check-equal("", any?(method (e)
           if (e > 5)
             e;
           else
             #f;
           end if;
         end method,
         (range(from: -5, below: 6))),
     #f);
end test any?-2;

define test any?-3 (description: "deque")
  check-equal("", any?(method (e)
         if (e > 5)
           e;
         else
           #f;
         end if;
       end method,
       (deque-instance(1, 2, 3, 4, 5, 6, 7))),
   6);
  check-equal("", any?(method (e)
           if (e > 5)
             e;
           else
             #f;
           end if;
         end method,
         (deque-instance(1, 2, 3, 4, 5))),
     #f);
end test any?-3;

define test any?-4 (description: "table")
  check-equal("", any?(method (e)
         if (e > 5)
           e;
         else
           #f;
         end if;
       end method,
       (table-instance(#(1, 2), #(3, 4), #(5, 6)))),
   6);
  check-equal("", any?(method (e)
           if (e > 5)
             e;
           else
             #f;
           end if;
         end method,
         (table-instance(#(1, 2), #(3, 4)))),
     #f);
end test any?-4;

define test any?-5 (description: "stretchy-vector")
  check-equal("", any?(method (e)
         if (e > 5)
           e;
         else
           #f;
         end if;
       end method,
       (stretchy-vector-instance(1, 2, 3, 4, 5, 6))),
   6);
  check-equal("", any?(method (e)
           if (e > 5)
             e;
           else
             #f;
           end if;
         end method,
         (stretchy-vector-instance(1, 2, 3, 4, 5))),
     #f);
end test any?-5;

define test any?-6 (description: "simple-object-vector")
  check-equal("", any?(method (e)
         if (e > 5)
           e;
         else
           #f;
         end if;
       end method,
       (simple-object-vector-instance(1, 2, 3, 4, 5, 6))),
   6);
  check-equal("", any?(method (e)
           if (e > 5)
             e;
           else
             #f;
           end if;
         end method,
         (simple-object-vector-instance(1, 2, 3, 4, 5))),
     #f);
end test any?-6;

define test any?-7 (description: "string")
  check-equal("", any?(method (e)
         if (e = 'c')
           "Yes";
         else
           #f;
         end if;
       end method,
       ("abcde")),
   "Yes");
  check-equal("", any?(method (e)
           if (e = 'c')
             e;
           else
             #f;
           end if;
         end method,
         ("ab")),
     #f);
end test any?-7;

define test any?-8 (description: "multiple collections")
  check-true("", any?(\>, #(1, 2, 3, 4), #(5, 4, 3, 2)));
end test any?-8;

/* commented out by amit - look at ~amit/dylan/lib/test-suites/issues
define test any?-9 (description: "with #rest collections")
  check-equal("", any?(method (x, y)
         if (instance?(x, <character>) & instance?(y, <vector>))
           pair(x, y);
         else
           #f;
         end if;
       end method,
       (#[1, 3, '5', 'a', 7],
       #(99, 'a', 4, #[], 2))),
   pair('a', make(<vector>)));
end test any?-9;

define test any?-10 (description: "only returns 1st value")
    let (a, b)
      = any?(method (x, y)
               if (instance?(x, <character>) & instance?(y, <vector>))
                 values(x, y);
               else
                 #f;
               end if;
             end method,
             (#[1, 3, '5', 'a', 7],
             #(99, 'a', 4, #[], 2)));
    check-equal("", list(a, b), #('a', #f));
end test any?-10;
*/

define test any?-11 (description: "Stops on first true return")
  
    let var = 0;
    any?(method (x)
           if ((var := x) = 4)
             var;
           else
             #f;
           end if;
         end method,
         (#(1, 2, 3, 4, 5, 6, 7)));
  check-equal("", var,4);
end test any?-11;

define test every?-type ()
  check-true("", instance?(every?, <generic-function>));
end test every?-type;

define test every?-0 (description: "list")
  check-true("", every?
    (method (e)
       if (e > -5)
         e;
       else
         #f;
       end if;
     end method,
     #(1, 2, 3, 4, 5, 6, 7)));

  check-false("", every?
      (method (e)
         e > 6;
       end method,
       #(1, 2, 3, 4, 5)));
end test;

define test every?-1 (description: "empty-list")
  check-equal("", every?
    (method (e)
       if (e > 5)
         e;
       else
         #f;
       end if;
     end method,
     (#())),
   #t);
end test every?-1;

define test every?-2 (description: "range")
  check-equal("", every?
    (method (e)
       if (e > -10)
         e;
       else
         #f;
       end if;
     end method,
     (range(from: -5, below: 7))),
   #t);
  check-equal("", every?
      (method (e)
         if (e > 5)
           e;
         else
           #f;
         end if;
       end method,
       (range(from: -5, below: 6))),
     #f);
end test every?-2;

define test every?-3 (description: "deque")
  check-equal("", every?
    (method (e)
       if (e > -5)
         e;
       else
         #f;
       end if;
     end method,
     (deque-instance(1, 2, 3, 4, 5, 6, 7))),
   #t);
  check-equal("", every?
      (method (e)
         if (e > 5)
           e;
         else
           #f;
         end if;
       end method,
       (deque-instance(1, 2, 3, 4, 5))),
     #f);
end test every?-3;

define test every?-4 (description: "table")
  check-equal("", every?
    (method (e)
       if (e > -5)
         e;
       else
         #f;
       end if;
     end method,
     (table-instance(#(1, 2), #(3, 4), #(5, 6)))),
  #t);
  check-equal("", every?
      (method (e)
         if (e > 5)
           e;
         else
           #f;
         end if;
       end method,
       (table-instance(#(1, 2), #(3, 4)))),
     #f);
end test every?-4;

define test every?-5 (description: "stretchy-vector")
  check-equal("", every?
    (method (e)
       if (e > -5)
         e;
       else
         #f;
       end if;
     end method,
     (stretchy-vector-instance(1, 2, 3, 4, 5, 6))),
  #t);
  check-equal("", every?
      (method (e)
         if (e > 5)
           e;
         else
           #f;
         end if;
       end method,
       (stretchy-vector-instance(1, 2, 3, 4, 5))),
     #f);
end test every?-5;

define test every?-6 (description: "simple-object-vector")
  check-equal("", every?
    (method (e)
       if (e > -5)
         e;
       else
         #f;
       end if;
     end method,
     (simple-object-vector-instance(1, 2, 3, 4, 5, 6))),
   #t);
  check-equal("", every?
      (method (e)
         if (e > 5)
           e;
         else
           #f;
         end if;
       end method,
       (simple-object-vector-instance(1, 2, 3, 4, 5))),
     #f);
end test every?-6;

define test every?-7 (description: "string")
  check-equal("", every?
    (method (e)
       e = 'c';
     end method,
     ("cccc")),
   #t);
  check-equal("", every?
      (method (e)
         if (e = 'c')
           e;
         else
           #f;
         end if;
       end method,
       ("ab")),
     #f);
end test every?-7;

define test every?-8 (description: "with #rest collections")
  check-true("", ~every?
     (method (x, y)
        if (instance?(x, <character>) & instance?(y, <vector>))
          pair(x, y);
        else
          #f;
        end if;
      end method,
      #[1, 3, '5', 'a', 7],
      #(99, 'a', 4, #[], 2)));
  check-true("", every?
      (method (x, y)
         if (instance?(x, <character>) | instance?(y, <number>))
           pair(x, y);
         else
           #f;
         end if;
       end method,
       #['a', 3, '5', '7', #"a"],
       #(99, 6, 4, #[], 2)));
end test every?-8;

define test every?-9 (description: "Stops on first false return")
    let var = 0;
    check-equal("", 
     begin
      every?
      (method (x)
         if ((var := x) < 4)
           var;
         else
           #f;
         end if;
       end method,
       #(1, 2, 3, 4, 5, 6, 7));
     var;
    end,
  4);
end test every?-9;

define test reduce-type ()
  check-true("", instance?(reduce, <generic-function>));
end test reduce-type;

define test reduce-0 (description: "list")
  let high-score = 10;
  check-equal("", reduce(\+, 0, list(1, 2, 3, 4)), 10);
  check-equal("", reduce(max, high-score, #(3, 1, 4, 1, 5, 9)), 10);
  check-equal("", reduce(max, high-score, #(3, 12, 9, 8, 8, 6)), 12);
end test reduce;

define test reduce-1 (description: "empty-list")
  check-equal("", reduce(\+, 0, #()), 0);
end test reduce-1;

define test reduce-2 (description: "range")
  check-equal("", reduce(\+, 0, range(from: -5, below: 5)), -5);
end test reduce-2;

define test reduce-3 (description: "deque")
  check-equal("", reduce(\+, 0, deque-instance(1, 2, 3, 4, 5)), 15);
end test reduce-3;

define test reduce-4 (description: "table")
  check-equal("", reduce(\+, 0, table-instance(#(1, 2), #(3, 4), #(5, 6))), 12);
end test reduce-4;

define test reduce-5 (description: "stretchy-vector")
  check-equal("", reduce(\+, 0, stretchy-vector-instance(1, 2, 3, 4, 5)), 15);
end test reduce-5;

define test reduce-6 (description: "simple-object-vector")
  check-equal("", reduce(\+, 0, simple-object-vector-instance(1, 2, 3, 4, 5)), 15);
end test reduce-6;

define test reduce-7 (description: "string")
  check-equal("", reduce
    (method (l, c)
       pair(c, l);
     end method,
     #(),
     "Hello"),
   #('o', 'l', 'l', 'e', 'H'));
  check-equal("", reduce(list, #"foo", #(1, 2, 3, 4)), #(#(#(#(#"foo", 1), 2), 3), 4));
  check-equal("", reduce(pair, 1, #(2, 3, 4)), pair(pair(pair(1, 2), 3), 4));
end test reduce-7;

define test reduce1-type ()
  check-true("", instance?(reduce1, <generic-function>));
end test reduce1-type;

define test reduce1-0 (description: "simple cases")
  check-true("", #t);
end test reduce1;

define test reduce1-1 (description: "list")
  let high-score = 10;
  check-equal("", reduce1(\+, list(1, 2, 3, 4)), 10);
  check-equal("", reduce1(max, list(high-score, 3, 1, 4, 1, 5, 9)), 10);
end test reduce1-1;

define test reduce1-2 (description: "range")
  check-equal("", reduce1(\+, range(from: -5, below: 5)), -5);
end test reduce1-2;

define test reduce1-3 (description: "deque")
  check-equal("", reduce1(\+, deque-instance(1, 2, 3, 4, 5)), 15);
end test reduce1-3;

define test reduce1-4 (description: "table")
  check-equal("", reduce1(\+, table-instance(#(1, 2), #(3, 4), #(5, 6))), 12);
end test reduce1-4;

define test reduce1-5 (description: "stretchy-vector")
  check-equal("", reduce1(\+, stretchy-vector-instance(1, 2, 3, 4, 5)), 15);
end test reduce1-5;

define test reduce1-6 (description: "simple-object-vector")
  check-equal("", reduce1(\+, simple-object-vector-instance(1, 2, 3, 4, 5)), 15);
end test reduce1-6;

define test reduce1-7 (description: "string")
  check-equal("", reduce1
    (method (l, c)
       pair(c, l);
     end method,
     "Hello"),
       pair('o', pair('l', pair('l', pair('e', 'H')))));
end test reduce1-7;

define test reduce1-8
  (description: "this should work like reduce w foo as init-val and empty list")
  check-equal("", reduce1(\+, #(#"foo")), #"foo");
end test reduce1-8;

define test member?-type ()
  check-true("", instance?(member?, <generic-function>));
end test member?-type;

// note -- must return a boolean, see design change note #16

define test member?-0 (description: "list")
  check-equal("", member?(5, list(1, 2, 3, 4, 5, 6)), #t);
  check-true("", ~member?(5, list(1, 2, 3, 4)));
end test member?;

define test member?-1 (description: "empty-list")
  check-true("", ~member?(5, #()));
end test member?-1;

define test member?-2 (description: "range")
  check-equal("", member?(5, range(from: -5, below: 6)), #t);
  check-true("", ~member?(5, range(from: -5, below: 5)));
end test member?-2;

define test member?-3 (description: "deque")
  check-equal("", member?(5, deque-instance(1, 2, 3, 4, 5, 6)), #t);
  check-true("", ~member?(5, deque-instance(1, 2, 3, 4)));
end test member?-3;

define test member?-4 (description: "deque")
  check-equal("", member?(6, table-instance(#(1, 2), #(3, 4), #(5, 6))), #t);
  check-true("", ~member?(5, table-instance(#(1, 2), #(3, 4), #(5, 6))));
end test member?-4;

define test member?-5 (description: "stretchy-vector")
  check-equal("", member?(6, stretchy-vector-instance(1, 2, 3, 4, 5, 6)), #t);
  check-true("", ~member?(5, stretchy-vector-instance(1, 2, 3, 4)));
end test member?-5;

define test member?-6 (description: "simple-object-vector")
  check-equal("", member?(6, simple-object-vector-instance(1, 2, 3, 4, 5, 6)), #t);
  check-true("", ~member?(5, simple-object-vector-instance(1, 2, 3, 4)));
end test member?-6;

define test member?-7 (description: "string")
  check-equal("", member?('c', "abcdef"), #t );
  check-true("", ~member?('c', "abdef"));
end test member?-7;
