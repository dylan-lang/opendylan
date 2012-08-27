Module:    apple-dylan-test-suite
Filename:  test-sequence3.dylan
Summary:   Apple Dylan test suite, test-sequence
Version:   29-Oct-93
Copyright: (c) 1993 Apple Computer, Inc.

/*---------------------------------------------
Modified by: Shri Amit(amit)
Date: August 24 1996
Summary: Converted to new testworks protocol
Copyright: (c) 1996 Functional Objects, Inc. 
           All rights reserved.  
----------------------------------------------*/

define test intersection-7 (description: "mixed cases")
  check-true("", intersection(#('a', 'b', 'z'), "acde") = #('a'));
end test intersection-7;

define test intersection-8 (description: "test defaults to id?")
  check-true("", ~("john" == "john"));
  check-true("", intersection
      (#("john", "paul", "george", "ringo"),
       #("richard", "george", "edward", "charles", "john"))
    = #());
end test intersection-8;

define test intersection-9
  (description: "with other test:, returns val from 1st seq")
  let result
    = intersection
        (range(from: -1, below: -5, by: -1),
         #(1, 2, 3, 4),
         test: method (a, b)
                 a.abs = b.abs
               end method);
  check-true("", every?(rcurry(member?, result), #(-1, -3, -2, -4)));
end test intersection-9;

// union

define test union-type (description: "")
  check-true("", instance?(union, <generic-function>));
end test union-type;

define test union-0 (description: "list")
 check-true("", every?
    (method (e)
       member?(e, union(#(#"john", #"paul"), #(#"george", #"edward")))
     end method,
     #(#"john", #"paul", #"george", #"edward")));
end test union-0;

define test union-1 (description: "empty-list")
  check-true("", union(#(), #()).empty?);
end test union-1;

define test union-2 (description: "range")
  check-true("", every?
    (method (e)
       member?
         (e,
          union(range(from: 0, below: 9, by: 2),
                range(from: 1, below: 9, by: 2)))
     end method,
     range(from: 0, below: 9)));
end test union-2;

define test union-3 (description: "deque")
  check-true("", every?
    (method (e)
       member?
         (e,
          union(deque-instance(1, 2, 3, 4, 5),
                deque-instance(4, 6, 8, 10, 12)))
     end method,
     deque-instance(1, 2, 3, 4, 5, 6, 8, 10, 12)));
end test union-3;

define test union-4 (description: "stretchy-vector")
  check-true("", every?
    (method (e)
       member?
         (e,
          union(stretchy-vector-instance(1, 2, 3, 4, 5),
                stretchy-vector-instance(4, 6, 8, 10, 12)))
     end method,
     stretchy-vector-instance(1, 2, 3, 4, 5, 6, 8, 10, 12)));
end test union-4;

define test union-5 (description: "simple-object-vector")
  check-true("", every?
    (method (e)
       member?
         (e,
          union(simple-object-vector-instance(1, 2, 3, 4, 5),
                simple-object-vector-instance(4, 6, 8, 10, 12)))
     end method,
     simple-object-vector-instance(1, 2, 3, 4, 5, 6, 8, 10, 12)));
end test union-5;

define test union-6 (description: "string")
  check-true("", every?
    (method (e)
       member?(e, union("When", ", in the course of human events"))
     end method,
     "When, in the course of human events"));
end test union-6;

define test union-7 (description: "mixed cases")
  check-true("", every?(member?, union(#(1, 2, 3), #[1, 2, 3, 4, 5]), #(#(5, 4, 3, 2, 1))));
end test union-7;

define test union-8 (description: "union with test:")
  check-true("", union(#(#"a", #"b", #"c", #"d", #"e", #"f"),
        #(#"b", #"c", #"d", #"e", #"f", #"g", #"h"),
        test: \&=)
  = #(#"a"));
end test union-8;

// remove-duplicates

define test remove-duplicates-type (description: "")
  check-true("", instance?(remove-duplicates, <generic-function>));
end test remove-duplicates-type;

define test remove-duplicates-0 (description: "list")
  let platter
    = remove-duplicates
        (#(#"spam", #"eggs", #"spam", #"sausage", #"spam", #"spam", #"spam"));
  check-true("", platter.size = 3);
  check-true("", every?(rcurry(member?, platter), #(#"eggs", #"sausage", #"spam")));
end test remove-duplicates-0;

define test remove-duplicates-1 (description: "range")
  check-true("", range(from: 0, below: 5).remove-duplicates = range(from: 0, below: 5));
end test remove-duplicates-1;

define test remove-duplicates-2 (description: "deque")
  check-true("", deque-instance(1, 2, 3, 4, 5, 4, 3, 2, 1).remove-duplicates
  = deque-instance(1, 2, 3, 4, 5));
end test remove-duplicates-2;

define test remove-duplicates-3 (description: "stretchy-vector")
  check-true("", stretchy-vector-instance(1, 2, 3, 4, 5, 4, 3, 2, 1).remove-duplicates
  = stretchy-vector-instance(1, 2, 3, 4, 5));
end test remove-duplicates-3;

define test remove-duplicates-4 (description: "simple-object-vector")
  check-true("", simple-object-vector-instance(1, 2, 3, 4, 5, 4, 3, 2, 1).remove-duplicates
  = simple-object-vector-instance(1, 2, 3, 4, 5));
end test remove-duplicates-4;

define test remove-duplicates-5 (description: "string")
  check-true("", remove-duplicates("Abandon every hope all ye who enter here!")
  = "Abando evryhplwt!");
end test remove-duplicates-5;

define test remove-duplicates-6 (description: "test defaults to id?")
  check-true("", ~("and" == "and"));
  check-true("", remove-duplicates(#("shoes", "and", "ships", "and", "sealingwax"))
    = #("shoes", "and", "ships", "and", "sealingwax"));
end test remove-duplicates-6;

define test remove-duplicates-7 (description: "with test: arg")
  check-true("", remove-duplicates
    (#(#"spam", #"eggs", #"spam", #"sausage", #"spam", #"spam", #"spam"),
     test: \&=)
  = #(#"spam", #"spam", #"spam", #"spam", #"spam"));
  check-true("", remove-duplicates
      (#(#(#"foo", #(#"quote", #"a")), #(#"bar", '%'), #(#"baz", #(#"quote", #"a"))),
       test: method (x, y)
               x.cadr = y.cadr
             end method)
    = #(#(#"foo", #(#"quote", #"a")), #(#"bar", '%')));
end test remove-duplicates-7;

define test remove-duplicates-8 (description: "sequence not modified")
  let s = list(#"a", #"a", #"p", #"b");
  let result = s.remove-duplicates;
  check-true("", s = list(#"a", #"a", #"p", #"b") & ~share-struct?(s, result));
end test remove-duplicates-8;

// remove-duplicates!

define test remove-duplicates!-type (description: "")
  check-true("", instance?(remove-duplicates!, <generic-function>));
end test remove-duplicates!-type;

define test remove-duplicates! (description: "list")
  let platter
    = remove-duplicates!
        (#(#"spam", #"eggs", #"spam", #"sausage", #"spam", #"spam", #"spam"));
  check-true("", platter.size = 3);
  check-true("", every?(rcurry(member?, platter), #(#"eggs", #"sausage", #"spam")));
end test remove-duplicates!;

define test remove-duplicates!-1 (description: "range")
  check-true("", remove-duplicates!(range(from: 0, below: 5)) = range(from: 0, below: 5));
end test remove-duplicates!-1;

define test remove-duplicates!-2 (description: "deque")
  check-true("", remove-duplicates!(deque-instance(1, 2, 3, 4, 5, 4, 3, 2, 1))
  = deque-instance(1, 2, 3, 4, 5));
end test remove-duplicates!-2;

define test remove-duplicates!-3 (description: "stretchy-vector")
  check-true("", remove-duplicates!(stretchy-vector-instance(1, 2, 3, 4, 5, 4, 3, 2, 1))
  = stretchy-vector-instance(1, 2, 3, 4, 5));
end test remove-duplicates!-3;

define test remove-duplicates!-4 (description: "simple-object-vector")
  check-true("", remove-duplicates!(simple-object-vector-instance(1, 2, 3, 4, 5, 4, 3, 2, 1))
  = simple-object-vector-instance(1, 2, 3, 4, 5));
end test remove-duplicates!-4;

define test remove-duplicates!-5 (description: "string")
  check-true("", remove-duplicates!("Abandon every hope all ye who enter here!")
  = "Abando evryhplwt!");
end test remove-duplicates!-5;

define test remove-duplicates!-6 (description: "test defaults to id?")
  check-true("", ~("and" == "and"));
  check-true("", remove-duplicates!(#("shoes", "and", "ships", "and", "sealingwax"))
    = #("shoes", "and", "ships", "and", "sealingwax"));
end test remove-duplicates!-6;

define test remove-duplicates!-7 (description: "with test: arg")
  check-true("", remove-duplicates!
    (#(#"spam", #"eggs", #"spam", #"sausage", #"spam", #"spam", #"spam"),
     test: \&=)
  = #(#"spam", #"spam", #"spam", #"spam", #"spam"));
  check-true("", remove-duplicates!
      (#(#(#"foo", #(#"quote", #"a")), #(#"bar", '%'), #(#"baz", #(#"quote", #"a"))),
       test: method (x, y)
               x.cadr = y.cadr
             end method)
    = #(#(#"foo", #(#"quote", #"a")), #(#"bar", '%')));
end test remove-duplicates!-7;

define test copy-sequence-type (description: "")
  check-true("", instance?(copy-sequence, <generic-function>));
end test copy-sequence-type;

define test copy-sequence-0 (description: "list")
  let s = #(#"a", #"b", #"c", #"d");
  let new-s = s.copy-sequence;
  check-true("", new-s = s & ~(new-s == s));
end test copy-sequence-0;

define test copy-sequence-2 (description: "range")
  let s = range(from: 0, below: 5);
  let new-s = s.copy-sequence;
  check-true("", new-s = s & ~(new-s == s));
end test copy-sequence-2;

define test copy-sequence-3 (description: "deque")
  let s = deque-instance(1, 2, 3, 4, 5);
  let new-s = s.copy-sequence;
  check-true("", new-s = s & ~(new-s == s));
end test copy-sequence-3;

define test copy-sequence-4 (description: "stretchy-vector")
  let s = stretchy-vector-instance(1, 2, 3, 4, 5);
  let new-s = s.copy-sequence;
  check-true("", new-s = s & ~(new-s == s));
end test copy-sequence-4;

define test copy-sequence-5 (description: "simple-object-vector")
  check-true("", begin
    let s = simple-object-vector-instance(1, 2, 3, 4, 5);
    let new-s = s.copy-sequence;
    new-s = s & ~(new-s == s)
  end);
  check-true("", copy-sequence(#[#"a", #"b", #"c", #"d"]) = #[#"a", #"b", #"c", #"d"]);
end test copy-sequence-5;

define test copy-sequence-6 (description: "with start:")
  check-true("", copy-sequence(#[#"a", #"b", #"c", #"d"], start: 1) = #[#"b", #"c", #"d"]);
end test copy-sequence-6;

define test copy-sequence-7 (description: "with end:")
  check-true("", copy-sequence(#(#"a", #"b", #"c", #"d"), end: 3) = #(#"a", #"b", #"c"));
end test copy-sequence-7;

define test copy-sequence-8 (description: "with start: and end:")
  check-true("", copy-sequence("abcd", start: 1, end: 3) = "bc");
end test copy-sequence-8;

define test concatenate-as-type (description: "")
  check-true("", instance?(concatenate-as, <generic-function>));
end test concatenate-as-type;

define test concatenate-as-0 (description: "list")
  let in-1 = list(1, 2, 3, 4, 5);
  let in-2 = list(6, 7, 8, 9, 10);
  let result = concatenate-as(<list>, in-1, in-2);
  check-true("", instance?(result, <list>) & result = #(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
end test concatenate-as-0;

define test concatenate-as-1 (description: "range")
  let in-1 = range(from: 1, below: 6);
  let in-2 = range(from: 6, below: 11);
  let result = concatenate-as(<list>, in-1, in-2);
  check-true("", instance?(result, <list>) & result = #(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
end test concatenate-as-1;

define test concatenate-as-2 (description: "deque")
  begin
    let in-1 = list(1, 2, 3, 4, 5);
    let in-2 = list(6, 7, 8, 9, 10);
    let result = concatenate-as(<deque>, in-1, in-2);
    check-true("", instance?(result, <deque>));
    check-true("", result = deque-instance(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
  end;
  begin
      let in-1 = deque-instance(1, 2, 3, 4, 5);
      let in-2 = deque-instance(6, 7, 8, 9, 10);
      let result = concatenate-as(<list>, in-1, in-2);
      check-true("", instance?(result, <list>) & result = #(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
    end;
end test concatenate-as-2;

define test concatenate-as-3 (description: "stretch-vector")
  begin
    let in-1 = list(1, 2, 3, 4, 5);
    let in-2 = list(6, 7, 8, 9, 10);
    let result = concatenate-as(<stretchy-vector>, in-1, in-2);
    check-true("", instance?(result, <stretchy-vector>));
    check-true("", result = stretchy-vector-instance(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
  end;
  begin
      let in-1 = stretchy-vector-instance(1, 2, 3, 4, 5);
      let in-2 = stretchy-vector-instance(6, 7, 8, 9, 10);
      let result = concatenate-as(<list>, in-1, in-2);
      check-true("", instance?(result, <list>) & result = #(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
    end;
end test concatenate-as-3;

define test concatenate-as-4 (description: "simple-object-vector")
  begin
    let in-1 = list(1, 2, 3, 4, 5);
    let in-2 = list(6, 7, 8, 9, 10);
    let result = concatenate-as(<simple-object-vector>, in-1, in-2);
    check-true("", instance?(result, <simple-object-vector>));
    check-true("", result = simple-object-vector-instance(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
  end;
  begin
      let in-1 = simple-object-vector-instance(1, 2, 3, 4, 5);
      let in-2 = simple-object-vector-instance(6, 7, 8, 9, 10);
      let result = concatenate-as(<list>, in-1, in-2);
      check-true("", instance?(result, <list>) & result = #(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
    end;
end test concatenate-as-4;

define test concatenate-as-5 (description: "more args")
  let in-1 = #(1, 2, 3, 4, 5);
  let in-2 = range(from: 6, below: 11);
  let in-3 = deque-instance(10, 9, 8, 7, 6);
  let in-4 = stretchy-vector-instance(5, 4);
  let in-5 = simple-object-vector-instance(3, 2, 1);
  let result = concatenate-as(<list>, in-1, in-2, in-3, in-4, in-5);
  check-true("", instance?(result, <list>));
  check-true("", result = #(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1));
end test concatenate-as-5;

define test concatenate-type (description: "")
  check-true("", instance?(concatenate, <generic-function>));
end test concatenate-type;

define test concatenate-0 (description: "list")
  let in-1 = list(1, 2, 3, 4, 5);
  let in-2 = list(6, 7, 8, 9, 10);
  let result = concatenate(in-1, in-2);
  check-true("", result = #(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
end test concatenate-0;

define test concatenate-1 (description: "range")
  let in-1 = range(from: 1, below: 6);
  let in-2 = range(from: 6, below: 11);
  let result = concatenate(in-1, in-2);
  check-true("", result = range(from: 1, below: 11));
end test concatenate-1;

define test concatenate-2 (description: "deque")
  let in-1 = deque-instance(1, 2, 3, 4, 5);
  let in-2 = deque-instance(6, 7, 8, 9, 10);
  let result = concatenate(in-1, in-2);
  check-true("", result = deque-instance(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
end test concatenate-2;

define test concatenate-3 (description: "stretch-vector")
  let in-1 = stretchy-vector-instance(1, 2, 3, 4, 5);
  let in-2 = stretchy-vector-instance(6, 7, 8, 9, 10);
  let result = concatenate(in-1, in-2);
  check-true("", result = stretchy-vector-instance(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
end test concatenate-3;

define test concatenate-4 (description: "simple-object-vector")
  let in-1 = simple-object-vector-instance(1, 2, 3, 4, 5);
  let in-2 = simple-object-vector-instance(6, 7, 8, 9, 10);
  let result = concatenate(in-1, in-2);
  check-true("", result = simple-object-vector-instance(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
end test concatenate-4;

define test concatenate-5 (description: "more args")
  let in-1 = #(1, 2, 3, 4, 5);
  let in-2 = range(from: 6, below: 11);
  let in-3 = deque-instance(10, 9, 8, 7, 6);
  let in-4 = stretchy-vector-instance(5, 4);
  let in-5 = simple-object-vector-instance(3, 2, 1);
  let result = concatenate(in-1, in-2, in-3, in-4, in-5);
  check-true("", result = #(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1));
end test concatenate-5;
