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

// mixed cases
define test intersection-7 ()
  check-true("", intersection(#('a', 'b', 'z'), "acde") = #('a'));
end test intersection-7;

// test defaults to id?
define test intersection-8 ()
  check-true("", ~("john" == "john"));
  check-true("", intersection
      (#("john", "paul", "george", "ringo"),
       #("richard", "george", "edward", "charles", "john"))
    = #());
end test intersection-8;

// with other test:, returns val from 1st seq
define test intersection-9 ()
  let result
    = intersection
        (range(from: -1, above: -5, by: -1),
         #(1, 2, 3, 4),
         test: method (a, b)
                 a.abs = b.abs
               end method);
  check-true("", every?(rcurry(member?, result), #(-1, -3, -2, -4)));
end test intersection-9;

// union

define test union-type ()
  check-true("", instance?(union, <generic-function>));
end test union-type;

// list
define test union-0 ()
 check-true("", every?
    (method (e)
       member?(e, union(#(#"john", #"paul"), #(#"george", #"edward")))
     end method,
     #(#"john", #"paul", #"george", #"edward")));
end test union-0;

// empty-list
define test union-1 ()
  check-true("", union(#(), #()).empty?);
end test union-1;

// range
define test union-2 ()
  check-true("", every?
    (method (e)
       member?
         (e,
          union(range(from: 0, below: 9, by: 2),
                range(from: 1, below: 9, by: 2)))
     end method,
     range(from: 0, below: 9)));
end test union-2;

// deque
define test union-3 ()
  check-true("", every?
    (method (e)
       member?
         (e,
          union(deque-instance(1, 2, 3, 4, 5),
                deque-instance(4, 6, 8, 10, 12)))
     end method,
     deque-instance(1, 2, 3, 4, 5, 6, 8, 10, 12)));
end test union-3;

// stretchy-vector
define test union-4 ()
  check-true("", every?
    (method (e)
       member?
         (e,
          union(stretchy-vector-instance(1, 2, 3, 4, 5),
                stretchy-vector-instance(4, 6, 8, 10, 12)))
     end method,
     stretchy-vector-instance(1, 2, 3, 4, 5, 6, 8, 10, 12)));
end test union-4;

// simple-object-vector
define test union-5 ()
  check-true("", every?
    (method (e)
       member?
         (e,
          union(vector(1, 2, 3, 4, 5),
                vector(4, 6, 8, 10, 12)))
     end method,
     vector(1, 2, 3, 4, 5, 6, 8, 10, 12)));
end test union-5;

// string
define test union-6 ()
  check-true("", every?
    (method (e)
       member?(e, union("When", ", in the course of human events"))
     end method,
     "When, in the course of human events"));
end test union-6;

// mixed cases
define test union-7 ()
  check-true("", every?(member?, union(#(1, 2, 3), #[1, 2, 3, 4, 5]), #(#(5, 4, 3, 2, 1))));
end test union-7;

// union with test:
define test union-8 ()
  check-true("", union(#(#"a", #"b", #"c", #"d", #"e", #"f"),
        #(#"b", #"c", #"d", #"e", #"f", #"g", #"h"),
        test: \&=)
  = #(#"a"));
end test union-8;

// remove-duplicates

define test remove-duplicates-type ()
  check-true("", instance?(remove-duplicates, <generic-function>));
end test remove-duplicates-type;

// list
define test remove-duplicates-0 ()
  let platter
    = remove-duplicates
        (#(#"spam", #"eggs", #"spam", #"sausage", #"spam", #"spam", #"spam"));
  check-true("", platter.size = 3);
  check-true("", every?(rcurry(member?, platter), #(#"eggs", #"sausage", #"spam")));
end test remove-duplicates-0;

// range
define test remove-duplicates-1 ()
  check-true("", range(from: 0, below: 5).remove-duplicates = range(from: 0, below: 5));
end test remove-duplicates-1;

// deque
define test remove-duplicates-2 ()
  check-true("", deque-instance(1, 2, 3, 4, 5, 4, 3, 2, 1).remove-duplicates
  = deque-instance(1, 2, 3, 4, 5));
end test remove-duplicates-2;

// stretchy-vector
define test remove-duplicates-3 ()
  check-true("", stretchy-vector-instance(1, 2, 3, 4, 5, 4, 3, 2, 1).remove-duplicates
  = stretchy-vector-instance(1, 2, 3, 4, 5));
end test remove-duplicates-3;

// simple-object-vector
define test remove-duplicates-4 ()
  check-true("", vector(1, 2, 3, 4, 5, 4, 3, 2, 1).remove-duplicates
  = vector(1, 2, 3, 4, 5));
end test remove-duplicates-4;

// string
define test remove-duplicates-5 ()
  check-true("", remove-duplicates("Abandon every hope all ye who enter here!")
  = "Abando evryhplwt!");
end test remove-duplicates-5;

// test defaults to id?
define test remove-duplicates-6 ()
  check-true("", ~("and" == "and"));
  check-true("", remove-duplicates(#("shoes", "and", "ships", "and", "sealingwax"))
    = #("shoes", "and", "ships", "and", "sealingwax"));
end test remove-duplicates-6;

// with test: arg
define test remove-duplicates-7 ()
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

// sequence not modified
define test remove-duplicates-8 ()
  let s = list(#"a", #"a", #"p", #"b");
  let result = s.remove-duplicates;
  check-true("", s = list(#"a", #"a", #"p", #"b") & ~share-struct?(s, result));
end test remove-duplicates-8;

// remove-duplicates!

define test remove-duplicates!-type ()
  check-true("", instance?(remove-duplicates!, <generic-function>));
end test remove-duplicates!-type;

// list
define test remove-duplicates!-0 ()
  let platter
    = remove-duplicates!
        (#(#"spam", #"eggs", #"spam", #"sausage", #"spam", #"spam", #"spam"));
  check-true("", platter.size = 3);
  check-true("", every?(rcurry(member?, platter), #(#"eggs", #"sausage", #"spam")));
end test remove-duplicates!-0;

// range
define test remove-duplicates!-1 ()
  check-true("", remove-duplicates!(range(from: 0, below: 5)) = range(from: 0, below: 5));
end test remove-duplicates!-1;

// deque
define test remove-duplicates!-2 ()
  check-true("", remove-duplicates!(deque-instance(1, 2, 3, 4, 5, 4, 3, 2, 1))
  = deque-instance(1, 2, 3, 4, 5));
end test remove-duplicates!-2;

// stretchy-vector
define test remove-duplicates!-3 ()
  check-true("", remove-duplicates!(stretchy-vector-instance(1, 2, 3, 4, 5, 4, 3, 2, 1))
  = stretchy-vector-instance(1, 2, 3, 4, 5));
end test remove-duplicates!-3;

// simple-object-vector
define test remove-duplicates!-4 ()
  check-true("", remove-duplicates!(vector(1, 2, 3, 4, 5, 4, 3, 2, 1))
  = vector(1, 2, 3, 4, 5));
end test remove-duplicates!-4;

// string
define test remove-duplicates!-5 ()
  check-true("", remove-duplicates!("Abandon every hope all ye who enter here!")
  = "Abando evryhplwt!");
end test remove-duplicates!-5;

// test defaults to id?
define test remove-duplicates!-6 ()
  check-true("", ~("and" == "and"));
  check-true("", remove-duplicates!(#("shoes", "and", "ships", "and", "sealingwax"))
    = #("shoes", "and", "ships", "and", "sealingwax"));
end test remove-duplicates!-6;

// with test: arg
define test remove-duplicates!-7 ()
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

define test copy-sequence-type ()
  check-true("", instance?(copy-sequence, <generic-function>));
end test copy-sequence-type;

// list
define test copy-sequence-0 ()
  let s = #(#"a", #"b", #"c", #"d");
  let new-s = s.copy-sequence;
  check-true("", new-s = s & ~(new-s == s));
end test copy-sequence-0;

// range
define test copy-sequence-2 ()
  let s = range(from: 0, below: 5);
  let new-s = s.copy-sequence;
  check-true("", new-s = s & ~(new-s == s));
end test copy-sequence-2;

// deque
define test copy-sequence-3 ()
  let s = deque-instance(1, 2, 3, 4, 5);
  let new-s = s.copy-sequence;
  check-true("", new-s = s & ~(new-s == s));
end test copy-sequence-3;

// stretchy-vector
define test copy-sequence-4 ()
  let s = stretchy-vector-instance(1, 2, 3, 4, 5);
  let new-s = s.copy-sequence;
  check-true("", new-s = s & ~(new-s == s));
end test copy-sequence-4;

// simple-object-vector
define test copy-sequence-5 ()
  check-true("", begin
    let s = vector(1, 2, 3, 4, 5);
    let new-s = s.copy-sequence;
    new-s = s & ~(new-s == s)
  end);
  check-true("", copy-sequence(#[#"a", #"b", #"c", #"d"]) = #[#"a", #"b", #"c", #"d"]);
end test copy-sequence-5;

// with start:
define test copy-sequence-6 ()
  check-true("", copy-sequence(#[#"a", #"b", #"c", #"d"], start: 1) = #[#"b", #"c", #"d"]);
end test copy-sequence-6;

// with end:
define test copy-sequence-7 ()
  check-true("", copy-sequence(#(#"a", #"b", #"c", #"d"), end: 3) = #(#"a", #"b", #"c"));
end test copy-sequence-7;

// with start: and end:
define test copy-sequence-8 ()
  check-true("", copy-sequence("abcd", start: 1, end: 3) = "bc");
end test copy-sequence-8;

define test concatenate-as-type ()
  check-true("", instance?(concatenate-as, <generic-function>));
end test concatenate-as-type;

// list
define test concatenate-as-0 ()
  let in-1 = list(1, 2, 3, 4, 5);
  let in-2 = list(6, 7, 8, 9, 10);
  let result = concatenate-as(<list>, in-1, in-2);
  check-true("", instance?(result, <list>) & result = #(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
end test concatenate-as-0;

// range
define test concatenate-as-1 ()
  let in-1 = range(from: 1, below: 6);
  let in-2 = range(from: 6, below: 11);
  let result = concatenate-as(<list>, in-1, in-2);
  check-true("", instance?(result, <list>) & result = #(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
end test concatenate-as-1;

// deque
define test concatenate-as-2 ()
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

// stretch-vector
define test concatenate-as-3 ()
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

// simple-object-vector
define test concatenate-as-4 ()
  begin
    let in-1 = list(1, 2, 3, 4, 5);
    let in-2 = list(6, 7, 8, 9, 10);
    let result = concatenate-as(<simple-object-vector>, in-1, in-2);
    check-true("", instance?(result, <simple-object-vector>));
    check-true("", result = vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
  end;
  begin
      let in-1 = vector(1, 2, 3, 4, 5);
      let in-2 = vector(6, 7, 8, 9, 10);
      let result = concatenate-as(<list>, in-1, in-2);
      check-true("", instance?(result, <list>) & result = #(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
    end;
end test concatenate-as-4;

// more args
define test concatenate-as-5 ()
  let in-1 = #(1, 2, 3, 4, 5);
  let in-2 = range(from: 6, below: 11);
  let in-3 = deque-instance(10, 9, 8, 7, 6);
  let in-4 = stretchy-vector-instance(5, 4);
  let in-5 = vector(3, 2, 1);
  let result = concatenate-as(<list>, in-1, in-2, in-3, in-4, in-5);
  check-true("", instance?(result, <list>));
  check-true("", result = #(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1));
end test concatenate-as-5;

define test concatenate-type ()
  check-true("", instance?(concatenate, <generic-function>));
end test concatenate-type;

// list
define test concatenate-0 ()
  let in-1 = list(1, 2, 3, 4, 5);
  let in-2 = list(6, 7, 8, 9, 10);
  let result = concatenate(in-1, in-2);
  check-true("", result = #(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
end test concatenate-0;

// range
define test concatenate-1 ()
  let in-1 = range(from: 1, below: 6);
  let in-2 = range(from: 6, below: 11);
  let result = concatenate(in-1, in-2);
  check-true("", result = range(from: 1, below: 11));
end test concatenate-1;

// deque
define test concatenate-2 ()
  let in-1 = deque-instance(1, 2, 3, 4, 5);
  let in-2 = deque-instance(6, 7, 8, 9, 10);
  let result = concatenate(in-1, in-2);
  check-true("", result = deque-instance(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
end test concatenate-2;

// stretch-vector
define test concatenate-3 ()
  let in-1 = stretchy-vector-instance(1, 2, 3, 4, 5);
  let in-2 = stretchy-vector-instance(6, 7, 8, 9, 10);
  let result = concatenate(in-1, in-2);
  check-true("", result = stretchy-vector-instance(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
end test concatenate-3;

// simple-object-vector
define test concatenate-4 ()
  let in-1 = vector(1, 2, 3, 4, 5);
  let in-2 = vector(6, 7, 8, 9, 10);
  let result = concatenate(in-1, in-2);
  check-true("", result = vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
end test concatenate-4;

// more args
define test concatenate-5 ()
  let in-1 = #(1, 2, 3, 4, 5);
  let in-2 = range(from: 6, below: 11);
  let in-3 = deque-instance(10, 9, 8, 7, 6);
  let in-4 = stretchy-vector-instance(5, 4);
  let in-5 = vector(3, 2, 1);
  let result = concatenate(in-1, in-2, in-3, in-4, in-5);
  check-true("", result = #(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1));
end test concatenate-5;
