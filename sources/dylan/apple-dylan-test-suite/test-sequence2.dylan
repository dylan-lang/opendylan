Module:    apple-dylan-test-suite
Filename:  test-sequence2.dylan
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

define test add-new!-4 (description: "add old element, using test: argument")
  every?
                     (method (s)
       let collection = s.first;
       let elements = s.second;
       let new-element = s.third;
       let size1 = s.first.size;
       let id-test? = s[3];
       let test = s[4];
       let new-collection = add-new!(collection, new-element);
       check-true("", (size1 = new-collection.size & size1 = elements.size));
       check-true("", collection = new-collection);
       check-true("", if (id-test?)
           collection == new-collection
         else
           #t
         end if);
       check-true("", every?(rcurry(member?, collection), elements));
       check-true("", every?(rcurry(member?, new-collection), elements));
     end method,
     list(list(list(7, 8, 9), list(7, 8, 9), 64, #f, divides?),
          list(range(from: 7, below: 10), list(7, 8, 9), 64, #f, divides?),
          list(vector(7, 8, 9), list(7, 8, 9), 64, #f, divides?),
          list(deque-instance(7, 8, 9), list(7, 8, 9), 64, #t, divides?),
          list(stretchy-vector-instance(7, 8, 9),
               list(7, 8, 9),
               64,
               #t,
               divides?),
          list(simple-object-vector-instance(7, 8, 9),
               list(7, 8, 9),
               64,
               #f,
               divides?),
          list(byte-string-instance('Y', 'o', 'w'),
               list('Y', 'o', 'w'),
               'O',
               #f,
               caseless=?),
          list(unicode-string-instance('Y', 'o', 'w'),
               list('Y', 'o', 'w'),
               'O',
               #f,
               caseless=?)))
end test add-new!-4;

// remove

define test remove-type (description: "")
  check-true("", instance?(remove, <generic-function>));
end test remove-type;

define test remove-0 (description: "list")
  check-true("", remove(#(1, 2, 4, 1, 3, 4, 5), 4) = #(1, 2, 1, 3, 5));
end test remove-0;

define test remove-1 (description: "empty-list")
  check-true("", remove(#(), 4) = #());
end test remove-1;

define test remove-2 (description: "range")
  check-true("", remove(range(from: 1, below: 5), 4) = #(1, 2, 3));
end test remove-2;

define test remove-3 (description: "deque")
  check-true("", remove(deque-instance(1, 2, 3, 4, 5), 4) = deque-instance(1, 2, 3, 5));
end test remove-3;

define test remove-4 (description: "stretchy-vector")
  check-true("", remove(stretchy-vector-instance(1, 2, 3, 4, 5), 4)
  = stretchy-vector-instance(1, 2, 3, 5));
end test remove-4;

define test remove-5 (description: "simple-object-vector")
  check-true("", remove(simple-object-vector-instance(1, 2, 3, 4, 5), 4)
  = simple-object-vector-instance(1, 2, 3, 5));
end test remove-5;

define test remove-6 (description: "string")
  check-true("", remove("Abandon every hope all ye who enter here!", ' ')
  = "Abandoneveryhopeallyewhoenterhere!");
end test remove-6;

define test remove-7 (description: "with test: arg")
  check-true("", remove
    (#(#"a", #"b", #(#"a", #"c"), #"d", #"e", #(#"a", #"c"), #"f"),
     #(#"a", #"c"),
     test: \=)
  = #(#"a", #"b", #"d", #"e", #"f"));
end test remove-7;

define test remove-8 (description: "with count: arg")
  check-true("", remove(#(1, 2, 4, 1, 3, 4, 5), 4, count: 1) = #(1, 2, 1, 3, 4, 5));
end test remove-8;

define test remove-9 (description: "with count: and test: args")
  check-true("", remove("Abandon every hope all ye who enter here!", 'e', test: \&=)
  = "eeeeeeee");
end test remove-9;

define test remove-10
  (description: "doesn't modify sequence, & result doesn't share structure")
  let x = #(#"a", #"b");
  let numbers = list(3, 1, 4, 1, x, 9);
  let result = remove(numbers, 1, test: \>, count: 2);
  check-true("", numbers = list(3, 1, 4, 1, x, 9) & ~share-struct?(numbers, result));
end test remove-10;

//remove!

define test remove!-type (description: "")
  check-true("", instance?(remove!, <generic-function>));
end test remove!-type;

define test remove!-0 (description: "list")
  check-true("", remove!(#(1, 2, 4, 1, 3, 4, 5), 4) = #(1, 2, 1, 3, 5));
end test remove!-0;

define test remove!-1 (description: "empty-list")
  check-true("", remove!(#(), 4) = #());
end test remove!-1;

define test remove!-2 (description: "range")
  check-true("", remove!(range(from: 1, below: 5), 4) = #(1, 2, 3));
end test remove!-2;

define test remove!-3 (description: "deque")
  check-true("", remove!(deque-instance(1, 2, 3, 4, 5), 4) = deque-instance(1, 2, 3, 5));
end test remove!-3;

define test remove!-4 (description: "stretchy-vector")
  check-true("", remove!(stretchy-vector-instance(1, 2, 3, 4, 5), 4)
  = stretchy-vector-instance(1, 2, 3, 5));
end test remove!-4;

define test remove!-5 (description: "simple-object-vector")
  check-true("", remove!(simple-object-vector-instance(1, 2, 3, 4, 5), 4)
  = simple-object-vector-instance(1, 2, 3, 5));
end test remove!-5;

define test remove!-6 (description: "string")
  check-true("", remove!("Abandon every hope all ye who enter here!", ' ')
  = "Abandoneveryhopeallyewhoenterhere!");
end test remove!-6;

define test remove!-7 (description: "with test: arg")
  check-true("", remove!
    (#(#"a", #"b", #(#"a", #"c"), #"d", #"e", #(#"a", #"c"), #"f"),
     #(#"a", #"c"),
     test: \=)
  = #(#"a", #"b", #"d", #"e", #"f"));
end test remove!-7;

define test remove!-8 (description: "with count: arg")
  check-true("", remove!(#(1, 2, 4, 1, 3, 4, 5), 4, count: 1) = #(1, 2, 1, 3, 4, 5));
end test remove!-8;

define test remove!-9 (description: "with cound: and test: args")
  check-true("", remove!("Abandon every hope all ye who enter here!", 'e', test: \&=)
  = "eeeeeeee");
end test remove!-9;

define test choose-type (description: "")
  check-true("", instance?(choose, <generic-function>));
end test choose-type;

define test choose-0 (description: "list")
  check-true("", choose
    (method (e)
       e > 5
     end method,
     #(1, 2, 3, 4, 5, 6))
  = #(6));
end test choose-0;

define test choose-1 (description: "empty list")
  check-true("", choose
    (method (e)
       e > 5
     end method,
     #())
  = #());
end test choose-1;

define test choose-2 (description: "range")
  check-true("", choose(even?, range(from: 0, below: 8)) = #(0, 2, 4, 6));
end test choose-2;

define test choose-3 (description: "deque")
  check-true("", choose(even?, deque-instance(1, 2, 3, 4, 5, 6, 7, 8))
  = deque-instance(2, 4, 6, 8));
end test choose-3;

define test choose-4 (description: "stretchy-vector")
  check-true("", choose(even?, stretchy-vector-instance(1, 2, 3, 4, 5, 6, 7, 8))
  = stretchy-vector-instance(2, 4, 6, 8));
end test choose-4;

define test choose-5 (description: "simple-object-vector")
  check-true("", choose(even?, simple-object-vector-instance(1, 2, 3, 4, 5, 6, 7, 8))
  = simple-object-vector-instance(2, 4, 6, 8));
end test choose-5;

define test choose-6 (description: "string")
  check-true("", choose
    (method (c)
       c >= 'a' & c <= 'z'
     end method,
     "Abandon Hope All Ye Who Enter Here!")
  = "bandonopellehonterere");
  check-true("", choose(even?, #[1, 2, 3, 4, 5, 6, 7, 8]) = #[2, 4, 6, 8]);
  check-true("", choose(even?, range(from: 0, below: 8)) = #(0, 2, 4, 6));
  check-true("", choose(even?, #()) = #());
  check-true("", choose(even?, #(1, 3, 5, 7)) = #());
end test choose-6;

define test choose-7 (description: "returns *new* sequence")
  let t = make(<deque>, fill: 9);
  let new-t = choose(odd?, t) = t;
  check-true("", t = new-t);
  check-true("", ~(t == new-t));
end test choose-7;

define test choose-by-type (description: "")
  check-true("", instance?(choose-by, <generic-function>));
end test choose-by-type;

define test choose-by-0 (description: "list")
  check-true("", choose-by
    (even?,
     #(1, 2, 3, 4, 5, 6, 7, 8),
     #(#"a", #"b", #"c", #"d", #"e", #"f", #"g", #"h"))
  = #(#"b", #"d", #"f", #"h"));
end test choose-by-0;

define test choose-by-1 (description: "Empty list")
  check-true("", choose-by(even?, #(1, 2, 3, 4), #()) = #());
  check-true("", choose-by(even?, #(), #(1, 2, 3, 4)) = #());
end test choose-by-1;

define test choose-by-2 (description: "deque")
  check-true("", choose-by
    (even?,
     deque-instance(1, 2, 3, 4, 5, 6, 7, 8),
     deque-instance(#"a", #"b", #"c", #"d", #"e", #"f", #"g", #"h"))
  = deque-instance(#"b", #"d", #"f", #"h"));
end test choose-by-2;

define test choose-by-3 (description: "range")
  check-true("", choose-by
    (even?, range(from: 0, below: 15), range(from: 0, above: -15, by: -1))
  = #(0, -2, -4, -6, -8, -10, -12, -14));
end test choose-by-3;

define test choose-by-4 (description: "stretchy-vector")
  check-true("", choose-by
    (even?,
     stretchy-vector-instance(1, 2, 3, 4, 5, 6, 7, 8),
     stretchy-vector-instance(#"a", #"b", #"c", #"d", #"e", #"f", #"g", #"h"))
  = stretchy-vector-instance(#"b", #"d", #"f", #"h"));
end test choose-by-4;

define test choose-by-5 (description: "simple-object-vector-vector")
  check-true("", choose-by
    (even?,
     simple-object-vector-instance(1, 2, 3, 4, 5, 6, 7, 8),
     simple-object-vector-instance
       (#"a", #"b", #"c", #"d", #"e", #"f", #"g", #"h"))
  = simple-object-vector-instance(#"b", #"d", #"f", #"h"));
end test choose-by-5;

define test choose-by-6 (description: "string")
  check-true("", choose-by
    (method (c)
       c >= 'a' & c <= 'z'
     end method,
     "Abandon Hope All Ye Who Enter Here!",
     "#This i$@s t=}he*{ ?>ri+%ght _+ans")
  = "This is the right ans");
end test choose-by-6;

define test choose-by-7 (description: "a few mixed cases")
  check-true("", choose-by
    (even?,
     deque-instance(1, 2, 3, 4, 5, 6, 7, 8),
     stretchy-vector-instance(#"a", #"b", #"c", #"d", #"e", #"f", #"g", #"h"))
  = stretchy-vector-instance(#"b", #"d", #"f", #"h"));
  check-true("", choose-by
      (method (c)
         c >= 'a' & c <= 'z'
       end method,
       #('A', 'b', 'a', 'n', 'd', 'o', 'n', ' ', 'H', 'o', 'p', 'e', ' ', 'A', 'l', 'l', ' ', 'Y', 'e', ' ', 'W', 'h', 'o', ' ', 'E', 'n', 't', 'e', 'r', ' ', 'H', 'e', 'r', 'e', '!'),
       "#This i$@s t=}he*{ ?>ri+%ght _+ans")
    = "This is the right ans");
  check-true("", choose-by(even?, range(from: 1), #(#"a", #"b", #"c", #"d", #"e", #"f"))
    = #(#"b", #"d", #"f"));
end test choose-by-7;

define test choose-by-8 (description: "returns new sequence")
  let x = "abcdefgh";
  let result = choose-by(odd?, #(1, 1, 1, 1, 1, 1, 1, 1), x);
 check-true("",  ~(x == result) & x = result);
end test choose-by-8;

// these test intersection

define test intersection-type (description: "")
  check-true("", instance?(intersection, <generic-function>));
end test intersection-type;

define test intersection-0 (description: "list")
  begin
    let result
      = intersection
          (#(#"john", #"paul", #"george", #"ringo"),
           #(#"richard", #"george", #"edward", #"charles", #"john"));
    check-true("", member?(#"john", result) & member?(#"george", result));
  end;
  check-true("", intersection
      (#(#"john", #"paul", #"george", #"ringo"),
       #(#"richard", #"edward", #"charles")).empty?);
end test intersection-0;

define test intersection-1 (description: "empty-list")
  check-true("", intersection(#(), #()).empty?);
end test intersection-1;

define test intersection-2
  (description: "always terminates, even with unbounded ranges")
  check-true("", intersection(range(from: 0, by: 2), range(from: 1, by: 2)).empty?);
  check-true("", intersection(range(from: 8, by: 2), range(from: 9, by: 3))
    = range(from: 12, by: 6));
end test intersection-2;

define test intersection-2a (description: "with test")
  let result
    = intersection
        (as(<list>, range(from: 0, by: 2, size: 5)),
         as(<list>, range(from: 1, by: 2, size: 5)),
         test: method (x, y)
                 x + 1 = y
               end method);
  check-true("", every?(rcurry(member?, result), #(0, 2, 4, 6, 8)) & result.size = 5);
end test intersection-2a;

// See design note #18
//

define test intersection-2b (description: "same, but with ranges not lists")
  let result
    = intersection
        (range(from: 0, by: 2, size: 5),
         range(from: 1, by: 2, size: 5),
         test: method (x, y)
                 x + 1 = y
               end method);
  check-true("", every?(rcurry(member?, result), #(0, 2, 4, 6, 8)) & result.size = 5);
end test intersection-2b;

define test intersection-2c (description: "range, test: =")
  check-true("", intersection
    (range(from: 0, by: 2, size: 5), range(from: 6, by: 2, size: 5), test: \=)
  = range(from: 6, by: 2, size: 2));
end test intersection-2c;

define test intersection-2c (description: "semi-infinite ranges, test: id?")
  check-true("", intersection(range(from: 0, by: 2), range(by: 2, to: 10), test: \==)
  = range(from: 0, by: 2, size: 6));
end test intersection-2c;

define test intersection-3 (description: "deque")
  check-true("", intersection(deque-instance(1, 2, 3, 4, 5), deque-instance(4, 6, 8, 10, 12))
  = deque-instance(4));
end test intersection-3;

define test intersection-4 (description: "stretchy-vector")
  check-true("", intersection
    (stretchy-vector-instance(1, 2, 3, 4, 5),
     stretchy-vector-instance(4, 6, 8, 10, 12))
  = stretchy-vector-instance(4));
end test intersection-4;

define test intersection-5 (description: "simple-object-vector")
  check-true("", intersection
    (simple-object-vector-instance(1, 2, 3, 4, 5),
     simple-object-vector-instance(4, 6, 8, 10, 12))
  = simple-object-vector-instance(4));
end test intersection-5;

define test intersection-6 (description: "string")
  let result
    = intersection
        ("When, in the course of human events",
         "Abandon hope all ye who enter here!");
  check-true("", every?(rcurry(member?, "aehnort "), result));
  check-true("", every?(rcurry(member?, result), "aehnort "));
end test intersection-6;

