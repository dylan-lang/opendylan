Module:    apple-dylan-test-suite
Filename:  test-collection3.dylan
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

define test member?-8 (description: "test defaults to id?")
  check-true("", ~member?(#[2], #[#[1], #[2], #[3]]));
end test member?-8;

define test member?-9 (description: "with other tests")
  check-true("", member?(#[2], #[#[1], #[2], #[3]], test: \=));
  check-true("", ~member?(0, range(from: 0, up-to: 6), test: \>));
  check-true("", member?(1, range(from: 0, up-to: 6), test: \>));
  check-true("", member?(1, #[1, 2, 3], test: \<));
  check-true("", ~member?(#(#(1), #(2), #(3)), #(1)));
  check-true("", member?('a', #[3, 5, 'b', 7, 'a']));
end test member?-9;

define test member?-10 (description: "test may be non-commutative")
  let error = #f;
  check-true("", member?
    (1, #('a', 'b'),
	test: method (x, y)
             	if (instance?(y, <character>))
               	   error := #t;
             	else
               	   #f;
                end if;
              end method));
end test member?-10;

define test find-key-type ()
  check-true("", instance?(find-key, <generic-function>))
end test find-key-type;

define test find-key-0 (description: "list")
  let var = list(1, 2, 3, 4, 4);
  check-equal("", 4, var[find-key(var, curry(\==, 4))] & even?(var[find-key(var, even?)]));
  check-equal("", find-key(#(#()), empty?), 0);
  check-equal("", find-key(#(#(#())), empty?), #f);
end test find-key-0;

define test find-key-1 (description: "empty-list")
  check-equal("", find-key(#(), empty?), #f);
end test find-key-1;

define test find-key-2 (description: "range")
    let var = range(from: 1, up-to: 6, by: 3);
    check-true("", even?(var[find-key(var, even?)]));
 
  check-equal("", find-key(range(from: 1, up-to: 6, by: 2), even?), #f);
end test find-key-2;

define test find-key-3 (description: "deque")
    let var = deque-instance(1, 2, 3, 4, 5);
  check-true("", even?(var[find-key(var, even?)]));
  check-equal("", find-key(deque-instance(1, 3, 5), even?), #f);
end test find-key-3;

define test find-key-4 (description: "table")
    let var = table-instance(#(1, 2), #(3, 4), #(5, 6));
  check-true("",  even?(var[find-key(var, even?)]));
  check-equal("", find-key(table-instance(#(1, 1), #(3, 3), #(5, 5)), even?), #f);
end test find-key-4;

define test find-key-5 (description: "stretchy-vector")
    let var = stretchy-vector-instance(1, 2, 3, 4, 5);
  check-true("",  even?(var[find-key(var, even?)]));

  check-equal("", find-key(stretchy-vector-instance(1, 3, 5), even?), #f);
end test find-key-5;

define test find-key-6 (description: "simple-object-vector")
 
    let var = simple-object-vector-instance(1, 2, 3, 4, 5);
  check-true("",  even?(var[find-key(var, even?)]));
  check-equal("", find-key(simple-object-vector-instance(1, 3, 5), even?), #f);
end test find-key-6;

define test find-key-7 (description: "string")

    let var = "abcdefgh";
    check-equal("", 'c', var[find-key(var, curry(\=, 'c'))]);
  check-equal("", find-key("abdefgh", curry(\=, 'c')), #f);
end test find-key-7;

define test find-key-8 (description: "skip arg")
  check-equal("", 5, find-key(list(1, 2, 3, 4, 5, 6), even?.curry, skip: 2));
end test find-key-8;

define test find-key-9 (description: "failure")
  check-equal("", #f, find-key(list(1, 2, 3, 4, 5, 6), curry(\<, 9)));
  check-equal("", "Can't find the key",
     find-key
        (list(1, 2, 3, 4, 5, 6),
         even?.curry,
         skip: 3,
         failure: "Can't find the key"));
end test find-key-9;

define test replace-elements!-type ()
  check-true("", instance?(replace-elements!, <generic-function>));
end test replace-elements!-type;

define test replace-elements!-0 (description: "list")
  check-equal("", replace-elements!
    (#(1, 2, 3, 4, 5),
     even?,
     method (num)
       num + 1
     end method),
   #(1, 3, 3, 5, 5));
end test replace-elements!-0;

define test replace-elements!-1 (description: "empty-list")
  check-equal("", replace-elements!
    (#(),
     even?,
     method (num)
       num + 1
     end method),
   #());
end test replace-elements!-1;

define test replace-elements!-2 (description: "deque")
  check-equal("", replace-elements!
    (deque-instance(1, 2, 3, 4, 5),
     even?,
     method (num)
       num + 1
     end method),
   deque-instance(1, 3, 3, 5, 5));
end test replace-elements!-2;

define test replace-elements!-3 (description: "table")
  check-equal("", replace-elements!
    (table-instance(#(1, 2), #(3, 4), #(5, 6)),
     even?,
     method (num)
       num + 1
     end method),
   table-instance(#(1, 3), #(3, 5), #(5, 7)));
end test replace-elements!-3;

define test replace-elements!-4 (description: "stretchy-vector")
  check-equal("", replace-elements!
    (stretchy-vector-instance(1, 2, 3, 4, 5),
     even?,
     method (num)
       num + 1
     end method),
   stretchy-vector-instance(1, 3, 3, 5, 5));
end test replace-elements!-4;

define test replace-elements!-5 (description: "simple-object-vector")
  check-equal("", replace-elements!
    (simple-object-vector-instance(1, 2, 3, 4, 5),
     even?,
     method (num)
       num + 1
     end method),
   simple-object-vector-instance(1, 3, 3, 5, 5));
end test replace-elements!-5;

define test replace-elements!-6 (description: "string")
  check-equal("", replace-elements!
    ("abandon hope!",
     method (c)
       c = 'a'
     end method,
     method (c)
       'z'
     end method),
   "zbzndon hope!");
end test replace-elements!-6;

define test replace-elements!-7 (description: "with count arg")
  check-equal("", replace-elements!
    (#(1, 2, 3, 4, 5, 6),
     odd?,
     method (n)
       n + n
     end method,
     count: 2),
   #(2, 2, 6, 4, 5, 6));
end test replace-elements!-7;

define test fill!-0 (description: "list")
  check-equal("", fill!(#(1, 2, 3, 4), 3), #(3, 3, 3, 3));
end test fill!-0;

define test fill!-1 (description: "empty-list")
  check-equal("", fill!(#(), 4), #());
end test fill!-1;

define test fill!-2 (description: "deque")
  check-equal("", fill!(deque-instance(#"a", #"b", #"c", #"d"), 4), deque-instance(4, 4, 4, 4));
end test fill!-2;

define test fill!-3 (description: "table")
  check-equal("", fill!(table-instance(#(#"a", #"b"), #(#"c", #"d")), 4), table-instance(#(#"a", 4), #(#"c", 4)));
end test fill!-3;

define test fill!-4 (description: "stretchy-vector")
  check-equal("", fill!(stretchy-vector-instance(#"a", #"b", #"c", #"d"), 4), stretchy-vector-instance(4, 4, 4, 4));
end test fill!-4;

define test fill!-5 (description: "simple-object-vector")
  check-equal("", fill!(simple-object-vector-instance(#"a", #"b", #"c", #"d"), 4), simple-object-vector-instance(4, 4, 4, 4));
end test fill!-5;

define test fill!-6 (description: "string")
  check-equal("", fill!("abcdefgh", 'x'), "xxxxxxxx");
  check-equal("", fill!("", 'x'), "");
end test fill!-6;

define test fill!-7 (description: "with start: arg")
  check-equal("", fill!("abcdefgh", 'x', start: 3), "abcxxxxx");
end test fill!-7;

define test fill!-8 (description: "with end: arg")
    let t = "abcdefgh";
    check-equal("", fill!(t, 'x', end: 2), "xxcdefgh");
end test fill!-8;

define test fill!-9 (description: "with start: and end: args")
  check-equal("", fill!(#[#"a", #"b", #"c", #"d"], 1, start: 1, end: 3), #[#"a", 1, 1, #"d"]);
  check-equal("", fill!("abcdefgh", 'x', start: 3, end: 5), "abcxxfgh");
  check-equal("", fill!(#(), #"x", start: 0, end: 0), #());
end test fill!-9;

// KJP: end: 3 -> 0

define test fill!-10 (description: "alters mutable-collection")
  let s = "abcdefgh";
  check-equal("", fill!(s, 'x'), "xxxxxxxx" );
  check-equal("", fill!(s, 'x'), s);
end test fill!-10;

define test element-type ()
  check-true("", instance?(element, <generic-function>));
end test element-type;

define test element-0 (description: "simple cases")
  check-equal("", deque-instance(1, 2, 3, 4)[2], 3);
  check-equal("", stretchy-vector-instance(1, 2, 3, 4)[2], 3);
  check-equal("", simple-object-vector-instance(1, 2, 3, 4)[2], 3);
  check-equal("", table-instance(#(1, #"a"), #(2, #"b"), #(3, #"c"))[2], #"b");
  check-equal("", "Now is the time"[5], 's');
  check-equal("", #(99, 98, 97, 96, 95)[4], 95);
end test element;

define test element-1 (description: "with default")
  check-true("", empty?(element(deque-instance(1, 2, 3, 4), 7, default: deque-instance())));
  check-equal("", element(stretchy-vector-instance(1, 2, 3, 4), 6, default: #()), #());
  check-equal("", element(simple-object-vector-instance(1, 2, 3, 4), 8, default: #t), #t);
  check-equal("", element
      (table-instance(#(1, #"a"), #(2, #"b"), #(3, #"c")), 87, default: #"d"), #"d");
  check-equal("", element("Now is the time", 100, default: "no"), "no");
  check-equal("", element(#(99, 98, 97, 96, 95), 5, default: #f), #f);
 
      let a = "foobar";
    check-equal("", apply(aref, list(a, 3)), a[3]);
    check-equal("", apply(aref, list(a, 3)), 'b');
end test element-1;

// element-setter

define test element-setter-type ()
  check-true("", instance?(element-setter, <generic-function>));
end test element-setter-type;

define test element-setter-0 ()
  let t = #[7, 8, 9];
  element-setter(5, t, 1);
  check-equal("", t, #[7, 5, 9]);
end test element-setter-0;

// design note #10: Element-setter signals an error if it can't
// successfully set the element of a sequence.

// These do not exhaustively test all possible collections

define test element-setter-list1
  (description: "list: index too low")
  check-condition("", <error>, list(1, 2, 3)[-1] := 0);
end test element-setter-list1;

define test element-setter-list2
  (signal: <error>, description: "list: index too high")
  check-condition("", <error>, list(1, 2, 3)[5] := 5);
end test element-setter-list2;

define test element-setter-list3
  (signal: <error>, description: "improper list")
  check-condition("", <error>, pair(1, 2)[1] := #"a");
end test element-setter-list3;

define test element-setter-vector1
  (signal: <error>, description: "vector: index too low")
  check-condition("", <error>, vector(1, 2, 3)[-1] := 0);
end test element-setter-vector1;

define test element-setter-vector2
  (signal: <error>, description: "vector: index too high")
  check-condition("", <error>, vector(1, 2, 3)[5] := 5);
end test element-setter-vector2;

define test element-setter-string1
  (signal: <error>, description: "string: index too low")
  let s = "foo";
  check-condition("", <error>, s[-1] := 'a');
end test element-setter-string1;

define test element-setter-string2
  (signal: <error>, description: "string: index too high")
  let s = "foo";
  check-condition("", <error>, s[5] := 'a');
end test element-setter-string2;

define test element-setter-string3
  (signal: <error>, description: "string: not a char")
  let s = "foo";
  check-condition("", <error>, s[1] := #"a");
end test element-setter-string3;

// element, page 124

define test element1-0 (description: "list")
  let c = #(1, 2, 3, 4);
  check-equal("", c[0], 1);
  check-equal("", element(c, 99, default: #t), #t);
end test element1-0;

define test element1-1 (description: "range")
  let c = range(from: 1, up-to: 6);
  check-equal("", c[0], 1);
  check-equal("", element(c, 99, default: #t), #t);
end test element1-1;

define test element1-2 (description: "deque")
  let c = deque-instance(1, 2, 3, 4);
  check-equal("", c[0], 1);
  check-equal("", element(c, 99, default: #t), #t);
end test element1-2;

define test element1-3 (description: "table")
  let c = table-instance(#(1, 2), #(3, 4));
  check-equal("", c[3], 4 );
  check-equal("", element(c, 99, default: #t), #t);
end test element1-3;

define test element1-4 (description: "stretchy-vector")
  let c = stretchy-vector-instance(1, 2, 3, 4);
  check-equal("", c[0], 1 );
  check-equal("", element(c, 99, default: #t), #t);
end test element1-4;

define test element1-5 (description: "simple-object-vector")
  let c = simple-object-vector-instance(1, 2, 3, 4);
  check-equal("", c[0], 1);
  check-equal("", element(c, 99, default: #t), #t);
end test element1-5;

define test element1-6 (description: "string")
  let c = "1234";
  check-equal("", c[0], '1' );
  check-equal("", element(c, 99, default: #t), #t);
end test element1-6;

define test key-sequence1-0 ()
  check-true("", instance?(key-sequence(#(1, 2, 3)), <sequence>));
      let t = make(<table>);
      let t1 = #(1, 2, 3, 4, 5);
      map-into
        (t,
         method (item)
           item / t1.size
         end method,
         t1);
      remove-key!(t, 2);
      check-equal("", 4, t.key-sequence.size);
 check-equal("", 4, t.key-sequence.remove-duplicates.size);
    check-true("", every?
        (method (x)
           member?(x, #(0, 1, 3, 4))
         end method,
         t.key-sequence));
end test key-sequence1-0;

define test current-key1-0 ()
  check-true("", every?
    (method (key-collection)
       2
       = current-key
           (key-collection,
            next-state
              (key-collection,
               next-state(key-collection, key-collection.initial-state)))
     end method,
     list(vector(5, 6, 7, 8),
          "abcde",
          simple-object-vector-instance(5, 6, 7, 8),
          stretchy-vector-instance(5, 6, 7, 8))));
end test current-key1-0;

define suite test-collection-suite ()
  test size-type;
  test size-0;
  test size-1;
  test size-2;
  test size-3;
  test size-4;
  test size-5;
  test size-6;
  test size-7;
  test size-setter-type;
  test size-setter-1;
  test size-setter-2;
  test class-for-copy-type;
  test class-for-copy-0; 
  test class-for-copy-1;
  test class-for-copy-3; 
  test empty?-type;
  test empty?-0;
  test empty?-1;
  test empty?-2;
  test empty?-3;
  test empty?-4;
  test empty?-5;
  test empty?-6;
  test empty?-7;
  test do-type;
  test do-0;
  test do-1;
  test do-2;
  test map-type;
  test map-0;
  test map-1;
  test map-2;
  test map-3;
  test map-4;
  test map-5;
  test map-6;
  test map-7;
  test map-8;
  test map-9;
  test map-10;
  test map-11;
  test map-as-type; 
  test map-as-0;
  test map-as-1;
  test map-as-2;
  test map-as-3;
  test map-as-4;
  test map-as-5;
  test map-as-6;
  test map-as-7;
  test map-as-8;
  test map-as-9;
  test map-into-type;
  test map-into-0;
  test map-into-1;
  test map-into-2; 
  test map-into-3;
  test map-into-4;
  test map-into-5;
  test map-into-6;
  test map-into-7;
  test map-into-8;
  test map-into-9;
  test map-into-11;
  test any?-type;
  test any?-0;
  test any?-1;
  test any?-2;
  test any?-3;
  test any?-4;
  test any?-5;
  test any?-6;
  test any?-7;
  test any?-8;
//  test any?-9; commented out by amit - look at issues
//  test any?-10; or test-collection2.dylan
  test any?-11;
  test every?-type;
  test every?-0; 
  test every?-1;
  test every?-2;
  test every?-3;
  test every?-4;
  test every?-5;
  test every?-6;
  test every?-7;
  test every?-8;
  test every?-9;
  test reduce-type;
  test reduce-0;
  test reduce-1;
  test reduce-2;
  test reduce-3;
  test reduce-4;
  test reduce-5;
  test reduce-6;
  test reduce-7;
  test reduce1-type;
  test reduce1-0;
  test reduce1-1;
  test reduce1-2;
  test reduce1-3;
  test reduce1-4;
  test reduce1-5;
  test reduce1-6;
  test reduce1-7;
  test reduce1-8;
  test member?-type;
  test member?-0;
  test member?-1;
  test member?-2;
  test member?-3;
  test member?-4;
  test member?-5;
  test member?-6;
  test member?-7;
  test member?-8;
  test member?-9;
  test member?-10;
  test find-key-type;
  test find-key-0;
  test find-key-1;
  test find-key-2;
  test find-key-3;
  test find-key-4;
  test find-key-5;
  test find-key-6;
  test find-key-7;
  test find-key-8;
  test find-key-9;
  test replace-elements!-type;
  test replace-elements!-0;
  test replace-elements!-1;
  test replace-elements!-2;
  test replace-elements!-3;
  test replace-elements!-4;
  test replace-elements!-5;
  test replace-elements!-6;
  test replace-elements!-7;
  test fill!-0;
  test fill!-1;
  test fill!-2;
  test fill!-3;
  test fill!-4;
  test fill!-5;
  test fill!-6;
  test fill!-7;
  test fill!-8;
  test fill!-9;
  test fill!-10; 
  test element-type;
  test element-0;
  test element-1;
  test element-setter-type;
  test element-setter-0;
  test element-setter-list1;
  test element-setter-list2;
  test element-setter-list3;
  test element-setter-vector1;
  test element-setter-vector2;
  test element-setter-string1;
  test element-setter-string2;
  test element-setter-string3;
  test element1-0;
  test element1-1;
  test element1-2;
  test element1-3;
  test element1-4;
  test element1-5;
  test element1-6;
  test key-sequence1-0;
  test current-key1-0;
end suite;
