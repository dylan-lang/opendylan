Module:    apple-dylan-test-suite
Filename:  test-sequence5.dylan
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

define test first-1 (description: "dotted pair")
  check-true("", pair(#"a", #"b").first = #"a");
end test first-1;

define test first-2 (description: "range")
  check-true("", range(from: 0, below: 9).first = 0);
  check-true("", first(range(from: 0, below: 0), default: #"no") = #"no");
end test first-2;

define test first-3 (description: "deque")
  check-true("", deque-instance(0, 1, 2, 3, 4).first = 0);
  check-true("", first(deque-instance(), default: #"no") = #"no");
end test first-3;

define test first-4 (description: "stretchy-vector")
  check-true("", stretchy-vector-instance(0, 1, 2, 3, 4).first = 0);
  check-true("", first(stretchy-vector-instance(), default: #"no") = #"no");
end test first-4;

define test first-5 (description: "simple-object-vector")
  check-true("", vector(0, 1, 2, 3, 4).first = 0);
  check-true("", first(vector(), default: #"no") = #"no");
end test first-5;

define test first-6 (description: "string")
  check-true("", first("abcde") = 'a' & first("", default: #"no") = #"no");
end test first-6;

define test second-type (description: "")
  check-true("", instance?(second, <generic-function>));
end test second-type;

define test second-0 (description: "simple cases")
  check-true("", second(#(1, 2, 3, 4, 5)) = 2 & second(#(), default: #"no") = #"no");
end test second-0;

define test second-1 (description: "dotted pair")
  check-true("", pair(#"a", pair(#"b", #"c")).second = #"b");
end test second-1;

define test second-2 (description: "range")
  check-true("", range(from: 0, below: 9).second = 1);
  check-true("", second(range(from: 0, below: 0), default: #"no") = #"no");
end test second-2;

define test second-3 (description: "deque")
  check-true("", deque-instance(0, 1, 2, 3, 4).second = 1);
  check-true("", second(deque-instance(), default: #"no") = #"no");
end test second-3;

define test second-4 (description: "stretchy-vector")
  check-true("", stretchy-vector-instance(0, 1, 2, 3, 4).second = 1);
  check-true("", second(stretchy-vector-instance(), default: #"no") = #"no");
end test second-4;

define test second-5 (description: "simple-object-vector")
  check-true("", vector(0, 1, 2, 3, 4).second = 1);
  check-true("", second(vector(), default: #"no") = #"no");
end test second-5;

define test second-6 (description: "string")
  check-true("", second("abcde") = 'b' & second("", default: #"no") = #"no");
end test second-6;

define test third-type (description: "")
  check-true("", instance?(third, <generic-function>));
end test third-type;

define test third-0 (description: "simple cases")
  check-true("", third(#(1, 2, 3, 4, 5)) = 3 & third(#(), default: #"no") = #"no");
end test third-0;

define test third-1 (description: "dotted pair")
  check-true("", pair(#"a", pair(#"b", pair(#"c", #"d"))).third = #"c");
end test third-1;

define test third-2 (description: "range")
  check-true("", range(from: 0, below: 9).third = 2);
  check-true("", third(range(from: 0, below: 0), default: #"no") = #"no");
end test third-2;

define test third-3 (description: "deque")
  check-true("", deque-instance(0, 1, 2, 3, 4).third = 2);
  check-true("", third(deque-instance(), default: #"no") = #"no");
end test third-3;

define test third-4 (description: "stretchy-vector")
  check-true("", stretchy-vector-instance(0, 1, 2, 3, 4).third = 2);
  check-true("", third(stretchy-vector-instance(), default: #"no") = #"no");
end test third-4;

define test third-5 (description: "simple-object-vector")
  check-true("", vector(0, 1, 2, 3, 4).third = 2);
  check-true("", third(vector(), default: #"no") = #"no");
end test third-5;

define test third-6 (description: "string")
  check-true("", third("abcde") = 'b' & third("", default: #"no") = #"no");
end test third-6;

// first-setter

define test first-setter-type (description: "")
  check-true("", instance?(first-setter, <generic-function>));
end test first-setter-type;

define test first-setter-0 (description: "simple cases")
  check-true("", begin
    let t = #(3, 4, 5);
    first-setter(#(1, 2), t);
    t = #(#(1, 2), 4, 5)
  end);
  check-true("", begin
      let t = #(1, 2, 3, 4, 5, 6, 7, 8, 9);
      first-setter(0, t);
      t
    end
    = #(0, 2, 3, 4, 5, 6, 7, 8, 9));
end test first-setter-0;

// second-setter

define test second-setter-type (description: "")
  check-true("", instance?(second-setter, <generic-function>));
end test second-setter-type;

define test second-setter-0 (description: "simple cases")
  check-true("", begin
    let t = #(3, 4, 5);
    second-setter(#(1, 2), t);
    t = #(3, #(1, 2), 5)
  end);
  check-true("", begin
      let t = #(1, 2, 3, 4, 5, 6, 7, 8, 9);
      second-setter(0, t);
      t
    end
    = #(1, 0, 3, 4, 5, 6, 7, 8, 9));
end test second-setter-0;

// third-setter

define test third-setter-type (description: "")
  check-true("", instance?(third-setter, <generic-function>));
end test third-setter-type;

define test third-setter-0 (description: "simple cases")
  check-true("", begin
    let t = #(3, 4, 5);
    third-setter(#(1, 2), t);
    t = #(3, 4, #(1, 2))
  end);
  check-true("", begin
      let t = #(1, 2, 3, 4, 5, 6, 7, 8, 9);
      third-setter(0, t);
      t
    end
    = #(1, 2, 0, 4, 5, 6, 7, 8, 9));
end test third-setter-0;

define test last-type (description: "")
  check-true("", instance?(last, <generic-function>));
end test last-type;

define test last-0 (description: "list")
  check-true("", last(#(1, 2, 3, 4, 5)) = 5 & last(#(), default: #"no") = #"no");
end test last-0;

define test last-1 (description: "dotted pair")
  check-true("", pair(#"a", pair(#"b", pair(#"c", #"d"))).last = #"c");
end test last-1;

define test last-2 (description: "range")
  check-true("", range(from: 0, below: 9).last = 8);
  check-true("", last(range(from: 0, below: 0), default: #"no") = #"no");
end test last-2;

define test last-3 (description: "deque")
  check-true("", deque-instance(0, 1, 2, 3, 4).last = 4);
  check-true("", last(deque-instance(), default: #"no") = #"no");
end test last-3;

define test last-4 (description: "stretchy-vector")
  check-true("", stretchy-vector-instance(0, 1, 2, 3, 4).last = 4);
  check-true("", last(stretchy-vector-instance(), default: #"no") = #"no");
end test last-4;

define test last-5 (description: "simple-object-vector")
  check-true("", vector(0, 1, 2, 3, 4).last = 4);
  check-true("", last(vector(), default: #"no") = #"no");
end test last-5;

define test last-6 (description: "string")
  check-true("", last("abcde") = 'e' & last("", default: #"no") = #"no");
end test last-6;

// Design note #11: add last-setter
//

define test last-setter-type (description: "")
  check-true("", instance?(last-setter, <generic-function>));
end test last-setter-type;

define test last-setter-0 (description: "simple cases")
  check-true("", begin
    let t = #(3, 4, 5);
    last-setter(#(1, 2), t);
    t = #(3, 4, #(1, 2))
  end);
  check-true("", begin
      let t = #(1, 2, 3, 4, 5, 6, 7, 8, 9);
      last-setter(-1, t);
      t
    end
    = #(1, 2, 3, 4, 5, 6, 7, 8, -1));
end test last-setter-0;

define test last-setter-string (description: "")
  let s = byte-string-instance('g', 'l', 'u', 'e');
  s.last := 'b';
  check-true("", s = "glub");
end test last-setter-string;

define test last-setter-vector (description: "")
  let v = vector(7, 8, 9);
  v.last := -1;
  check-true("", v = #[7, 8, -1]);
end test last-setter-vector;

define test last-setter-stretchy-vector (description: "")
  let v = stretchy-vector-instance(7, 8, 9);
  v.last := -1;
  check-true("", v = stretchy-vector-instance(7, 8, -1));
end test last-setter-stretchy-vector;

define test last-setter-simple-object-vector (description: "")
  let v = vector(7, 8, 9);
  v.last := -1;
  check-true("", v = vector(7, 8, -1));
end test last-setter-simple-object-vector;

define test last-setter-deque (description: "")
  let d = deque-instance(7, 8, 9);
  d.last := -1;
  check-true("", d = deque-instance(7, 8, -1));
end test last-setter-deque;

define test subsequence-position-type (description: "")
  check-true("", instance?(subsequence-position, <generic-function>));
end test subsequence-position-type;

define test subsequence-position-0 (description: "list")
  check-true("", subsequence-position
    (#(#"a", #"b", #"c", #"x", #"y", #"z"), #(#"c", #"x", #"y"))
  = 2);
  check-true("", subsequence-position
      (#(#"a", #"b", #"c", #"x", #"y", #"z"), #(#"d", #"x", #"y"))
    = #f);
end test subsequence-position-0;

define test subsequence-position-1 (description: "empty list")
  check-true("", subsequence-position(#(), #(1, 2, 3, 4)) = #f);
end test subsequence-position-1;

define test subsequence-position-2 (description: "range")
  check-true("", subsequence-position(range(from: 0, below: 6), range(from: 2, below: 5)) = 2);
end test subsequence-position-2;

define test subsequence-position-3 (description: "deque")
  check-true("", subsequence-position(deque-instance(0, 1, 2, 3, 4, 5), deque-instance(3, 4))
  = 3);
end test subsequence-position-3;

define test subsequence-position-4 (description: "stretchy-vector")
  check-true("", subsequence-position
    (stretchy-vector-instance(0, 1, 2, 3, 4, 5),
     stretchy-vector-instance(3, 4))
  = 3);
end test subsequence-position-4;

define test subsequence-position-5 (description: "simple-object-vector")
  check-equal("", 3, subsequence-position(vector(0, 1, 2, 3, 4, 5),
                                          vector(3, 4)));
end test subsequence-position-5;

define test subsequence-position-6 (description: "string")
  check-equal("", 3, subsequence-position("my cat", "cat"));
end test subsequence-position-6;

define test subsequence-position-7 (description: "test:")
  check-equal("", 1, subsequence-position(#(2, 4, 6, 8),
                                          #(2, 3),
                                          test: method (a, b)
                                                  floor/(a, 2) = b
                                                end method));
  // I doubt this is valid since the order of arguments to the test function
  // is undefined, but it will at least show if implementations differ in
  // this respect.  --cgay Aug 2012
  check-equal("", 3, subsequence-position(#(5, 4, 3, 2, 1), #(3), test: \<));
end test subsequence-position-7;

define test subsequence-position-8 (description: "count:")
  check-equal("", 10, subsequence-position("my cat concatenates", "cat", count: 2));
end test subsequence-position-8;

define test subsequence-position-9 (description: "count: and test:")
  check-equal("", 4, subsequence-position(#(5, 4, 3, 2, 1, 0), #(4, 2), test: \<, count: 2));
end test subsequence-position-9;

define suite test-sequence-suite ()
  test add-type;
  test add-1;
  test add-2;
  test add!-type;
  test add!-1;
  test add!-position;
  test add-new-type;
  test add-new-1;
  test add-new-2;
  test add-new-3;
  test add-new-4;
  test add-new!-type;
  test add-new!-1;
  test add-new!-2;
  test add-new!-3;
  test add-new!-4;
  test remove-type;
  test remove-0;
  test remove-1;
  test remove-2;
  test remove-3;
  test remove-4;
  test remove-5;
  test remove-6;
  test remove-7;
  test remove-8;
  test remove-9;
  test remove-10;
  test remove!-type;
  test remove!-0;
  test remove!-1;
  test remove!-2;
  test remove!-3;
  test remove!-4;
  test remove!-5;
  test remove!-6;
  test remove!-7;
  test remove!-8;
  test remove!-9;
  test choose-type;
  test choose-0;
  test choose-1;
  test choose-2;
  test choose-3;
  test choose-4;
  test choose-5;
  test choose-6;
  test choose-7;
  test choose-by-type;
  test choose-by-0;
  test choose-by-1;
  test choose-by-2;
  test choose-by-3;
  test choose-by-4;
  test choose-by-5;
  test choose-by-6;
  test choose-by-7;
  test choose-by-8;
  test intersection-type;
  test intersection-0;
  test intersection-1;
  test intersection-2;
  test intersection-2a;
  test intersection-2b;
  test intersection-2c;
  test intersection-2d;
  test intersection-3;
  test intersection-4;
  test intersection-5;
  test intersection-6;
  test intersection-7;
  test intersection-8;
  test intersection-9;
  test union-type;
  test union-0;
  test union-1;
  test union-2;
  test union-3;
  test union-4;
  test union-5;
  test union-6;
  test union-7;
  test union-8;
  test remove-duplicates-type;
  test remove-duplicates-0;
  test remove-duplicates-1;
  test remove-duplicates-2;
  test remove-duplicates-3;
  test remove-duplicates-4;
  test remove-duplicates-5;
  test remove-duplicates-6;
  test remove-duplicates-7;
  test remove-duplicates-8;
  test remove-duplicates!-type;
  test remove-duplicates!-0;
  test remove-duplicates!-1;
  test remove-duplicates!-2;
  test remove-duplicates!-3;
  test remove-duplicates!-4;
  test remove-duplicates!-5;
  test remove-duplicates!-6;
  test remove-duplicates!-7;
  test copy-sequence-type;
  test copy-sequence-0;
  test copy-sequence-2;
  test copy-sequence-3;
  test copy-sequence-4;
  test copy-sequence-5;
  test copy-sequence-6;
  test copy-sequence-7;
  test copy-sequence-8;
  test concatenate-as-type;
  test concatenate-as-0;
  test concatenate-as-1;
  test concatenate-as-2;
  test concatenate-as-3;
  test concatenate-as-4;
  test concatenate-as-5;
  test concatenate-type;
  test concatenate-0;
  test concatenate-1;
  test concatenate-2;
  test concatenate-3;
  test concatenate-4;
  test concatenate-5;
  test replace-subsequence!-type;
  test replace-subsequence!-0;
  test replace-subsequence!-1;
  test replace-subsequence!-2;
  test replace-subsequence!-4;
  test replace-subsequence!-5;
  test replace-subsequence!-6;
  test replace-subsequence!-7;
  test replace-subsequence!-8;
  test replace-subsequence!-9;
  test replace-subsequence!-10;
  test replace-subsequence!-11;
  test reverse-type;
  test reverse-0;
  test reverse-1;
  test reverse-2;
  test reverse-3;
  test reverse-4;
  test reverse-5;
  test reverse-6;
  test reverse-7;
  test reverse!-type;
  test reverse!-0;
  test reverse!-1;
  test reverse!-2;
  test reverse!-3;
  test reverse!-4;
  test reverse!-5;
  test reverse!-6;
  test sort-type;
  test sort-0;
  test sort-1;
  test sort-2;
  test sort-3;
  test sort-4;
  test sort-5;
  test sort-6;
  test sort-7;
  test sort-8;
  test sort!-type;
  test sort!-0;
  test sort!-1;
  test sort!-2;
  test sort!-3;
  test sort!-4;
  test sort!-5;
  test sort!-6;
  test sort!-7;
  test first-type;
  test first-0;
  test first-1;
  test first-2;
  test first-3;
  test first-4;
  test first-5;
  test first-6;
  test second-type;
  test second-0;
  test second-1;
  test second-2;
  test second-3;
  test second-4;
  test second-5;
  test second-6;
  test third-type;
  test third-0;
  test third-1;
  test third-2;
  test third-3;
  test third-4;
  test third-5;
  test third-6;
  test first-setter-type;
  test first-setter-0;
  test second-setter-type;
  test second-setter-0;
  test third-setter-type;
  test third-setter-0;
  test last-setter-type;
  test last-setter-0;
  test last-type;
  test last-0;
  test last-1;
  test last-2;
  test last-3;
  test last-4;
  test last-5;
  test last-6;
  test last-setter-string;
  test last-setter-vector;
  test last-setter-stretchy-vector;
  test last-setter-simple-object-vector;
  test last-setter-deque;
  test subsequence-position-type;
  test subsequence-position-0;
  test subsequence-position-1;
  test subsequence-position-2;
  test subsequence-position-3;
  test subsequence-position-4;
  test subsequence-position-5;
  test subsequence-position-6;
  test subsequence-position-7;
  test subsequence-position-8;
  test subsequence-position-9;
end suite;
