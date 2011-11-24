Module:    apple-dylan-test-suite
Filename:  test-sequence4.dylan
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

define test replace-subsequence!-type (description: "")
  check-true("", instance?(replace-subsequence!, <generic-function>));
end test replace-subsequence!-type;

// tests replace-subsequence!

define test replace-subsequence!-0 (description: "list")
  check-true("", replace-subsequence!(#(1, 2, 3, 4), #(99, 100, 101, 102))
  = #(99, 100, 101, 102));
end test replace-subsequence!-0;

define test replace-subsequence!-1 (description: "empty-list")
  check-true("", replace-subsequence!(#(1, 2, 3), #()) = #(1, 2, 3));
end test replace-subsequence!-1;

define test replace-subsequence!-2 (description: "deque")
  check-true("", replace-subsequence!
    (deque-instance(1, 3, 5, 7, 9), deque-instance(2, 4, 6, 8))
  = deque-instance(2, 4, 6, 8, 9));
end test replace-subsequence!-2;

define test replace-subsequence!-4 (description: "simple-object-vector")
  check-true("", replace-subsequence!
    (simple-object-vector-instance(1, 3, 5, 7, 9),
     simple-object-vector-instance(2, 4, 6, 8))
  = simple-object-vector-instance(2, 4, 6, 8, 9));
end test replace-subsequence!-4;

define test replace-subsequence!-5 (description: "string")
  check-true("", replace-subsequence!("I hate oatmeal", "like") = "likete oatmeal");
end test replace-subsequence!-5;

define test replace-subsequence!-6 (description: "start: arg")
  check-true("", replace-subsequence!("I hate oatmeal", "like", start: 2) = "I like oatmeal");
end test replace-subsequence!-6;

define test replace-subsequence!-7 (description: "overwrites seq")
  let phrase = "I hate oatmeal";
  let result = replace-subsequence!(phrase, "like", start: 2);
  check-true("", phrase == result);
end test replace-subsequence!-7;

define test replace-subsequence!-8 (description: "mixed cases")
  check-true("", replace-subsequence!(#(1, 2, 3), "now") = #('n', 'o', 'w'));
  check-true("", replace-subsequence!(#(#"a", #"b", #"c", #"d"), range(from: 0, up-to: 4))
    = #(0, 1, 2, 3));
end test replace-subsequence!-8;

// Design note #15 extends the definition of replace-subsequence!

define test replace-subsequence!-9 (description: "end insertion at beginning")
  let abcde = list(#"a", #"b", #"c", #"d", #"e");
  abcde := replace-subsequence!(abcde, #(#"x", #"y", #"z"), end: 1);
  check-true("", abcde = #(#"x", #"y", #"z", #"b", #"c", #"d", #"e"));
end test replace-subsequence!-9;

define test replace-subsequence!-10 (description: "end")
  let abcde = list(#"a", #"b", #"c", #"d", #"e");
  abcde := replace-subsequence!(abcde, #(#"x", #"y", #"z"), start: 2);
  check-true("", abcde = #(#"a", #"b", #"x", #"y", #"z"));
end test replace-subsequence!-10;

define test replace-subsequence!-11
  (description: "insert into middle with start and end")
  let abcde = list(#"a", #"b", #"c", #"d", #"e");
  abcde := replace-subsequence!(abcde, #(#"x", #"y", #"z"), start: 1, end: 3);
  check-true("", abcde = #(#"a", #"x", #"y", #"z", #"e"));
end test replace-subsequence!-11;

define test reverse-type (description: "")
  check-true("", instance?(reverse, <generic-function>));
end test reverse-type;

define test reverse-0 (description: "list")
  check-true("", reverse(#(1, 2, 3, 4)) = #(4, 3, 2, 1));
end test reverse-0;

define test reverse-1 (description: "empty list")
  check-true("", reverse(#()) = #());
end test reverse-1;

define test reverse-2 (description: "range")
  check-true("", range(from: 0, up-to: 6).reverse = range(from: 5, up-to: -1, by: -1));
end test reverse-2;

define test reverse-3 (description: "deque")
  check-true("", deque-instance(1, 2, 3, 4).reverse = deque-instance(4, 3, 2, 1));
end test reverse-3;

define test reverse-4 (description: "stretchy-vector")
  check-true("", stretchy-vector-instance(1, 2, 3, 4).reverse
  = stretchy-vector-instance(4, 3, 2, 1));
end test reverse-4;

define test reverse-5 (description: "simple-object-vector")
  check-true("", simple-object-vector-instance(1, 2, 3, 4).reverse
  = simple-object-vector-instance(4, 3, 2, 1));
end test reverse-5;

define test reverse-6 (description: "string")
  check-true("", reverse("milk") = "klim");
end test reverse-6;

define test reverse-7 (description: "returns new seq")
  let s = #(1, 2, 3);
  let result = s.reverse;
  check-true("", ~(s == result));
end test reverse-7;

define test reverse!-type (description: "")
  check-true("", instance?(reverse!, <generic-function>));
end test reverse!-type;

define test reverse!-0 (description: "list")
  check-true("", reverse!(#(1, 2, 3, 4)) = #(4, 3, 2, 1));
end test reverse!-0;

define test reverse!-1 (description: "empty list")
  check-true("", reverse!(#()) = #());
end test reverse!-1;

define test reverse!-2 (description: "range")
  check-true("", reverse!(range(from: 0, up-to: 6)) = range(from: 5, up-to: -1, by: -1));
end test reverse!-2;

define test reverse!-3 (description: "deque")
  check-true("", reverse!(deque-instance(1, 2, 3, 4)) = deque-instance(4, 3, 2, 1));
end test reverse!-3;

define test reverse!-4 (description: "stretchy-vector")
  check-true("", reverse!(stretchy-vector-instance(1, 2, 3, 4))
  = stretchy-vector-instance(4, 3, 2, 1));
end test reverse!-4;

define test reverse!-5 (description: "simple-object-vector")
  check-true("", reverse!(simple-object-vector-instance(1, 2, 3, 4))
  = simple-object-vector-instance(4, 3, 2, 1));
end test reverse!-5;

define test reverse!-6 (description: "string")
  check-true("", reverse("milk") = "klim");
end test reverse!-6;

define test sort-type (description: "")
  check-true("", instance?(sort, <generic-function>));
end test sort-type;

define test sort-0 (description: "list")
  check-true("", sort(#(1, 9, 2, 8, 3, 7, 4, 6, 5)) = #(1, 2, 3, 4, 5, 6, 7, 8, 9));
end test sort-0;

define test sort-1 (description: "empty-list")
  check-true("", sort(#()) = #());
end test sort-1;

define test sort-2 (description: "range")
  check-true("", range(from: 9, up-to: 0, by: -1).sort = #(1, 2, 3, 4, 5, 6, 7, 8, 9));
end test sort-2;

define test sort-3 (description: "deque")
  check-true("", deque-instance(1, 9, 2, 8, 3, 7, 4, 6, 5).sort = #(1, 2, 3, 4, 5, 6, 7, 8, 9));
end test sort-3;

define test sort-4 (description: "stretchy-vector")
  check-true("", stretchy-vector-instance(1, 9, 2, 8, 3, 7, 4, 6, 5).sort
  = stretchy-vector-instance(1, 2, 3, 4, 5, 6, 7, 8, 9));
end test sort-4;

define test sort-5 (description: "simple-object-vector")
  check-true("", simple-object-vector-instance(1, 9, 2, 8, 3, 7, 4, 6, 5).sort
  = simple-object-vector-instance(1, 2, 3, 4, 5, 6, 7, 8, 9));
end test sort-5;

define test sort-6 (description: "test")
  check-true("", sort(#(1, 9, 2, 8, 3, 7, 4, 6, 5), test: \>) = #(9, 8, 7, 6, 5, 4, 3, 2, 1));
  check-true("", sort(#(#"a", #"b", #"c", #"d", #"c", #"b", #"a"),
         test: method (a, b)
                 if (a = #"b")
                   #t
                 else
                   #f
                 end if
               end method,
         stable: #t)
    = #(#"b", #"b", #"a", #"c", #"d", #"c", #"a"));
end test sort-6;

define test sort-7 (description: "stability")
  check-true("", sort(#('z', 'Z', 'Y', 'y', 'x', 'X'),
       test: method (a, b)
               a.as-uppercase < b.as-uppercase
             end method)
  = #('x', 'X', 'Y', 'y', 'z', 'Z'));
  check-true("", sort(#[#("Tokens", "The Lion Sleeps Tonight"), #("Carpenters", "Close to You"), #("Rolling Stones", "Brown Sugar"), #("Beach Boys", "I Get Around"), #("Mozart", "Eine Kleine Nachtmusik", #(#"k", 525)), #("Beatles", "I Want to Hold Your Hand")],
         test: method (x, y)
                 binary<(x.first, y.first)
               end method)
    = #[#("Beach Boys", "I Get Around"), #("Beatles", "I Want to Hold Your Hand"), #("Carpenters", "Close to You"), #("Mozart", "Eine Kleine Nachtmusik", #(#"k", 525)), #("Rolling Stones", "Brown Sugar"), #("Tokens", "The Lion Sleeps Tonight")]);
end test sort-7;

define test sort-8 (description: "new seq")
  let s = #(1, 2, 3, 4, 5, 6, 7, 8, 9);
  let result = s.sort;
  check-true("", s = result & ~(s == result));
end test sort-8;

define test sort!-type (description: "")
  check-true("", instance?(sort!, <generic-function>));
end test sort!-type;

define test sort!-0 (description: "list")
  check-true("", sort!(#(1, 9, 2, 8, 3, 7, 4, 6, 5)) = #(1, 2, 3, 4, 5, 6, 7, 8, 9));
end test sort!-0;

define test sort!-1 (description: "empty-list")
  check-true("", sort!(#()) = #());
end test sort!-1;

define test sort!-2 (description: "range")
  check-true("", sort!(range(from: 9, up-to: 0, by: -1)) = #(1, 2, 3, 4, 5, 6, 7, 8, 9));
end test sort!-2;

define test sort!-3 (description: "deque")
  check-true("", sort!(deque-instance(1, 9, 2, 8, 3, 7, 4, 6, 5))
  = #(1, 2, 3, 4, 5, 6, 7, 8, 9));
end test sort!-3;

define test sort!-4 (description: "stretchy-vector")
  check-true("", sort!(stretchy-vector-instance(1, 9, 2, 8, 3, 7, 4, 6, 5))
  = stretchy-vector-instance(1, 2, 3, 4, 5, 6, 7, 8, 9));
end test sort!-4;

define test sort!-5 (description: "simple-object-vector")
  check-true("", sort!(simple-object-vector-instance(1, 9, 2, 8, 3, 7, 4, 6, 5))
  = simple-object-vector-instance(1, 2, 3, 4, 5, 6, 7, 8, 9));
end test sort!-5;

define test sort!-6 (description: "test")
  check-true("", sort!(#(1, 9, 2, 8, 3, 7, 4, 6, 5), test: \>) = #(9, 8, 7, 6, 5, 4, 3, 2, 1));
  check-true("", sort!(#(#"a", #"b", #"c", #"d", #"c", #"b", #"a"),
          test: method (a, b)
                  if (a = #"b")
                    #t
                  else
                    #f
                  end if
                end method,
          stable: #t)
    = #(#"b", #"b", #"a", #"c", #"d", #"c", #"a"));
end test sort!-6;

define test sort!-7 (description: "stability")
  check-true("", sort!(#('z', 'Z', 'Y', 'y', 'x', 'X'),
        test: method (a, b)
                a.as-uppercase < b.as-uppercase
              end method)
  = #('x', 'X', 'Y', 'y', 'z', 'Z'));
  check-true("", sort!(#[#("Tokens", "The Lion Sleeps Tonight"), #("Carpenters", "Close to You"), #("Rolling Stones", "Brown Sugar"), #("Beach Boys", "I Get Around"), #("Mozart", "Eine Kleine Nachtmusik", #(#"k", 525)), #("Beatles", "I Want to Hold Your Hand")],
          test: method (x, y)
                  binary<(x.first, y.first)
                end method)
    = #[#("Beach Boys", "I Get Around"), #("Beatles", "I Want to Hold Your Hand"), #("Carpenters", "Close to You"), #("Mozart", "Eine Kleine Nachtmusik", #(#"k", 525)), #("Rolling Stones", "Brown Sugar"), #("Tokens", "The Lion Sleeps Tonight")]);
end test sort!-7;

define test first-type (description: "")
  check-true("", instance?(first, <generic-function>));
end test first-type;

define test first-0 (description: "simple cases")
  check-true("", first(#(1, 2, 3, 4, 5)) = 1 & first(#(), default: #"no") = #"no");
end test first-0;

