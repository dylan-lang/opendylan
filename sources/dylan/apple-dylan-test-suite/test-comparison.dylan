Module:    apple-dylan-test-suite
Filename:  test-comparison.dylan
Summary:   Apple Dylan test suite, test-comparison
Version:   29-Oct-93
Copyright: (c) 1993 Apple Computer, Inc.

/*---------------------------------------------
Modified by: Shri Amit(amit)
Date: August 24 1996
Summary: Converted to new testworks protocol
Copyright: (c) 1996 Functional Objects, Inc.
           All rights reserved.
----------------------------------------------*/

define test id?-type ()
  check("", instance?, \==, <function>);
  check("", method(a, b) ~instance?(a, b) end, \==, <generic-function>);
end test id?-type;

// Simple cases
define test id?-0 ()
  check-equal("", #f, #f);
  check-equal("", 3, 3);
  check-equal("", #"foo", #"foo");
  check-equal("", #(), #());
  check-equal("", #"abc", #"abc");
end test id?-0;

// It's not clear whether literal constants are id?
//  (not (id? '(1 2 3) '(1 2 3)))
//  (not (id? "abc" "abc"))

// #rest args
define test id?-1 ()
  check-true("", #t == #t & #t == #t);
  check-true("", #f == even?(3) & ~(#(1, 2, 3) == #(1, 2, 3)));
end test id?-1;

define test equal-type ()
  check-true("", instance?(\=, <function>));
  check-true("", instance?(\=, <generic-function>));
end test equal-type;

// Numeric cases
define test equal-0 ()
  check-true("", 1 + 2 = 5 - 2);
  check-true("", 1 + 2 = 3);
  check-true("", 3 = 3);
  check-true("", ~(3 = 4));
//  check-true("<ratio> is undefined in emulator", {RATIO instance} = {RATIO instance});
end test equal-0;

// = on complex numbers
define test equal-complex ()
  check-true("", complex-instance() = complex-instance());
end test equal-complex;

// collections
define test a=-1 ()
  check-true("", "abc" = "abc" & "abc" = "abc");
  check-true("", ~("abc" = "aBc" & "abc" = "abc"));
  check-true("", list(#"a", #"b", #"c") = list(#"a", #"b", #"c"));
  check-true("", list(#"a", #"b", #"c") = list(#"a", #"b", #"c"));
  check-true("", ~(list(#"a", #"z", #"c") = list(#"a", #"b", #"c")));
  check-false("", list(#"a", #"z", #"c") = list(#"a", #"b", #"c"));
  check-true("", stretchy-vector-instance(1, 2, 3) = stretchy-vector-instance(1, 2, 3));
  check-true("", stretchy-vector-instance(1, 2, 3) = stretchy-vector-instance(1, 2, 3));
  check-true("", ~(stretchy-vector-instance(1, 2, 3) = stretchy-vector-instance(1, 999, 3)));
  check-true("", stretchy-vector-instance(1, 2, 3) = stretchy-vector-instance(1, 2, 3));
  check-true("", vector(1, 2, 3)
     = vector(1, 2, 3));
  check-true("", vector(1, 2, 3)
       = vector(1, 2, 3));
  check-true("", ~(vector(1, 2, 3)
      = vector(1, 999, 3)));
  check-true("", vector(1, 2, 3)
        = vector(1, 2, 3));
  check-true("", (#() = list() & #() = #()));
  check-true("", ~(list() = #(1) & list() = #(1)));
//  check-true("", (pair(1, 2) = #(1 . 2) & pair(1, 2) = #(1 . 2)));
//  check-true("", ~(pair(1, 2) = #(1 . 0) & pair(1, 2) = #(1 . 0)));
  check-true("", (deque-instance(1, 2, 3) = deque-instance(1, 2, 3)));
  check-true("", deque-instance(1, 2, 3) = deque-instance(1, 2, 3));
  check-true("", ~(deque-instance(1, 2, 3) = deque-instance(1, 999, 3)));
  check-true("", deque-instance(1, 2, 3) = deque-instance(1, 2, 3));
  check-true("", (' ' = ' ' & ' ' = ' '));
  check-true("", ~(' ' = ' ' & ' ' = '\n'));
  check-true("", (range = range & range = range));
  check-true("", ~(range = range & range = instance?));
  check-true("", \= = \= & \= = \=);
  check-false("", \= = \= & \= = \&=);
  //---*** Triggers compiler crash:
  // check-true("", \& = \& & \& = \&=);
  // check-false("", \& = \& & \& = =hash);
end test a=-1;

define test a=-type ()
  check-true("", instance?(\&=, <function>));
  check-false("", instance?(\&=, <generic-function>));
end test a=-type;

// Numeric cases
define test qw ()
  check-true("", 3 ~= 4);
//  check-true("<ratio> is yet undefined", {RATIO instance} ~= {RATIO instance});
end test;

// ~= on complex numbers
define test p=-complex ()
  check-true("", complex-instance(real: 5, imag: 2) ~= complex-instance(real: 5, imag: 3));
end test;

// collections
define test p=-1 ()
  check-true("", "abc" = "abc");
  check-true("", "abc" ~= "aBc");
  check-true("", list(#"a", #"z", #"c") ~= list(#"a", #"b", #"c"));
  check-true("", stretchy-vector-instance(1, 999, 3) ~= stretchy-vector-instance(1, 2, 3));
  check-true("", vector(1, 999, 3)
    ~= vector(1, 2, 3));
  check-true("", list() ~= #(1));
//  check-true("pairs dont seem to work in the emulator", pair(1, 2) ~= #(1 . 0));
  check-true("", deque-instance(1, 999, 3) ~= deque-instance(1, 2, 3));
  check-true("", ' ' ~= '\n');
  check-true("", range ~= instance?);
  check-true("", \= ~= \&=);
  //---*** Triggers compiler crash:
  // check-true("", \& ~= =hash);
end test p=-1;

// symbols - not case sensitive
define test p=-2 ()
  check-true("", #"foo" = #"FOO" & #"FOo" = #"foO");
end test;

define test binary=-type ()
  check-true("", instance?(binary=, <generic-function>));
end test binary=-type;

define test binary= ()
  check-true("", ~binary=(1, 2));
  check-true("", binary=(#(1, 2, 3), #(1, 2, 3)));
end test binary=;

define test p=hash-type ()
  check-true("", instance?(=hash, <generic-function>));
end test;

// Numeric cases
define test p=hash ()
  check-true("", =hash(1 + 2) = =hash(5 - 2));
  check-true("", =hash(3) = =hash(3));
//  check-true("<ratio> is undefined", =hash({RATIO instance}) = =hash({RATIO instance}));
end test;

// =hash on complex
define test =hash-complex ()
  check-equal("", complex-instance(real: 3, imag: 2).=hash,
  complex-instance(real: 3, imag: 2).=hash);
end test =hash-complex;

// collections
define test =hash-1 ()
  check-true("", =hash("abc") = =hash("abc"));
  check-true("", list(#"a", #"b", #"c").=hash = list(#"a", #"b", #"c").=hash);
  check-equal("", stretchy-vector-instance(1, 2, 3).=hash,
     stretchy-vector-instance(1, 2, 3).=hash);
  check-equal("", vector(1, 2, 3).=hash,
     vector(1, 2, 3).=hash);
  check-true("", =hash(#()) = list().=hash);
//  check-true("pairs dont work in the emulator", pair(1, 2).=hash = =hash(#(1 . 2)));
  check-true("", deque-instance(1, 2, 3).=hash = deque-instance(1, 2, 3).=hash);
  check-true("", =hash(' ') = =hash(' '));
  check-true("", range.=hash = range.=hash);
  check-true("", \=.=hash = \=.=hash);
  //---*** Triggers compiler crash:
  // check-true("", \&.=hash = \&.=hash);
end test =hash-1;

define test less-than-type ()
  check-true("", instance?(\<, <function>) & ~instance?(\<, <generic-function>));
end test less-than-type;

// Numeric cases
define test q ()
  check-true("", 1 + 2 < 15 - 4);
  check-true("", 3 < 5);
  check-true("", 3 < 5);
  check-true("", 3 < 4);
//  check-true("<ratio> undefined", {RATIO instance} < {RATIO instance});
end test q;

// strings, without knowing underlying char set
define test less-than-1 ()
  check-true("", "prefix" < "prefix is less than");
  check-true("", if ('a' < 'c')
      "aaa" < "ccc"
    else
      "ccc" < "aaa"
    end if);
end test less-than-1;

define test greater-than-type ()
  check-true("", instance?(\>, <function>) & ~instance?(\>, <generic-function>));
end test greater-than-type;

// Numeric cases
define test w> ()
  check-true("", 15 - 4 > 1 + 2);
  check-true("", 5 > 3);
  check-true("", (9 > 5 & 9 > 3));
//  check-true("<ratio> undefined", {RATIO instance} > {RATIO instance});
end;

// strings, without knowing underlying char set
define test greater-than-1 ()
  check-true("", "prefix is less than" > "prefix");
  check-true("", if ('a' > 'c')
      "aaa" > "ccc"
    else
      "ccc" > "aaa"
    end if);
end test greater-than-1;

define test binary<-type ()
  check-true("", instance?(binary<, <generic-function>));
end test binary<-type;

define test binary< ()
  check-true("", binary<(1, 2));
end test binary<;

/*

// Transitivity of items.  Page 75.

(define-test BINARY<-2 ()
  ""
  (bind
    ((t 'true) (range1 (range from: 2 to: 50 by: 2))
     (range2 (range from: 3 to: 60 by: 3))
     (range3 (range from: 1 to: 40)))
    (=
     (for
       ((item1 (initial-state range1) (next-state range1 item1))
        (item2 (initial-state range2) (next-state range2 item2))
        (item3 (initial-state range3) (next-state range3 item3)))
       ((or (not item1) (not item2) (not item3)) t)
       (when ;; KJP: if -> when
         (and (and (binary< item1 item2) (binary< item2 item3))
              (not (binary< item1 item3)))
         (set! t 'impossible)))
     'true)))


// Trichotomy, antireflexivity and commutativity on items. Page 75 and footnote.

(define-test FLOW-CONTROL ()
  ""
  (bind ((t 'true)
         (range1 (range from: 2 to: 50 by: 3))
         (range2 (range from: 10 to: 50 by: 2)))
    (=
     (for
       ((item1 (initial-state range1) (next-state range1 item1))
        (item2 (initial-state range2) (next-state range2 item2)))
       ((or (not item1) (not item2)) t)
       (when ;; KJP: if -> when
         (or
          (or (and (binary< item1 item2) (binary= item1 item2))
              (and (binary= item1 item2) (binary< item2 item1))
              (and (binary< item1 item2) (binary< item2 item1)))
          (and (id? item1 item2) (binary< item1 item2))
          (and (binary= item1 item2)
               (not (binary= item2 item1))))
         (set! t 'impossible)))
     'true)))
*/

define suite test-comparison-suite ()
  test id?-type;
  test id?-0;
  test id?-1;
  test equal-type;
  test equal-0;
  test equal-complex;
  test a=-1;
  test a=-type;
  test qw;
  test p=-complex;
  test p=-1;
  test p=-2;
  test binary=-type;
  test binary=;
  test p=hash-type;
  test p=hash;
  test =hash-complex;
  test =hash-1;
  test less-than-type;
  test less-than-1;
  test q;
  test greater-than-type;
  test w>;
  test greater-than-1;
  test binary<-type;
  test binary<;
end suite;
