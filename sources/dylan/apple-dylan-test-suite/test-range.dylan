Module:    apple-dylan-test-suite
Filename:  test-range.dylan
Summary:   Apple dylan test suite, test-range
Version:   29-Oct-93
Copyright: (c) 1993 Apple Computer, Inc.

/*---------------------------------------------
Modified by: Shri Amit(amit)
Date: August 24 1996
Summary: Converted to new testworks protocol
Copyright: (c) 1996 Functional Objects, Inc.
           All rights reserved.
----------------------------------------------*/

/// Please read these accompanying files:
///
///   "setup.dylan"      for information on how to run these tests.
///   "legal-info.txt"   for important legal information.

// simple case
define test range-0 ()
  check-true("", instance?(range(), <range>));
  check-true("", begin
      let c = #();
      do(method (x)
           c := pair(x, c)
         end method,
         range(from: 10, below: 20));
      c
    end
    = #(19, 18, 17, 16, 15, 14, 13, 12, 11, 10));
end test;

// from: (inclusive) and up-t: (inclusive)
define test range-1 ()
  check-true("", range(from: 0, below: 5) = #(0, 1, 2, 3, 4));
end test range-1;

// from: defaults to 0
define test range-2 ()
  check-true("", range(below: 4) = #(0, 1, 2, 3));
end test range-2;

// to: (inclusive)
define test range-3 ()
  check-true("", range(to: 4) = #(0, 1, 2, 3, 4));
end test range-3;

// BY:
define test range-4 ()
  check-true("", range(to: 4, by: 2) = #(0, 2, 4));
end test range-4;

// to:, by:, and size:
define test range-5 ()
  check-true("", range(to: 5, by: 2, size: 3) = #(0, 2, 4));
  check-true("", range(to: 6, by: 2, size: 4) = #(0, 2, 4, 6));
  check-true("", range(to: -5, by: -2, size: 3) = #(0, -2, -4));
  check-true("", range(to: -6, by: -2, size: 4) = #(0, -2, -4, -6));
  check-condition("", <error>, range(to: 6, by: 2, size: 3));
  check-condition("", <error>, range(to: 6, by: 2, size: 3));
  check-condition("", <error>, range(to: -6, by: -2, size: 3));
  check-condition("", <error>, range(to: 6, by: 2, size: 5));
  check-condition("", <error>, range(to: 6, by: -2, size: 5));
end test range-5;

// member? range
// only tests features unique to range

// always terminates
define test member?-range ()
  check-false("", member?(-9999, range()) & member?(9999, range()));
end test member?-range;

// size range

// always terminates
define test size-range ()
  check-false("", range().size);
end test size-range;

// copy-sequence range

// copy-seq on range always returns <range>
define test copy-sequence-range-0 ()
  check-true("", instance?(range(to: 5).copy-sequence, <range>));
end test copy-sequence-range-0;

// = range

// always terminates
define test equal-range ()
  check-false("", range() = range(from: -5, below: 6));
  check-true("", range() = range());
end test equal-range;

// reverse range

// always returns a range
define test reverse-range ()
  check-true("", instance?(range(from: 1, to: 10, by: 2).reverse, <range>));
end test reverse-range;

// non-destructive
define test reverse-range-1 ()
  check-true("", #t);
end test reverse-range-1;

// always returns a range
define test reverse!-range ()
  check-true("", instance?(reverse!(range(from: 1, to: 10, by: 2)), <range>));
end test reverse!-range;

// range-intersection

// always terminates
define test intersection-range ()
  check-true("", intersection(range(), range()) = range());
  check-true("", intersection(range(from: 0, by: 2), range(from: 0, by: 3))
    = range(from: 0, by: 6));
end test intersection-range;

// returns a range
define test intersection-range-1 ()
  check-true("", instance?
    (intersection(range(from: 0, below: 5), range(from: 3, below: 9)), <range>));
  check-true("",instance?(intersection(range(), range(from: -1)), <range>));
end test intersection-range-1;

define suite test-range-suite ()
  test range-0;
  test range-1;
  test range-2;
  test range-3;
  test range-4;
  test range-5;
  test member?-range;
  test size-range;
  test copy-sequence-range-0;
  test equal-range;
  test reverse-range;
  test reverse-range-1;
  test reverse!-range;
  test intersection-range;
  test intersection-range-1;
end suite;
