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

define test range-0 (description: "simple case")
  check-true("", instance?(range(), <range>));
  check-true("", begin
      let c = #();
      do(method (x)
           c := pair(x, c)
         end method,
         range(from: 10, up-to: 20));
      c
    end
    = #(19, 18, 17, 16, 15, 14, 13, 12, 11, 10));
end test;

define test range-1 (description: "from: (inclusive) and up-t: (inclusive)")
  check-true("", range(from: 0, up-to: 5) = #(0, 1, 2, 3, 4));
end test range-1;

define test range-2 (description: "from: defaults to 0")
  check-true("", range(up-to: 4) = #(0, 1, 2, 3));
end test range-2;

define test range-3 (description: "through: (inclusive)")
  check-true("", range(through: 4) = #(0, 1, 2, 3, 4));
end test range-3;

define test range-4 (description: "BY:")
  check-true("", range(through: 4, by: 2) = #(0, 2, 4));
end test range-4;

// amit : I removed the up-to: in here because range does
// not except through and up-to in a single expression
//
define test range-5 (description: "Through:, by:, and size:")
  check-true("", range(through: 6, by: 2, size: 3) = #(0, 2, 4));
  check-true("", range(through: 6, by: 2, size: 5) = #(0, 2, 4, 6));
  check-true("", range(through: 6, by: 2, size: 3) = #(0, 2, 4));
  check-true("", range(through: 6, by: -2, size: 5) = #());
  check-true("", range(through: -6, by: -2, size: 3) = #(0, -2, -4));
end test range-5;

// member? range
// only tests features unique to range

define test member?-range (description: "always terminates")
  check-false("", member?(-9999, range()) & member?(9999, range()));
end test member?-range;

// size range

define test size-range (description: "always terminates")
  check-false("", range().size);
end test size-range;

// copy-sequence range

define test copy-sequence-0
  (description: "copy-seq on range always returns <range>")
  check-true("", instance?(range(through: 5).copy-sequence, <range>));
end test copy-sequence-0;

// binary= range

define test binary=-range (description: "always terminates")
  check-false("", binary=(range(), range(from: -5, up-to: 6))); 
  check-true("", binary=(range(), range()));
end test binary=-range;

// reverse range

define test reverse-range (description: "always returns a range")
  check-true("", instance?(range(from: 1, through: 10, by: 2).reverse, <range>));
end test reverse-range;

define test reverse-range-1 (description: "non-destructive")
  check-true("", #t);
end test reverse-range-1;

define test reverse!-range (description: "always returns a range")
  check-true("", instance?(reverse!(range(from: 1, through: 10, by: 2)), <range>));
end test reverse!-range;

// range-intersection

define test intersection-range (description: "always terminates")
  check-true("", binary=(intersection(range(), range()), range()));
  check-true("", intersection(range(from: 0, by: 2), range(from: 0, by: 3))
    = range(from: 0, by: 6));
end test intersection-range;

define test intersection-range-1 (description: "returns a range")
  check-true("", instance?
    (intersection(range(from: 0, up-to: 5), range(from: 3, up-to: 9)), <range>));
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
  test copy-sequence-0;
  test binary=-range;
  test reverse-range;
  test reverse-range-1;
  test reverse!-range;
  test intersection-range;
  test intersection-range-1;
end suite;
