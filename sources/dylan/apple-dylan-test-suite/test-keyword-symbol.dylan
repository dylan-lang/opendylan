Module:    apple-dylan-test-suite
Filename:  test-keyword-symbol.dylan
Summary:   Apple Dylan test suite, test-keyword-symbol
Version:   29-Oct-93
Copyright: (c) 1993 Apple Computer, Inc.

/*---------------------------------------------
Modified by: Shri Amit(amit)
Date: August 24 1996
Summary: Converted to new testworks protocol
Copyright: (c) 1996 Functional Objects, Inc. 
           All rights reserved.  
----------------------------------------------*/

// Chapter 15. Keywords and Symbols
// keyword is self-evaluating

define test keyword-symbol-names (description: "")
  check-true("", #"foo" = #"foo");
  check-true("", "FOO" = map(as-uppercase, as(<string>, #"foo")));
  check-true("", "FOO" = map(as-uppercase, as(<string>, #"foo")));
end test keyword-symbol-names;

define test as-symbol-keyword (description: "as <symbol>")
  check-true("", #"foo" == as(<symbol>, byte-string-instance('F', 'o', 'o')));
  check-true("",  #"foo" == as(<symbol>, byte-string-instance('f', 'O', 'O')));
  check-true("",  #"foo" == as(<symbol>, byte-string-instance('f', 'o', 'o')));
  check-true("",  #"foo" == as(<symbol>, byte-string-instance('F', 'O', 'O')));
  check-true("", #"foo" == as(<keyword>, byte-string-instance('F', 'o', 'o')));
  check-true("",  #"foo" == as(<keyword>, byte-string-instance('f', 'O', 'O')));
  check-true("", #"foo" == as(<keyword>, byte-string-instance('f', 'o', 'o')));
  check-true("", #"foo" == as(<keyword>, byte-string-instance('F', 'O', 'O')));
end test as-symbol-keyword;

define suite test-keyword-symbol-suite () 
  test keyword-symbol-names;
  test as-symbol-keyword;
end;
