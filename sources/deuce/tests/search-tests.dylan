Module:       deuce-test-suite
Synopsis:     Test suite for the Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test simple-search-test ()
  let p1 = "test";
  let p2 = "frob";
  let source = "this is a simple test source string";
  // Reverse
  check-equal("Find a matching substring",
	      string-search(p1, source), 17);
  check-equal("Find a matching substring starting at 5",
	      string-search(p1, source, start: 5), 17);
  check-equal("Find a matching substring ending at 25",
	      string-search(p1, source, start: 5, end: 25), 17);
  check-equal("Miss on a non-matching substring",
	      string-search(p2, source), #f);
  check-equal("Miss on a non-matching substring starting at 5",
	      string-search(p2, source, start: 5), #f);
  check-equal("Miss on a non-matching substring ending at 25",
	      string-search(p2, source, start: 5, end: 25), #f);
  check-equal("Find an upcased substring",
	      string-search(as-uppercase(p1), source), 17);
  check-equal("Find an upcased substring starting at 5",
	      string-search(as-uppercase(p1), source, start: 5), 17);
  check-equal("Find an upcased substring ending at 25",
	      string-search(as-uppercase(p1), source, start: 5, end: 25), 17);
  check-equal("Miss on an upcased substring",
	      string-search(as-uppercase(p1), source, test: \=), #f);
  check-equal("Miss on an upcased substring starting at 5",
	      string-search(as-uppercase(p1), source, start: 5, test: \=), #f);
  check-equal("Miss on an upcased substring ending at 25",
	      string-search(as-uppercase(p1), source, start: 5, end: 25, test: \=), #f);
  // Reverse
  check-equal("Find a matching substring",
	      string-reverse-search(p1, source), 17);
  check-equal("Find a matching substring starting at 5",
	      string-reverse-search(p1, source, start: 5), 17);
  check-equal("Find a matching substring ending at 25",
	      string-reverse-search(p1, source, start: 5, end: 25), 17);
  check-equal("Miss on a non-matching substring",
	      string-reverse-search(p2, source), #f);
  check-equal("Miss on a non-matching substring starting at 5",
	      string-reverse-search(p2, source, start: 5), #f);
  check-equal("Miss on a non-matching substring ending at 25",
	      string-reverse-search(p2, source, start: 5, end: 25), #f);
  check-equal("Find an upcased substring",
	      string-reverse-search(as-uppercase(p1), source), 17);
  check-equal("Find an upcased substring starting at 5",
	      string-reverse-search(as-uppercase(p1), source, start: 5), 17);
  check-equal("Find an upcased substring ending at 25",
	      string-reverse-search(as-uppercase(p1), source, start: 5, end: 25), 17);
  check-equal("Miss on an upcased substring",
	      string-reverse-search(as-uppercase(p1), source, test: \=), #f);
  check-equal("Miss on an upcased substring starting at 5",
	      string-reverse-search(as-uppercase(p1), source, start: 5, test: \=), #f);
  check-equal("Miss on an upcased substring ending at 25",
	      string-reverse-search(as-uppercase(p1), source, start: 5, end: 25, test: \=), #f);
end test simple-search-test;

define test boyer-search-test ()
  let p1 = "test";
  let p2 = "frob";
  let source = "this is a boyer test source string";
  // Reverse
  check-equal("Find a matching substring",
	      boyer-search(p1, source), 16);
  check-equal("Find a matching substring starting at 5",
	      boyer-search(p1, source, start: 5), 16);
  check-equal("Find a matching substring ending at 25",
	      boyer-search(p1, source, start: 5, end: 25), 16);
  check-equal("Miss on a non-matching substring",
	      boyer-search(p2, source), #f);
  check-equal("Miss on a non-matching substring starting at 5",
	      boyer-search(p2, source, start: 5), #f);
  check-equal("Miss on a non-matching substring ending at 25",
	      boyer-search(p2, source, start: 5, end: 25), #f);
  check-equal("Find an upcased substring",
	      boyer-search(as-uppercase(p1), source), 16);
  check-equal("Find an upcased substring starting at 5",
	      boyer-search(as-uppercase(p1), source, start: 5), 16);
  check-equal("Find an upcased substring ending at 25",
	      boyer-search(as-uppercase(p1), source, start: 5, end: 25), 16);
  check-equal("Miss on an upcased substring",
	      boyer-search(as-uppercase(p1), source, test: \=), #f);
  check-equal("Miss on an upcased substring starting at 5",
	      boyer-search(as-uppercase(p1), source, start: 5, test: \=), #f);
  check-equal("Miss on an upcased substring ending at 25",
	      boyer-search(as-uppercase(p1), source, start: 5, end: 25, test: \=), #f);
end test boyer-search-test;

define suite search-suite ()
  test simple-search-test;
  test boyer-search-test;
end suite search-suite;
