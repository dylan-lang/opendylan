Module:       dylan-test-suite
Synopsis:     Dylan test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Constant testing

define dylan constant-test $permanent-hash-state ()
  //---*** Fill this in...
end constant-test $permanent-hash-state;

define dylan-extensions constant-test $minimum-integer ()
  //---*** Add some more tests here...
  check-condition("$minimum-integer - 1 overflows",
		  <error>,
		  $minimum-integer - 1)
end constant-test $minimum-integer;

define dylan-extensions constant-test $maximum-integer ()
  //---*** Add some more tests here...
  check-condition("$maximum-integer + 1 overflows",
		  <error>,
		  $maximum-integer + 1)
end constant-test $maximum-integer;
