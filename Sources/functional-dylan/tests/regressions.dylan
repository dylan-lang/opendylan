Module:    functional-dylan-test-suite
Synopsis:  Functional Objects extensions library test suite
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test bug-4252 ()
  check-no-errors("string-to-integer of minimum integer",
		  string-to-integer(integer-to-string($minimum-integer)));
end test bug-4252;

define test bug-4401 ()
  check-equal("string-to-integer(\"$\", default: -17)",
	      string-to-integer("$", default: -17),
	      -17);
  check-equal("string-to-integer(\"-$\", default: -17)",
	      string-to-integer("-$", default: -17),
	      -17);
  check-equal("string-to-integer(\"\", default: -17)",
	      string-to-integer("", default: -17),
	      -17);
  check-equal("string-to-integer(\" \", default: -17)",
	      string-to-integer(" ", default: -17),
	      -17);
  check-equal("string-to-integer(\"-\", default: -17)",
	      string-to-integer("-", default: -17),
	      -17);
end test bug-4401;

define test bug-2798 ()
  check-no-errors("remove-all-keys! has a method on <set>",
                  remove-all-keys!(make(<set>)));
end test bug-2798;

define suite functional-dylan-regressions ()
  test bug-4252;
  test bug-4401;
  test bug-2798;
end suite functional-dylan-regressions;
