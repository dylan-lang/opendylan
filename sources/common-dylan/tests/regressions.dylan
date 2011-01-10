Module:       common-dylan-test-suite
Synopsis:     Common Dylan library test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test bug-5805 ()
  check-no-errors("string-to-integer of minimum integer",
		  string-to-integer(integer-to-string($minimum-integer)));
end test bug-5805;

define test bug-5954 ()
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
end test bug-5954;

define suite common-dylan-regressions ()
  test bug-5805;
  test bug-5954;
end suite common-dylan-regressions;
