Module:    functional-dylan-test-suite
Synopsis:  Functional Objects extensions library test suite
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Constant testing

define functional-extensions constant-test $unsupplied ()
  check-true("unsupplied?($unsupplied)", unsupplied?($unsupplied));
  check-false("supplied?($unsupplied) is false", supplied?($unsupplied));
end constant-test $unsupplied;

define functional-extensions constant-test $unfound ()
  check-true("unfound?($unfound)", unfound?($unfound));
  check-false("found?($unfound) is false", found?($unfound));
end constant-test $unfound;
