Module:       Dylan-User
Synopsis:     Tests for DOM
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module dom-tests
  use functional-dylan,
    exclude: { position, position-if, count };
  use dylan-extensions,
    import: { \without-bounds-checks,
	      element-no-bounds-check,
	      element-no-bounds-check-setter,
	      element-range-error };
  use simple-format;			// for debugging
  use testworks;
  use dom-internals;

  export dom-test-suite;
end module dom-tests;
