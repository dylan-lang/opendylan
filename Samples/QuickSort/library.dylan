Module:       dylan-user
Synopsis:     Quicksort demo program
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library quicksort
  use functional-dylan;
  use io;
  use system;
  export quicksort;
end library quicksort;

define module quicksort
  use functional-dylan;
  use dylan-extensions,
    import: { \without-bounds-checks,
	      element-no-bounds-check,
	      element-no-bounds-check-setter,
	      element-range-error };
  use format-out;
  use standard-io;
  use simple-random;
  use operating-system;
end module quicksort;
