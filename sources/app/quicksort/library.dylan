Module:    dylan-user
Synopsis:  Quicksort example
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library quicksort
  use functional-dylan;
  export quicksort;
end library quicksort;

define module quicksort
  use functional-dylan;
  use simple-format;
  use simple-random;
  use simple-profiling;
end module quicksort;

