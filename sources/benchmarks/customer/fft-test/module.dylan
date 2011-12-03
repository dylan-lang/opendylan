Module:    dylan-user
Author:    Andy Armstrong
Synopsis:  Gabriel benchmarks
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module fft-test
  use finalization;
  use common-dylan;
  use dylan-extensions, import: { \without-bounds-checks };
  use simple-io;
  use simple-random;
  use simple-profiling;
  use transcendentals;

  // Add binding exports here.

end module fft-test;
