Module:    dylan-user
Author:    Bruce Mitchener, Jr.
Copyright: Original Code is Copyright 2015 Dylan Hackers.
           All rights reserved.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define library coloring-stream-test-suite
  use common-dylan;
  use coloring-stream;
  use testworks;
  use testworks-specs;
  use io;

  export coloring-stream-test-suite;
end library;

define module coloring-stream-test-suite
  use common-dylan;
  use coloring-stream;
  use coloring-stream-internals;
  use format;
  use print;
  use testworks;
  use testworks-specs;
  use streams;
  use standard-io;

  export coloring-stream-test-suite;
end module;
