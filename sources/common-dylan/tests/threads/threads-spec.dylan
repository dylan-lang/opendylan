Module:       common-dylan-test-suite
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sideways method make-test-instance
    (class == <thread>) => (object)
  make(<thread>, function: always(#f))
end method make-test-instance;

define sideways method destroy-test-instance
    (class == <thread>, thread :: <thread>) => ()
  join-thread(thread);
end method destroy-test-instance;

define test test-<thread> ()
  //---*** Fill this in...
end test;

define test test-$low-priority ()
  //---*** Fill this in...
end test;

define test test-$background-priority ()
  //---*** Fill this in...
end test;

define test test-$normal-priority ()
  //---*** Fill this in...
end test;

define test test-$interactive-priority ()
  //---*** Fill this in...
end test;

define test test-$high-priority ()
  //---*** Fill this in...
end test;

define test test-thread-name ()
  //---*** Fill this in...
end test;

define test test-thread-id ()
  //---*** Fill this in...
end test;

define test test-join-thread ()
  //---*** Fill this in...
end test;

define test test-<duplicate-join-error> ()
  //---*** Fill this in...
end test;

define test test-thread-yield ()
  //---*** Fill this in...
end test;

define test test-current-thread ()
  //---*** Fill this in...
end test;

define test test-current-thread-id ()
end test;
