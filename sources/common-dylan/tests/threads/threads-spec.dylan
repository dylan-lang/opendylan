Module:       common-dylan-test-suite
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sideways method make-test-instance
    (class == <thread>) => (object)
  make(<thread>, function: always(#f))
end method make-test-instance;

define sideways method destroy-test-instance
    (class == <thread>, thread :: <thread>) => ()
  join-thread(thread);
end method destroy-test-instance;

define threads class-test <thread> ()
  //---*** Fill this in...
end class-test <thread>;

define threads constant-test $low-priority ()
  //---*** Fill this in...
end constant-test $low-priority;

define threads constant-test $background-priority ()
  //---*** Fill this in...
end constant-test $background-priority;

define threads constant-test $normal-priority ()
  //---*** Fill this in...
end constant-test $normal-priority;

define threads constant-test $interactive-priority ()
  //---*** Fill this in...
end constant-test $interactive-priority;

define threads constant-test $high-priority ()
  //---*** Fill this in...
end constant-test $high-priority;

define threads function-test thread-name ()
  //---*** Fill this in...
end function-test thread-name;

define threads function-test join-thread ()
  //---*** Fill this in...
end function-test join-thread;

define threads class-test <duplicate-join-error> ()
  //---*** Fill this in...
end class-test <duplicate-join-error>;

define threads function-test thread-yield ()
  //---*** Fill this in...
end function-test thread-yield;

define threads function-test current-thread ()
  //---*** Fill this in...
end function-test current-thread;

