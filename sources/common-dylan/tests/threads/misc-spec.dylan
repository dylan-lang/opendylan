Module:       common-dylan-test-suite
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Timers

define threads function-test sleep ()
  //---*** Fill this in...
end function-test sleep;


/// Dynamic binding

define threads macro-test dynamic-bind-test ()
  //---*** Fill this in...
end macro-test dynamic-bind-test;


/// Conditional update

define threads macro-test conditional-update!-test ()
  //---*** Fill this in...
end macro-test conditional-update!-test;

define threads class-test <conditional-update-error> ()
  //---*** Fill this in...
end class-test <conditional-update-error>;

define threads macro-test atomic-decrement!-test ()
  //---*** Fill this in...
end macro-test atomic-decrement!-test;

define threads macro-test atomic-increment!-test ()
  //---*** Fill this in...
end macro-test atomic-increment!-test;
