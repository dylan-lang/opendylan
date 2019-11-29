Module:       common-dylan-test-suite
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sideways method make-test-instance
    (class == <notification>) => (object)
  let lock = make(<lock>);
  make(<notification>, lock: lock)
end method make-test-instance;

define test test-<notification> ()
  //---*** Fill this in...
end test;

define test test-associated-lock ()
  //---*** Fill this in...
end test;

define test test-<not-owned-error> ()
  //---*** Fill this in...
end test;

define test test-release-all ()
  //---*** Fill this in...
end test;

