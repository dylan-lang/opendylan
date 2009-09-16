Module:       common-dylan-test-suite
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sideways method make-test-instance
    (class == <notification>) => (object)
  let lock = make(<lock>);
  make(<notification>, lock: lock)
end method make-test-instance;

define threads class-test <notification> ()
  //---*** Fill this in...
end class-test <notification>;

define threads function-test associated-lock ()
  //---*** Fill this in...
end function-test associated-lock;

define threads class-test <not-owned-error> ()
  //---*** Fill this in...
end class-test <not-owned-error>;

define threads function-test release-all ()
  //---*** Fill this in...
end function-test release-all;

