Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test orbix-include-idl-orb ()
  check("", test-idl-file, *orbix-include-files*, "orb");
end test;

add-idl-file!(
  *orbix-include-files*,
  "orb",
"");
