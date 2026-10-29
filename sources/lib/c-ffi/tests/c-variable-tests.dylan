Module:    c-ffi-test-suite
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// A couple of variables for the c-variable-test
define c-address $foos-address :: <c-unsigned-long*>
 c-name: "foo";
end;

define c-variable foo-value :: <c-unsigned-long>
 c-name: "foo";
end;


/// try some things with a variable defined via define c-variable
define test variable-address-test ()
  check-equal("set c-address",
               (pointer-value($foos-address) := 1), 1);
  check-equal("c-address value", pointer-value($foos-address), 1);
  check-equal("c-address value from c-variable", foo-value(), 1);
  check-equal("set c-variable: %s should be %s",
               (foo-value() := 777), 777);
  check-equal("c-address value 777", pointer-value($foos-address), 777);
  check-equal("c-variable value 777", foo-value(), 777);
  // !@#$ need more tests that check for agreement with C
end;

