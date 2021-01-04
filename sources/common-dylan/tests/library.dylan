Module:       dylan-user
Synopsis:     Common Dylan library test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library common-dylan-test-suite
  use dylan,
    import: { dylan, dylan-extensions, simple-debugging };
  use common-dylan;
  use system,
    import: { file-system };
  use testworks;

  export common-dylan-test-suite;
end library;

define module common-dylan-test-suite
  use dylan;
  use dylan-extensions,
    import: { encode-single-float,
              encode-double-float,
              <abstract-integer> };
  use common-dylan-internals;
  use common-extensions;
  use streams-protocol;
  use locators-protocol;
  use finalization;

  // TODO(cgay): Should these be exported from common-extensions like the other
  // names in the simple-debugging:dylan module are? (They're needed here to
  // turn on debugging to test those other definitions.)
  use simple-debugging,
    import: { debugging?, debugging?-setter };

  use simple-format;
  use simple-random;
  use simple-profiling;
  use file-system,
    import: { file-exists? };
  use transcendentals;
  use byte-vector;
  use machine-words;
  use threads;

  use testworks;

  // Common Dylan test suite
  export common-dylan-test-suite;

  // Stream testing protocol
  export <stream-test-info>,
         make-stream-tests-of-size,
         info-test-name,
         info-class-info,
         info-contents,
         info-direction,
         info-make-function,
         info-destroy-function,
         register-stream-test,
         stream-class-info,
         test-stream-class;

  // Stream class info
  export <stream-class-info>,
         info-class-name,
         info-class,
         info-input-stream?,
         info-output-stream?,
         info-element-type,
         register-stream-class-info,
         registered-stream-classes;

  // Test streams
  export <test-input-stream>,
         <test-output-stream>;
end module common-dylan-test-suite;
