Module:       dylan-user
Synopsis:     Common Dylan library test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library common-dylan-test-suite
  use common-dylan;
  use testworks;
  use testworks-specs;
  use dylan-test-suite;

  export common-dylan-test-suite;
end library common-dylan-test-suite;

define module common-dylan-test-suite
  use dylan;
  use common-extensions;
  use streams-protocol;
  use locators-protocol;
  use finalization;
  use simple-io;
  use simple-random;
  use simple-profiling;
  use transcendentals;
  use byte-vector;
  use machine-words;
  use threads;

  use testworks;
  use testworks-specs;
  use dylan-test-suite;                // to get collection testing

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
