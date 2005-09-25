Module:       dylan-user
Synopsis:     TestWorks - a test harness library for dylan
Author:       Andrew Armstrong, James Kirsch
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library testworks
  use common-dylan;
  use io;

  export testworks;
end library testworks;

define module testworks
  use common-dylan;
  use format-out;
  use threads,
    import: { dynamic-bind };

  // Debugging options
  export *debug?*,
         debug-failures?,
         debug?;

  // Formatting
  export *format-function*,
         test-output,
         plural;

  // Announcing suite/test/check names
  export *announce-checks?*,
         *announce-check-function*;

  // Components
  export <component>,
         execute-component?,
         perform-component,
         component-name,
         component-description,
         component-tags,
         status-name;

  // Perform options
  export <perform-options>,
         perform-tags, perform-tags-setter,
         perform-announce-function, perform-announce-function-setter,
         perform-announce-checks?, perform-announce-checks?-setter,
         perform-progress-format-function, perform-progress-format-function-setter,
         perform-progress-function, perform-progress-function-setter,
         perform-debug?, perform-debug?-setter;

  // Checks
  export check,
         check-condition,
         check-equal,
         check-false,
         check-no-errors,
         check-instance?,
         check-true;

  // Benchmarks
  export benchmark;

  // Tests
  export <test>,
         test-definer,
         \with-test-unit,
         test-function,
         perform-test,
         find-test,
         find-test-object;
     
  // Suites
  export <suite>,
         make-suite,   //--- Needed for macro hygiene problems
         suite-definer,
         suite-setup-function, suite-cleanup-function,
         suite-components,
         perform-suite,
         root-suite,
         find-suite;  

  // Result objects
  export <result>,
         result-name,
         result-type-name,
         result-status,
         result-seconds,
         result-microseconds,
         result-time,
         result-bytes,

         <component-result>,
         result-subresults,

         <test-result>,
         <suite-result>,
         <unit-result>,
         result-operation,
         result-value,
         do-results,

         <check-result>,
         <benchmark-result>,
         $benchmark-result-divider,
         print-one-benchmark-result,
         print-benchmark-result-header,
         print-benchmark-result-footer;

  // Progress functions
  export *default-progress-function*,
         null-progress-function,
         full-progress-function;

  // Report functions
  export *default-report-function*,
         display-results,
         null-report-function,
         summary-report-function,
         failures-report-function,
         full-report-function,
         log-report-function;

  // Command line handling
  export run-test-application;

  // Internals for use by testworks-test-suite
  export $test-log-header,
         $test-log-footer,
         *check-recording-function*,
         failure-reason,
         safe-error-to-string;

  // Internals -- mostly due to macro hygiene failures
  export $test-objects-table,
         do-check,
         do-check-condition,
         do-benchmark,
         record-test-unit-crash,
         print-failure-reason;
end module testworks;
