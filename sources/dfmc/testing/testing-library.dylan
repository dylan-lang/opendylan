Module: Dylan-User
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-testing
  use functional-dylan;
  use dfmc-core;
  use dfmc-reader; // not part of core for some reason.
  use dfmc-macro-expander;
  use dfmc-optimization;
  use dfmc-typist;
  use dfmc-management;
  use dfmc-back-end;
  use dfmc-debug-back-end;
  use dfmc-execution;
  use projects;
  export dfmc-testing;
end library;

define module dfmc-testing
  use functional-dylan;
  use dfmc-core;
  use dfmc-reader; // not part of core for some reason.
  use dfmc-macro-expander;
  use dfmc-imports;
  use dfmc-optimization;
  use dfmc-typist;
  use dfmc-management;
  use dfmc-back-end;
  use dfmc-debug-back-end, import: {*print-method-bodies?*};
  use dfmc-execution;
  use projects;
  export 
    clear-tests, print-test-report, run-tests,
    show-lambda-type-estimates;
end module;

