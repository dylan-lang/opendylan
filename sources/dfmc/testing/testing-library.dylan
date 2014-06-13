Module: Dylan-User
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-testing
  use common-dylan;
  use dfmc-core;
  use dfmc-reader; // not part of core for some reason.
  use dfmc-macro-expander;
  use dfmc-optimization;
  use dfmc-typist;
  use dfmc-management;
  use dfmc-back-end;
  use dfmc-debug-back-end;
  use projects;
  use environment-protocols;
  use dfmc-environment-projects;
  use dfmc-back-end-implementations;

  export dfmc-testing;
end library;

define module dfmc-testing
  use common-dylan;
  use dfmc-core;
  use dfmc-reader; // not part of core for some reason.
  use dfmc-macro-expander;
  use dfmc-imports;
  use dfmc-optimization;
  use dfmc-typist;
  use dfmc-management;
  use dfmc-back-end;
  use dfmc-debug-back-end, import: {*print-method-bodies?*};
  use projects;
  use environment-protocols,
    import: { find-project, open-project-compiler-database };
  use dfmc-environment-projects;

  export
    clear-tests, print-test-report, run-tests,
    show-lambda-type-estimates;
end module;

