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
  use dfmc-flow-graph;
  use dfmc-optimization;
  use dfmc-typist;
  use dfmc-management;
  use dfmc-back-end;
  use dfmc-debug-back-end;
  use projects;
  use environment-protocols;
  use dfmc-environment-projects;
  use dfmc-back-end-implementations;
  use dfmc-execution;
  use io;
  use testworks;
end library;

define module dfmc-testing-support
  use common-dylan;
  use dfmc-management;

  export compile-library-until-optimized;
end module;

define module dfmc-execution-testing
  use common-dylan;
  use testworks;
  use dfmc-core;
  use dfmc-debug-back-end, import: {*print-method-bodies?*};
  use dfmc-execution;
  use dfmc-imports;
  use dfmc-management;
  use dfmc-runtime-execution;
  use projects;
  use dfmc-testing-support;

  export dfmc-execution-suite;
end module;

define module dfmc-flow-graph-environment-testing
  use common-dylan;
  use testworks;
  use dfmc-reader;
  use dfmc-flow-graph;

  export dfmc-flow-graph-environment-suite;
end;

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
  use dfmc-testing-support;
  use streams;
  use standard-io;
  use testworks;

  use dfmc-flow-graph-environment-testing;
  use dfmc-execution-testing;
end module;
