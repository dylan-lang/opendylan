module: dylan-user

define library dfmc-typist-tests
  use common-dylan;
  use io;
  use testworks;
  use system;
  use registry-projects;
  use release-info;

  use dfmc-core;
  use dfmc-typist;
  use dfmc-management;
  use dfmc-debug-back-end;
  use dfmc-optimization; //to get program-note classes
  use dfmc-browser-support;

  use dfmc-visualization;

  use projects;
  use environment-protocols;
  use dfmc-environment-projects;
end library;

define module dfmc-typist-tests
  use common-dylan;
  use threads, import: { dynamic-bind };
  use format-out;
  use print, import: { print-object };
  use streams;
  use testworks;
  use format;
  use date, import: { current-date, as-iso8601-string };

  use dfmc-core;
  use dfmc-typist;
  use dfmc-management;
  use dfmc-debug-back-end;
  use dfmc-optimization;

  use dfmc-visualization;

  use projects;
  use projects-implementation, import: { project-current-compilation-context, project-build-settings };
  use environment-protocols,
    import: { find-project, open-project-compiler-database, project-warnings };
  use dfmc-environment-projects; //needed for find-project
  use dfmc-project-compilation, import: { compilation-context-project };

  use file-system;
  use locators;
  use registry-projects;
  use release-info;
  use operating-system;
end module;
