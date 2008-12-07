module: dylan-user

define library dfmc-typist-tests
  use common-dylan;
  use io;
  use testworks;

  use dfmc-core;
  use dfmc-typist;
  use dfmc-management;
  use dfmc-debug-back-end;
  use dfmc-optimization; //to get program-note classes

  use projects;
  use environment-protocols;
  use dfmc-environment-projects;
end library;

define module dfmc-typist-tests
  use common-dylan;
  use threads, import: { dynamic-bind };
  use format-out;
  use testworks;

  use dfmc-core;
  use dfmc-typist;
  use dfmc-management;
  use dfmc-debug-back-end, import: {*print-method-bodies?*};
  use dfmc-optimization;

  use projects;
  use environment-protocols,
    import: { find-project, open-project-compiler-database, project-warnings };
  use dfmc-environment-projects; //needed for find-project
end module;
