Module: dylan-user


define library opendylan-test-suite
  use common-dylan;
  use testworks;

  // Bundled with opendylan...
  use collections-test-suite;
  use common-dylan-test-suite;
  use deuce-test-suite;
  use dfmc-reader-test-suite;
  use duim-test-suite;
  use dylan-test-suite;
  use environment-test-suite;
  use file-source-records-test-suite;
  use io-test-suite;
  use jam-test-suite;
  use system-test-suite;
  use variable-search-tests;

  // Submodules
  use channels-test-suite;
  use collection-extensions-test-suite;
  use command-line-parser-test-suite;
  // use deft-test-suite;      // TODO: Needs to have a non-app test suite
  use hash-algorithms-test;
  // use json-test-suite;      // TODO: Needs to have a non-app test suite
  use logging-test-suite;
  // use lsp-dylan-test-suite; // TODO: Needs to have a non-app test suite
  // use meta-test-suite;      // TODO: Needs to have a non-app test suite
  // use regular-expressions-test-suite;      // TODO: Needs to have a non-app test suite
  use strings-test-suite;
  use testworks-test-suite;
  use xml-test-suite;          // Used only by testworks as of 2025.

  export opendylan-test-suite
end library;

define module opendylan-test-suite
  use common-dylan;
  use testworks;

  // Bundled with opendylan...
  use collections-test-suite;
  use common-dylan-test-suite;
  use deuce-test-suite;
  use duim-test-suite;
  use dylan-test-suite;
  use environment-test-suite;
  use file-source-records-test-suite;
  use io-test-suite;
  use system-test-suite;
  // use variable-search-tests; // TODO: module not exported

  // Submodules...
  // use channels-test-suite;
  use collection-extensions-test-suite;
  // use command-line-parser-test-suite; // TODO: module not exported
  // use deft-test-suite;      // TODO: Needs to have a non-app test suite
  // use hash-algorithms-test; // TODO: module not exported
  // use json-test-suite;      // TODO: Needs to have a non-app test suite
  // use logging-test-suite;   // TODO: module not exported
  // use lsp-dylan-test-suite; // TODO: Needs to have a non-app test suite
  // use meta-test-suite;      // TODO: Needs to have a non-app test suite
  // use regular-expressions-test-suite;      // TODO: Needs to have a non-app test suite
  use strings-test-suite;      // TODO: module not exported
  use testworks-test-suite;
  use xml-test-suite;          // Used only by testworks submodule as of 2025.

  //export opendylan-test-suite;
end module;
