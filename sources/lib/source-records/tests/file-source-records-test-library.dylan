Module: dylan-user

define library file-source-records-test-suite
  use common-dylan;
  use io;
  use file-source-records;
  use testworks;

  export file-source-records-test-suite;
end library;

define module file-source-records-test-suite
  use common-dylan;
  use file-source-records;
  use streams;
  use testworks;

  export file-source-records-test-suite;
end module;
