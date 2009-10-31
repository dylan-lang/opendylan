Module:       Dylan-User
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library llvm-test-suite
  use common-dylan;
  use testworks;
  use testworks-specs;
  use io;
  use system;
  use llvm;

  export llvm-test-suite;
end library;

define module llvm-test-suite
  use common-dylan;
  use testworks;
  use testworks-specs;
  use streams;
  use file-system;
  use locators;
  use operating-system;
  use llvm;

  export llvm-test-suite;
end module;
