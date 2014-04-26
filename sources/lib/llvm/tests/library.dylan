Module:       Dylan-User
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library llvm-test-suite
  use common-dylan;
  use testworks;
  use testworks-specs;
  use io;
  use system;
  use llvm;
  use llvm-asm-parser;
  use collection-extensions;

  export llvm-test-suite;
end library;

define module llvm-test-suite
  use common-dylan;
  use testworks;
  use testworks-specs;
  use %testworks, import: { <test>, make-suite };
  use streams;
  use file-system;
  use locators;
  use operating-system;
  use llvm;
  use llvm-builder;
  use llvm-debug;
  use llvm-asm-parser;
  use sequence-diff;

  use format;                   // FIXME
  use standard-io;              // FIXME

  export llvm-test-suite;
end module;
