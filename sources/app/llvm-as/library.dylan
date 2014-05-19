Module:    dylan-user
Author:    Peter S. Housel
Copyright:    Original Code is Copyright 2009-2018 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library llvm-as
  use common-dylan;
  use io;
  use system;
  use llvm;
  use llvm-asm-parser;
  use command-line-parser;
end library llvm-as;

define module llvm-write-builder
  use common-dylan, exclude: { format-to-string };
  use streams;
  use print;
  use pprint;
  use format;
  use standard-io;
  use file-system;
  use operating-system;

  use llvm;

  export llvm-write-builder;
end module;

define module llvm-as
  use common-dylan, exclude: { format-to-string };
  use streams;
  use pprint;
  use format;
  use standard-io;
  use locators;
  use file-system;
  use operating-system;
  use command-line-parser;

  use llvm;
  use llvm-asm-parser;
  use llvm-write-builder;
end module llvm-as;

