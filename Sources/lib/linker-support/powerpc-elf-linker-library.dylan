Module:    dylan-user
Synopsis:  Support for "linking" via PowerPC ELF linker
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library powerpc-elf-linker
  use functional-dylan;
  use system;
  use io;
  use build-system;
  use elf-linker;
  export powerpc-elf-linker;
end library;

define module powerpc-elf-linker
  use functional-dylan;
  use threads;
  use operating-system;
  use file-system;
  use streams;
  use format;
  use locators;
  use build-system;
  use path-utilities;
  use elf-linker;
  export
    <powerpc-elf-linker>;
end module;
