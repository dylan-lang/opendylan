module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-llvm-linker
  use functional-dylan;
  use system;
  use dfmc-core;
  use dfmc-conversion;
  use dfmc-back-end;
  use dfmc-llvm-back-end;
  use dfmc-linker;
  use dfmc-execution;
  use dfmc-management;

  use llvm;

  export dfmc-llvm-linker;
end library;

define module dfmc-llvm-linker
  use functional-dylan;
  use dfmc-core;
  use dfmc-imports;
  use dfmc-conversion;
  use dfmc-back-end;
  use dfmc-llvm-back-end;
  use dfmc-linker;
  use dfmc-execution;
  use dfmc-management;

  use llvm;
  use llvm-builder;
end module;
