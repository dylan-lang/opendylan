module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-c-compiler
  use functional-dylan;
  use dfmc-management;
  use dfmc-execution;
  use dfmc-testing;
  use dfmc-debug-back-end;
  use dfmc-c-back-end;
  use dfmc-c-linker;

  use dfmc-core;
  export dfmc-c-compiler;
end library;

define module dfmc-c-compiler
  use functional-dylan;
  use dfmc-management;
  use dfmc-execution;
  use dfmc-testing;
  use dfmc-debug-back-end;
  use dfmc-c-back-end;
  use dfmc-c-linker;

  use dfmc-core;
  use dfmc-imports;
end module;

