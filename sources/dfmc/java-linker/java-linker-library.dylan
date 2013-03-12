module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-java-linker
  use functional-dylan;
  use system;

  use java-vm-code-generation ;

  use dfmc-core;
  use dfmc-conversion;
  use dfmc-back-end;
  use dfmc-java-back-end;
  use dfmc-linker;
  use dfmc-execution;
  use dfmc-management;
  use dfmc-namespace;
  export dfmc-java-linker;
end library;

define module dfmc-java-linker
  use functional-dylan;

  use java-vm-code-generation;

  use dfmc-core;
  use dfmc-imports;
  use dfmc-conversion;
  use dfmc-back-end;
  use dfmc-java-back-end;
  use dfmc-linker;
  use dfmc-execution;
  use dfmc-management;
  use dfmc-namespace;
end module;

