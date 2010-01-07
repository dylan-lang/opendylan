Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library webster
  use functional-dylan;
  use dfmc-core;
  use dfmc-management;
  use dfmc-execution;
  use dfmc-back-end;
  use dfmc-debug-back-end;
  use dfmc-c-back-end;
  use dfmc-c-linker;
  use registry-projects;
  use dfmc-c-ffi;
  use release-info;

  // For now, make debugging stuff available..
  use dfmc-c-file-compiler, import: { dfmc-debug };
  export webster;
end library;

define module webster
  use functional-dylan;
  use dfmc-core;
  use dfmc-imports;
  use dfmc-management;
  use dfmc-execution;
  use dfmc-back-end;
  use dfmc-debug-back-end;
  use dfmc-c-back-end;
  use dfmc-c-linker;
  use registry-projects;
  use dfmc-c-ffi;
end module;
