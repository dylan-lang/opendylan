Module:       dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-llvm-back-end
  use functional-dylan;
  use io;
  use dfmc-core;
  use dfmc-conversion;
  use dfmc-back-end;
  use llvm;

  export dfmc-llvm-back-end;
end library;

define module dfmc-llvm-back-end
  use functional-dylan;
  use dfmc-core;
  use dfmc-conversion;
  use dfmc-imports;
  use dfmc-back-end;

  use llvm;
  use llvm-builder;

  export
    <llvm-back-end>,
    llvm-back-end-target-triple,
    llvm-back-end-data-layout;
end module;
