module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-debug-back-end
  use functional-dylan;
  use dfmc-core;
  use dfmc-optimization;
  use dfmc-back-end;
  use dfmc-reader;
  use dfmc-conversion;
  use dfmc-management;
  use dfmc-typist;
  export dfmc-debug-back-end;
end library;

define module dfmc-debug-back-end
  use functional-dylan;
  use dfmc-core;
  use dfmc-imports;
  use dfmc-optimization;
  use dfmc-back-end;
  use dfmc-reader;
  use dfmc-conversion;
  use dfmc-management;
  use dfmc-typist;

  export
    *print-method-bodies?*;
end module;

// eof

