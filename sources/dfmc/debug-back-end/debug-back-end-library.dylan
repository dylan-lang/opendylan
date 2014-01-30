module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-debug-back-end
  use dylan;
  use dfmc-core;
  use dfmc-optimization;
  use dfmc-back-end;
  use dfmc-reader;
  use dfmc-conversion;
  use dfmc-typist;
  export dfmc-debug-back-end;
end library;

define module dfmc-debug-back-end
  use dylan;
  use dfmc-core;
  use dfmc-imports;
  use dfmc-optimization;
  use dfmc-back-end;
  use dfmc-reader;
  use dfmc-conversion;
  use dfmc-typist;

  export
    *print-method-bodies?*, print-specializers,
    structured-output, get-structured-output;
end module;
