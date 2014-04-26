module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-c-linker
  use common-dylan;
  use system;
  use dfmc-core;
  use dfmc-conversion;
  use dfmc-back-end;
  use dfmc-c-back-end;
  use dfmc-linker;
  use dfmc-management;
  use release-info;

  export dfmc-c-linker;
end library;

define module dfmc-c-linker
  use common-dylan;
  use dfmc-core;
  use dfmc-imports;
  use dfmc-conversion;
  use dfmc-back-end;
  use dfmc-c-back-end;
  use dfmc-linker;
  use dfmc-management;
  use release-info;
end module;

