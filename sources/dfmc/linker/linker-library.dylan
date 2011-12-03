module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-linker
  use dylan;
  use dfmc-core;
  use dfmc-back-end;
  export dfmc-linker;
end library;

define module dfmc-linker
  use dylan;
  use dfmc-core;
  use dfmc-back-end;
  use dfmc-imports;
  export 
    <linker>,
    emit-library-record,
    emit-library-records,
    emit-gluefile,
    emit-mainfile,
    emit-glue;

  export
    link-and-download,
    download-for-interactive-execution;
end module;

