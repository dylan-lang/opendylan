Module: dylan-user
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-harp-cg-linker
  use functional-dylan;

  use dfmc-harp-cg;
  use dfmc-linker;

  export dfmc-harp-cg-linker;
end library;

define module dfmc-harp-cg-linker
  use functional-dylan;
  use machine-word-lowlevel,
    import: { machine-word-unsigned-shift-left, machine-word-unsigned-shift-right };

  use dfmc-harp-cg;
  use dfmc-linker;

  export
    glue-name,
    emit-executable-entry-points,
    emit-shared-library-entry-points,
    emit-glue-data,
    emit-library-initializer,
    emit-library-imported-data,
    main-unit?;
end module;

