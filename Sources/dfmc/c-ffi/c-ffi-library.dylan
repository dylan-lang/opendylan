Module:   dylan-user
Synopsis: Library and module definitions for c-ffi in compiler
Author:   Peter Benson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library dfmc-c-ffi
  use functional-dylan;
  use dfmc-core;
  use dfmc-reader;
  use dfmc-macro-expander;
  use dfmc-definitions;
  use dfmc-conversion;
  export dfmc-c-ffi;
end library;


define module dfmc-c-ffi
  use functional-dylan;
  use dfmc-core;
  use dfmc-imports;
  use dfmc-reader;
  use dfmc-macro-expander;
  use dfmc-definitions;
  use dfmc-conversion;

  export
    <&designator-class>,
      ^concrete-pointer-type, ^abstract-pointer-type,
      ^referenced-type, ^concrete-class,
      ^mapped-import-type, ^mapped-export-type, 
      ^boxer-function-name, ^unboxer-function-name,
      ^import-function, ^export-function;

  export
    concrete-class-name, pointer-type-name;

end;

// eof
