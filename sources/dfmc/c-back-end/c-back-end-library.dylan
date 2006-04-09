module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-c-back-end
  use functional-dylan;
  use io;
  use dfmc-core;
  use dfmc-back-end;
  use dfmc-execution;
  use dfmc-reader;
  use dfmc-conversion;
  use dfmc-typist;

  export dfmc-c-back-end;
end library;

define module dfmc-c-back-end
  use functional-dylan;
  use streams-internals;
  use dfmc-core;
  use dfmc-imports;
  use dfmc-back-end;
  use dfmc-execution;
  use dfmc-reader;
  use dfmc-conversion;
  use dfmc-typist;
  export
    // *** emit-all,
    <c-back-end>,
    emit-struct-name,
    emit-struct-definer-name,
    emit-repeated-struct-definer-name,
    emit-struct-field-name,
    emit-type-name,
    $dylan-type-string,
    c-type-name,
    c-repeated-type-name,
    c-local-mangle, c-global-mangle, c-raw-mangle,
    format-emit, format-emit*,
    emit-raw-character-data,
    emit-lambda-interface,
    emit-parameters;
end module;

