module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-java-back-end
  use common-dylan;
  use io;
  use system;

  use java-vm-code-generation;

  use dfmc-mangling;
  use dfmc-core;
  use dfmc-back-end;
  use dfmc-execution;
  use dfmc-reader;
  use dfmc-conversion;
  use dfmc-typist;
////  use dfmc-file-management;
  use dfmc-linker;
  use dfmc-modeling;
  use dfmc-namespace;
  use dfmc-flow-graph;

  export dfmc-java-back-end;
end library;

define module dfmc-java-back-end
  use common-dylan;
  use threads;
  use streams-internals;
  use streams;
  use date;
  use byte-vector;
//  use machine-word-lowlevel,
//    import: {machine-word-logior,
//             machine-word-logxor,
//             machine-word-logand,
//             machine-word-lognot,
//             machine-word-unsigned-shift-left,
//             machine-word-unsigned-shift-right};

  use java-vm-code-generation;

  use dfmc-mangling;
  use dfmc-core;
  use dfmc-imports;
  use dfmc-back-end;
  use dfmc-execution;
  use dfmc-reader;
  use dfmc-conversion;
  use dfmc-typist;
  use dfmc-linker;
  use dfmc-modeling;
  use dfmc-namespace;
  use dfmc-flow-graph;

  export
    <java-back-end>,
    *java-back-end*,
    java-local-mangle, java-global-mangle, java-raw-mangle,
    *current-be-library*,
    flush-java-classes,
    emit-java-class-for-library,
    finalize-java-linking,
    *current-module-java-class*,
    *current-library-jar*;
end module;


