Module:       Dylan-User
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library llvm
  use dylan;
  use common-dylan;
  use collections;
  use io;
  use system;
  use parser-run-time;
  use generic-arithmetic;
  use big-integers;

  export llvm;
end library;

define module llvm
  create
    llvm-asm-parse,
    <llvm-module>,
    llvm-module-name,
    llvm-module-target-triple,
    llvm-module-target-triple-setter,
    llvm-module-data-layout,
    llvm-module-data-layout-setter,
    llvm-module-asm,
    llvm-module-asm-setter,

    llvm-save-bitcode-file;
end module;

define module llvm-internals
  use dylan-extensions,
    import: { <double-integer>, %double-integer-low, %double-integer-high,
              decode-single-float, decode-double-float,
              encode-single-float, encode-double-float };
  use common-dylan, exclude: { format-to-string };
  use streams;
  use file-system;
  use locators;
  use machine-words;
  use byte-vector;
  use parser-run-time;
  use operating-system;
  use threads;
  use big-integers,
    prefix: "generic-",
    rename: { <integer> => <abstract-integer> };

  use standard-io;              // FIXME
  use format;                   // FIXME

  use llvm;
end module;
