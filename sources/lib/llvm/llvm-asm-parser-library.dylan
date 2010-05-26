Module:       Dylan-User
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library llvm-asm-parser
  use dylan;
  use common-dylan;
  use collections;
  use io;
  use parser-run-time;
  use generic-arithmetic;
  use big-integers;

  use llvm;

  export llvm-asm-parser;
end library;

define module llvm-asm-parser
  create
    llvm-asm-parse;
end module;

define module llvm-asm-parser-internals
  use dylan-extensions,
    import: { <double-integer>, %double-integer-low, %double-integer-high,
              decode-single-float, decode-double-float,
              encode-single-float, encode-double-float };
  use common-dylan, exclude: { format-to-string };
  use streams;
  use machine-words;
  use parser-run-time;
  use threads;
  use big-integers,
    prefix: "generic-",
    rename: { <integer> => <abstract-integer> };

  use llvm;
  use llvm-asm-parser;
end module;
