module:    dylan-user
Synopsis:  A disassembler for 386 code
Author:    Jon Thackray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library disasm
  use common-dylan;
  use dylan;
  use collections;
  use io;
  use generic-arithmetic;
  use big-integers;

  export disasm;
end library;

define module disasm
  use generic-arithmetic-dylan;
  use common-extensions, exclude: { format-to-string };
  use dylan-extensions, import: { <byte-character> };
  use byte-vector;
  use format;
  use format-out;

  export decode-opcodes,
	 opcodes-to-string,
	 <general-opcode-and-offsets>,
         <external>,
         <no-external>,
         <some-external>,
         <labelled-external>,
         <relative-external>,
	 general-opcode-opcode,
	 general-opcode-offset,
	 general-opcode-end-offset;
end module;
