module:    dylan-user
Synopsis:  The module definition for the PENTIUM-HARP module
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define module pentium-harp
  use functional-dylan;
  use dylan-extensions,  import: {<abstract-integer>, <simple-integer-vector>};
  use big-integers, prefix: "generic-";
  use streams;
  use streams-internals, import: {<byte-file-stream>};
  use file-system;
  use file-system-internals;
  use format;
  use format-out;
  use print;
  use dfmc-back-end-protocol;
  use native-harp, export: all;
  use native-harp-for-extenders;
  use harp-coff, 
    exclude: { big-endian? };
  use gnu-outputter;

  export

    // Pentium instructions

    <pentium-back-end>,
    <pentium-windows-back-end>,
    <pentium-linux-back-end>,
    ins--st-index, ins--stb-index, ins--sth-index,
    ins--st-index-scaled, ins--sth-index-scaled, 
    ins--fst-index, ins--dst-index,
    ins--fst-index-scaled, ins--dst-index-scaled, ins--dst-index-scale-2,
    ins--bits-mem, ins--bitc-mem, 
    ins--and2-byte-mem, ins--or2-byte-mem,
    ins--beq-byte-mem, ins--bne-byte-mem, 
    ins--bz-bytes, ins--bnz-bytes,
    ins--ld-index, ins--ldb-index, ins--ldh-index,
    ins--ldb-index-signed, ins--ldh-index-signed,
    ins--ld-index-scaled, ins--ldh-index-scaled, 
    ins--ldh-index-scaled-signed, 
    ins--fld-index, ins--dld-index,
    ins--fld-index-scaled, ins--dld-index-scaled, ins--dld-index-scale-2,
    ins--move-lower-byte, ins--asr-unsafe,
    ins--move-arg-count-byte, ins--load-stack-index,
    ins--offset-to-tag, 
    ins--clear-direction-flag, ins--push-last-arg,


    // registers
    eax, ecx, edx, ebx, esp, ebp, esi, edi,
    reg--tmp1, reg--tmp2, reg--tmp3, 
    reg--stack, reg--frame, reg--arg0, 
    reg--mlist, reg--function

    ;

end module;


define module pentium-harp-test
  use dylan;
  use streams;
  use format;
  use print;
  use native-harp-for-extenders;
  use harp-coff, 
    exclude: { big-endian? };
  use pentium-harp;
  use source-records, 
    exclude: { source-record-start-line, source-record-end-line };

  export
    run-test,
    test0,
    test1,
    test2,
    test3,
    test4,
    test5,
    test6,
    test7,
    test8,
    file-test-1,
    file-test-2,
    file-test-3,
    file-test-4,
    file-test-5,
    defasm-test0;
end module;
    
