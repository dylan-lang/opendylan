module:    pentium-harp
Synopsis:  The Pentium HARP instruction set
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// The pentium instruction set


define instruction-set <pentium-instruction-set>
     (<instruction-set>, <pentium-back-end>)
  
  create pentium-instructions, inheriting: default-instructions;

  uuuu  op st-index, fst-index, dst-index, stb-index, sth-index, bits-mem, bitc-mem,
           and2-byte-mem, or2-byte-mem;

  uuuu  op st-index-scaled, sth-index-scaled, fst-index-scaled, dst-index-scaled, dst-index-scale-2;

  tuuu  op bne-byte-mem, beq-byte-mem;

  tuu   op bz-bytes, bnz-bytes;

  duuu  op ld-index, fld-index, dld-index, ldb-index, ldh-index, 
           ldb-index-signed, ldh-index-signed, eliminatable: #t;

  duuu  op ld-index-scaled, ldh-index-scaled, ldh-index-scaled-signed, 
           fld-index-scaled, dld-index-scaled, dld-index-scale-2, eliminatable: #t;

  duu   op move-lower-byte, asr-unsafe, eliminatable: #t;

  du    op move-arg-count-byte, eliminatable: #t;

  du    op load-stack-index;

  uu    op pop-mem, push-mem, flag: #t;

  t     op offset-to-tag;

  none  op clear-direction-flag, push-last-arg;

end;


define sealed inline method instructions (be :: <pentium-back-end>)
 => (instruction-set :: <pentium-instruction-set>)
  pentium-instructions
end method;

mark-reverse-ops (pentium-instructions)
  beq-byte <-> bne-byte;
  blt-byte <-> bge-byte;
  bgt-byte <-> ble-byte;
  bz-bytes <-> bnz-bytes;
end mark-reverse-ops;

