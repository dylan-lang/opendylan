module: dfmc-pentium-harp-cg
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// ACCESSORS


define sideways method op--load-index
    (back-end :: <pentium-back-end>, result, base, scaled-index, offset :: <integer>,
     #key tagged?) => ()

  if (tagged?)
    ins--ld-index(back-end, result, base, scaled-index, unset-tag-bit(offset));
  else
    ins--ld-index-scaled(back-end, result, base, scaled-index, offset);
  end if

end method op--load-index;

define sideways method op--load-index
    (back-end :: <pentium-back-end>, result, base, scaled-index, offset,
     #key tagged?) => ()

  ins--ld-index-scaled(back-end, result, op--add(back-end, #f, base, offset), scaled-index, 0);

end method op--load-index;


define sideways method op--store-index
    (back-end :: <pentium-back-end>, result, value, base, scaled-index, offset :: <integer>,
     #key tagged?) => ()

  if (tagged?)
    ins--st-index(back-end, value, base, scaled-index, unset-tag-bit(offset));
  else
    ins--st-index-scaled(back-end, value, base, scaled-index, offset);
  end if;
  ins--move(back-end, result, value);

end method op--store-index;

define sideways method op--store-index
    (back-end :: <pentium-back-end>, result, value, base, scaled-index, offset,
     #key tagged?) => ()

  ins--st-index-scaled(back-end, value, op--add(back-end, #f, base, offset), scaled-index, 0);
  ins--move(back-end, result, value);

end method op--store-index;


define sideways method op--load-byte-index
    (back-end :: <pentium-back-end>, result, base, scaled-index, offset :: <integer>) => ()

  ins--ldb-index(back-end, result, base, scaled-index, offset);

end method op--load-byte-index;

define sideways method op--load-byte-index
    (back-end :: <pentium-back-end>, result, base, scaled-index, offset) => ()

  ins--ldb-index(back-end, result, op--add(back-end, #f, base, offset), scaled-index, 0);

end method op--load-byte-index;


define sideways method op--store-byte-index
    (back-end :: <pentium-back-end>, result, value, base, scaled-index, offset :: <integer>) => ()

  ins--stb-index(back-end, value, base, scaled-index, offset);
  ins--move(back-end, result, value);

end method op--store-byte-index;

define sideways method op--store-byte-index
    (back-end :: <pentium-back-end>, result, value, base, scaled-index, offset) => ()

  ins--stb-index(back-end, value, op--add(back-end, #f, base, offset), scaled-index, 0);
  ins--move(back-end, result, value);

end method op--store-byte-index;


define sideways method op--store-bit-index
    (back-end :: <pentium-back-end>, result, value, base, index, offset, bit) => ()

  select (value)
    0 => 
      ins--bitc-mem(back-end, base, index, offset, bit);
    1 => 
      ins--bits-mem(back-end, base, index, offset, bit);
    otherwise =>
      let set-bit-tag = make-tag(back-end);
      let done-tag = make-tag(back-end);
      ins--bne(back-end, set-bit-tag, value, 0);
      ins--bitc-mem(back-end, base, index, offset, bit);
      ins--bra(back-end, done-tag);      
      ins--tag(back-end, set-bit-tag);
      ins--bits-mem(back-end, base, index, offset, bit);
      ins--tag(back-end, done-tag);
  end select;

  ins--move(back-end, result, value);

end method op--store-bit-index;

define sideways method op--load-signed-byte-index
    (back-end :: <pentium-back-end>, result, base, scaled-index, offset :: <integer>) => ()

  ins--ldb-index-signed(back-end, result, base, scaled-index, offset);

end method op--load-signed-byte-index;

define sideways method op--load-signed-byte-index
    (back-end :: <pentium-back-end>, result, base, scaled-index, offset) => ()

  ins--ldb-index-signed(back-end, result, op--add(back-end, #f, base, offset), scaled-index, 0);

end method op--load-signed-byte-index;


define sideways method op--store-signed-byte-index
    (back-end :: <pentium-back-end>, result, value, base, scaled-index, offset) => ()

  op--store-byte-index(back-end, result, value, base, scaled-index, offset);

end method op--store-signed-byte-index;


define sideways method op--load-half-index
    (back-end :: <pentium-back-end>, result, base, scaled-index, offset :: <integer>) => ()

  ins--ldh-index-scaled(back-end, result, base, scaled-index, offset);

end method op--load-half-index;

define sideways method op--load-half-index
    (back-end :: <pentium-back-end>, result, base, scaled-index, offset) => ()

  ins--ldh-index-scaled(back-end, result, op--add(back-end, #f, base, offset), scaled-index, 0);

end method op--load-half-index;


define sideways method op--store-half-index
    (back-end :: <pentium-back-end>, result, value, base, scaled-index, offset :: <integer>) => ()

  ins--sth-index-scaled(back-end, value, base, scaled-index, offset);
  ins--move(back-end, result, value);

end method op--store-half-index;

define sideways method op--store-half-index
    (back-end :: <pentium-back-end>, result, value, base, scaled-index, offset) => ()

  ins--sth-index-scaled(back-end, value, op--add(back-end, #f, base, offset), scaled-index, 0);
  ins--move(back-end, result, value);

end method op--store-half-index;

define sideways method op--load-signed-half-index
    (back-end :: <pentium-back-end>, result, base, scaled-index, offset :: <integer>) => ()

  ins--ldh-index-scaled-signed(back-end, result, base, scaled-index, offset);

end method op--load-signed-half-index;

define sideways method op--load-signed-half-index
    (back-end :: <pentium-back-end>, result, base, scaled-index, offset) => ()

  ins--ldh-index-scaled-signed(back-end, result, op--add(back-end, #f, base, offset), scaled-index, 0);

end method op--load-signed-half-index;


define sideways method op--store-signed-half-index
    (back-end :: <pentium-back-end>, result, value, base, scaled-index, offset) => ()

  op--store-half-index(back-end, result, value, base, scaled-index, offset);

end method op--store-signed-half-index;


define sideways method op--load-float-index
    (back-end :: <pentium-back-end>, result, base, scaled-index, offset :: <integer>,
     #key tagged?) => ()

  if (tagged?)
    ins--fld-index(back-end, result, base, scaled-index, unset-tag-bit(offset));
  else
    ins--fld-index-scaled(back-end, result, base, scaled-index, offset);
  end if

end method op--load-float-index;

define sideways method op--load-float-index
    (back-end :: <pentium-back-end>, result, base, scaled-index, offset,
     #key tagged?) => ()

  ins--fld-index-scaled(back-end, result, op--add(back-end, #f, base, offset), scaled-index, 0);

end method op--load-float-index;


define sideways method op--store-float-index
    (back-end :: <pentium-back-end>, result, value, base, scaled-index, offset :: <integer>,
     #key tagged?) => ()

  if (tagged?)
    ins--fst-index(back-end, value, base, scaled-index, unset-tag-bit(offset));
  else
    ins--fst-index-scaled(back-end, value, base, scaled-index, offset);
  end if;
  ins--fmove(back-end, result, value);

end method op--store-float-index;

define sideways method op--store-float-index
    (back-end :: <pentium-back-end>, result, value, base, scaled-index, offset,
     #key tagged?) => ()

  ins--fst-index-scaled(back-end, value, op--add(back-end, #f, base, offset), scaled-index, 0);
  ins--fmove(back-end, result, value);

end method op--store-float-index;


define sideways method op--load-dfloat-index
    (back-end :: <pentium-back-end>, result, base, scaled-index, offset :: <integer>,
     #key tagged?) => ()

  if (tagged?)
    ins--dld-index-scale-2(back-end, result, base, scaled-index, offset - 2);
  else
    ins--dld-index-scaled(back-end, result, base, scaled-index, offset);
  end if

end method op--load-dfloat-index;

define sideways method op--load-dfloat-index
    (back-end :: <pentium-back-end>, result, base, scaled-index, offset,
     #key tagged?) => ()

  ins--dld-index-scaled(back-end, result, op--add(back-end, #f, base, offset), scaled-index, 0);

end method op--load-dfloat-index;


define sideways method op--store-dfloat-index
    (back-end :: <pentium-back-end>, result, value, base, scaled-index, offset :: <integer>,
     #key tagged?) => ()

  if (tagged?)
    ins--dst-index-scale-2(back-end, value, base, scaled-index, offset - 2);
  else
    ins--dst-index-scaled(back-end, value, base, scaled-index, offset);
  end if;
  ins--dmove(back-end, result, value);

end method op--store-dfloat-index;

define sideways method op--store-dfloat-index
    (back-end :: <pentium-back-end>, result, value, base, scaled-index, offset,
     #key tagged?) => ()

  ins--dst-index-scaled(back-end, value, op--add(back-end, #f, base, offset), scaled-index, 0);
  ins--dmove(back-end, result, value);

end method op--store-dfloat-index;


define sideways method op--replace-bytes!(back-end :: <pentium-back-end>, result, dst, dst-base-offset, dst-offset, src, src-base-offset, src-offset, size) => ()
  let dst-offset = op--add(back-end, #f, bytes%(back-end, dst-base-offset), dst-offset);
  let src-offset = op--add(back-end, #f, bytes%(back-end, src-base-offset), src-offset);

  ins--copy-bytes-down(back-end,
 		       op--add(back-end, #f, dst, dst-offset),
 		       op--add(back-end, #f, src, src-offset),
		       size);
  ins--move(back-end, result, dst);
end method op--replace-bytes!;


/// THREADS PRIMITIVES


define constant OK = 1;
define constant NOT-LOCKED = 9;

define constant CONTAINER-HANDLE-OFFSET = 4;

define constant SIMPLELOCK-OWNER-OFFSET = 0;
define constant SIMPLELOCK-LOCK-COUNT-OFFSET = 8;

define constant RECURSIVELOCK-OWNER-OFFSET = 0;
define constant RECURSIVELOCK-LOCK-COUNT-OFFSET = 8;
define constant RECURSIVELOCK-RECURSION-COUNT-OFFSET = 12;

define thread variable $tlv-writer-counter-ref = #f;
define c-runtime-reference tlv-writer-counter;

define c-runtime-reference primitive-wait-for-simple-lock-internal;
define c-runtime-reference primitive-wait-for-simple-lock-timed-internal;
define c-runtime-reference primitive-release-simple-lock-internal;
define c-runtime-reference primitive-wait-for-recursive-lock-internal;
define c-runtime-reference primitive-wait-for-recursive-lock-timed-internal;
define c-runtime-reference primitive-release-recursive-lock-internal;
define c-runtime-reference primitive-write-thread-variable-internal;


define sideways method op--wait-for-simple-lock(back-end :: <pentium-back-end>, result, lock, #key zmilsecs) => ()

  let simple-lock = make-n-register(back-end);
  let hthread = make-n-register(back-end);
  let lock-count = make-n-register(back-end);
  let done-tag = make-tag(back-end);
  let tag = make-tag(back-end);

  ins--rem(back-end, "wait for simple lock");
  ins--ld(back-end, simple-lock, lock, CONTAINER-HANDLE-OFFSET);
  ins--ld-teb(back-end, hthread, back-end.teb-current-thread-handle-offset);

  ins--xadd-mem-locked(back-end, lock-count, simple-lock, SIMPLELOCK-LOCK-COUNT-OFFSET, 1);
  ins--beq(back-end, tag, lock-count, 0);
  if (zmilsecs)
    call-c-primitive(back-end, result, $primitive-wait-for-simple-lock-timed-internal, simple-lock, hthread, zmilsecs);
  else
    call-c-primitive(back-end, result, $primitive-wait-for-simple-lock-internal, simple-lock, hthread);
  end if;
  ins--bra(back-end, done-tag);

  ins--tag(back-end, tag);
  ins--st(back-end, hthread, simple-lock, SIMPLELOCK-OWNER-OFFSET);
  ins--move(back-end, result, OK);

  ins--tag(back-end, done-tag);
  
end method op--wait-for-simple-lock;


define sideways method op--wait-for-simple-lock-timed(back-end :: <pentium-back-end>, result, lock, zmilsecs) => ()

  op--wait-for-simple-lock(back-end, result, lock, zmilsecs: zmilsecs);

end method op--wait-for-simple-lock-timed;


define sideways method op--release-simple-lock(back-end :: <pentium-back-end>, result, lock) => ()

  let simple-lock = make-n-register(back-end);
  let hthread = make-n-register(back-end);
  let lock-owner = make-n-register(back-end);
  let lock-count = make-n-register(back-end);
  let done-tag = make-tag(back-end);
  let tag1 = make-tag(back-end);
  let tag2 = make-tag(back-end);

  ins--rem(back-end, "release simple lock");
  ins--ld(back-end, simple-lock, lock, CONTAINER-HANDLE-OFFSET);
  ins--ld-teb(back-end, hthread, back-end.teb-current-thread-handle-offset);

  ins--ld(back-end, lock-owner, simple-lock, SIMPLELOCK-OWNER-OFFSET);
  ins--beq(back-end, tag1, lock-owner, hthread);
  // primitive-print-error-message("release-simple-lock: Error, don't own the lock\n");
  ins--move(back-end, result, NOT-LOCKED);
  ins--bra(back-end, done-tag);

  ins--tag(back-end, tag1);
  ins--st(back-end, 0, simple-lock, SIMPLELOCK-OWNER-OFFSET);
  ins--xadd-mem-locked(back-end, lock-count, simple-lock, SIMPLELOCK-LOCK-COUNT-OFFSET, -1);
  ins--blt(back-end, tag2, lock-count, 0);
  call-c-primitive(back-end, result, $primitive-release-simple-lock-internal, simple-lock);
  ins--bra(back-end, done-tag);

  ins--tag(back-end, tag2);
  ins--move(back-end, result, OK);

  ins--tag(back-end, done-tag);
  
end method op--release-simple-lock;


define sideways method op--wait-for-recursive-lock(back-end :: <pentium-back-end>, result, lock, #key zmilsecs) => ()

  let recursive-lock = make-n-register(back-end);
  let hthread = make-n-register(back-end);
  let lock-owner = make-n-register(back-end);
  let lock-count = make-n-register(back-end);
  let done-tag = make-tag(back-end);
  let tag1 = make-tag(back-end);
  let tag2 = make-tag(back-end);

  ins--rem(back-end, "wait for recursive lock");
  ins--ld(back-end, recursive-lock, lock, CONTAINER-HANDLE-OFFSET);
  ins--ld-teb(back-end, hthread, back-end.teb-current-thread-handle-offset);

  ins--ld(back-end, lock-owner, recursive-lock, RECURSIVELOCK-OWNER-OFFSET);
  ins--beq(back-end, tag1, lock-owner, hthread);
  ins--xadd-mem-locked(back-end, lock-count, recursive-lock, RECURSIVELOCK-LOCK-COUNT-OFFSET, 1);
  ins--beq(back-end, tag2, lock-count, 0);
  if (zmilsecs)
    call-c-primitive(back-end, result, $primitive-wait-for-recursive-lock-timed-internal, recursive-lock, hthread, zmilsecs);
  else
    call-c-primitive(back-end, result, $primitive-wait-for-recursive-lock-internal, recursive-lock, hthread);
  end if;
  ins--bra(back-end, done-tag);

  ins--tag(back-end, tag2);
  ins--st(back-end, hthread, recursive-lock, RECURSIVELOCK-OWNER-OFFSET);
  ins--st(back-end, 1, recursive-lock, RECURSIVELOCK-RECURSION-COUNT-OFFSET);
  ins--move(back-end, result, OK);
  ins--bra(back-end, done-tag);

  ins--tag(back-end, tag1);
  ins--add2-mem(back-end, recursive-lock, RECURSIVELOCK-RECURSION-COUNT-OFFSET, 0, 1);
  ins--move(back-end, result, OK);

  ins--tag(back-end, done-tag);

end method op--wait-for-recursive-lock;

define sideways method op--wait-for-recursive-lock-timed(back-end :: <pentium-back-end>, result, lock, zmilsecs) => ()

  op--wait-for-recursive-lock(back-end, result, lock, zmilsecs: zmilsecs);

end method op--wait-for-recursive-lock-timed;


define sideways method op--release-recursive-lock(back-end :: <pentium-back-end>, result, lock) => ()

  let recursive-lock = make-n-register(back-end);
  let hthread = make-n-register(back-end);
  let lock-owner = make-n-register(back-end);
  let lock-count = make-n-register(back-end);
  let done-tag = make-tag(back-end);
  let tag1 = make-tag(back-end);
  let tag2 = make-tag(back-end);

  ins--rem(back-end, "release recursive lock");
  ins--ld(back-end, recursive-lock, lock, CONTAINER-HANDLE-OFFSET);
  ins--ld-teb(back-end, hthread, back-end.teb-current-thread-handle-offset);

  ins--ld(back-end, lock-owner, recursive-lock, RECURSIVELOCK-OWNER-OFFSET);
  ins--beq(back-end, tag1, lock-owner, hthread);
  // primitive-print-error-message("release-recursive-lock: Error, don't own the lock\n");
  ins--move(back-end, result, NOT-LOCKED);
  ins--bra(back-end, done-tag);

  ins--tag(back-end, tag1);
  ins--ld(back-end, lock-count, recursive-lock, RECURSIVELOCK-RECURSION-COUNT-OFFSET);
  ins--sub(back-end, lock-count, lock-count, 1);
  ins--st(back-end, lock-count, recursive-lock, RECURSIVELOCK-RECURSION-COUNT-OFFSET);
  ins--bne(back-end, tag2, lock-count, 0);
  // Give up the lock
  ins--st(back-end, 0, recursive-lock, RECURSIVELOCK-OWNER-OFFSET);
  ins--xadd-mem-locked(back-end, lock-count, recursive-lock, RECURSIVELOCK-LOCK-COUNT-OFFSET, -1);
  ins--blt(back-end, tag2, lock-count, 0);
  call-c-primitive(back-end, result, $primitive-release-recursive-lock-internal, recursive-lock);
  ins--bra(back-end, done-tag);

  ins--tag(back-end, tag2);
  ins--move(back-end, result, OK);

  ins--tag(back-end, done-tag);
  
end method op--release-recursive-lock;


define sideways method op--write-thread-variable(back-end :: <pentium-back-end>, result, variable-handle, new-value) => ()

  let tlv-vector = make-n-register(back-end);
  let counter = make-n-register(back-end);
  let tlv-counter-addr = make-n-register(back-end);
  let tag = make-tag(back-end);

  ins--rem(back-end, "write thread variable");
  // If another thread is growing the TLV vectors, wait till it's finished
  ins--move(back-end, tlv-counter-addr, $tlv-writer-counter-ref.runtime-reference);
  ins--xadd-mem-locked(back-end, counter, tlv-counter-addr, 0, 1);
  ins--bge(back-end, tag, counter, 0);
  call-c-primitive(back-end, result, $primitive-write-thread-variable-internal);

  ins--tag(back-end, tag);
  // The variable handle is the byte offset where the variable's value is
  // stored in the TLV.
  ins--ld-teb(back-end, tlv-vector, back-end.teb-thread-local-variables-offset);
  ins--st(back-end, new-value, tlv-vector, variable-handle);

  // Indicate that the write has finished
  ins--sub2-mem-locked(back-end, tlv-counter-addr, 0, 0, 1);

  ins--move(back-end, result, new-value);

end method op--write-thread-variable;


define sideways method op--read-thread-variable(back-end :: <pentium-back-end>, result, variable-handle) => ()

  let tlv-vector = make-n-register(back-end);

  ins--rem(back-end, "read thread variable");
  // The variable handle is the byte offset where the variable's value is
  // stored in the TLV.
  ins--ld-teb(back-end, tlv-vector, back-end.teb-thread-local-variables-offset);
  ins--ld(back-end, result, tlv-vector, variable-handle);

end method op--read-thread-variable;

define sideways method op--allocation-count(back-end :: <pentium-back-end>, result) => ()
  ins--ld-teb(back-end, result, -4);
end method;

define sideways method op--initialize-allocation-count(back-end :: <pentium-back-end>, result) => ()
  ins--st-teb(back-end, 0, -4);
end method;
