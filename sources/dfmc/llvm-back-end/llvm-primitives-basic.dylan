Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Machine

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-word-size
    () => (word-size :: <raw-integer>);
  llvm-builder-value(be, back-end-word-size(be))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-header-size
    () => (header-size :: <raw-integer>);
  let header-words = dylan-value(#"$number-header-words");
  llvm-builder-value(be, header-words * back-end-word-size(be))
end;


/// Pointers

define side-effect-free stateless indefinite-extent &primitive-descriptor primitive-cast-pointer-as-raw
    (x :: <raw-pointer>) => (z :: <raw-address>)
  ins--ptrtoint(be, x, llvm-reference-type(be, dylan-value(#"<raw-address>")))
end;

define side-effect-free stateless indefinite-extent &primitive-descriptor primitive-cast-raw-as-pointer
    (x :: <raw-address>) => (z :: <raw-pointer>)
  ins--inttoptr(be, x, llvm-reference-type(be, dylan-value(#"<raw-pointer>")))
end;


/// Comparisons

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-raw-as-boolean
    (x :: <raw-boolean>) => (true? :: <boolean>)
  let cmp = ins--icmp-ne(be, x, 0);
  op--boolean(be, cmp)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-boolean-as-raw
    (x) => (true? :: <raw-boolean>)
  let module = be.llvm-builder-module;
  let cmp = ins--icmp-ne(be, x, emit-reference(be, module, &false));
  ins--zext(be, cmp, llvm-reference-type(be, dylan-value(#"<raw-boolean>")))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-as-boolean
    (x) => (true? :: <boolean>)
  let module = be.llvm-builder-module;
  let cmp = ins--icmp-ne(be, x, emit-reference(be, module, &false));
  op--boolean(be, cmp)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-not
    (x :: <object>) => (not-x :: <boolean>)
  let module = be.llvm-builder-module;
  let cmp = ins--icmp-eq(be, x, emit-reference(be, module, &false));
  op--boolean(be, cmp)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-id?
    (x :: <object>, y :: <object>) => (id? :: <boolean>)
  let cmp = ins--icmp-eq(be, x, y);
  op--boolean(be, cmp)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-not-id?
    (x :: <object>, y :: <object>) => (not-id? :: <boolean>);
  let cmp = ins--icmp-ne(be, x, y);
  op--boolean(be, cmp)
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-compare-bytes
    (base1 :: <raw-pointer>, offset1 :: <raw-integer>,
     base2 :: <raw-pointer>, offset2 :: <raw-integer>,
     size-in-bytes :: <raw-integer>)
 => (same? :: <boolean>)
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-compare-words
    (base1 :: <raw-pointer>, offset1 :: <raw-integer>,
     base2 :: <raw-pointer>, offset2 :: <raw-integer>,
     size-in-words :: <raw-integer>)
 => (same? :: <boolean>)
  //---*** Fill this in...
end;


/// Accessors

// Compute a byte pointer based on object + word offset + byte offset
define function op--byte-element-ptr
    (be :: <llvm-back-end>, x, offset, byte-offset)
 => (ptr :: <llvm-value>);
  let word-size = back-end-word-size(be);

  let x-ptr = ins--bitcast(be, x, $llvm-i8*-type);
  let offset-scaled = ins--mul(be, offset, word-size);
  let offset = ins--add(be, offset-scaled, offset);
  ins--gep(be, x-ptr, offset)
end function;

define side-effect-free dynamic-extent &unimplemented-primitive-descriptor primitive-element
    (x :: <object>, offset :: <raw-integer>, byte-offset :: <raw-integer>)
 => (obj :: <object>);
  //---*** Fill this in...
end;

define side-effecting stateless dynamic-extent &unimplemented-primitive-descriptor primitive-element-setter
    (new-value :: <object>,
     x :: <object>, offset :: <raw-integer>, byte-offset :: <raw-integer>)
 => (obj :: <object>);
  //---*** Fill this in...
end;

define side-effect-free dynamic-extent &primitive-descriptor primitive-byte-element
    (x :: <object>, offset :: <raw-integer>, byte-offset :: <raw-integer>)
 => (obj :: <raw-byte-character>);
  let ptr = op--byte-element-ptr(be, x, offset, byte-offset);
  ins--load(be, ptr)
end;

define side-effecting stateless dynamic-extent &primitive-descriptor primitive-byte-element-setter
    (new-value :: <raw-byte-character>,
     x :: <object>, offset :: <raw-integer>, byte-offset :: <raw-integer>)
 => (obj :: <raw-byte-character>);
  let ptr = op--byte-element-ptr(be, x, offset, byte-offset);
  ins--store(be, new-value, ptr)
end;

/*
define side-effect-free dynamic-extent &primitive-descriptor primitive-bit-element
    (x :: <object>, word-offset :: <raw-integer>, byte-offset :: <raw-integer>,
     bit-offset :: <raw-integer>)
 => (obj :: <raw-integer>);
  //---*** Fill this in...
end;

define side-effecting stateless dynamic-extent &primitive-descriptor primitive-bit-element-setter
    (new-value :: <raw-integer>,
     x :: <object>, offset :: <raw-integer>, byte-offset :: <raw-integer>,
     bit-offset :: <raw-integer>)
 => (obj :: <raw-integer>);
  //---*** Fill this in...
end;
*/

define side-effect-free dynamic-extent &unimplemented-primitive-descriptor primitive-bit-field
    (pointer :: <raw-pointer>,
     bit-offset :: <raw-integer>,
     bit-size :: <raw-integer>)
 => (field :: <raw-integer>);
  //---*** Fill this in...
end;

define side-effecting stateless dynamic-extent &unimplemented-primitive-descriptor primitive-bit-field-setter
    (new-field :: <raw-integer>,
     pointer :: <raw-pointer>,
     bit-offset :: <raw-integer>,
     bit-size :: <raw-integer>)
 => (new-field :: <raw-integer>);
  //---*** Fill this in...
end;

define side-effecting stateless dynamic-extent &unimplemented-primitive-descriptor primitive-fill!
    (dst :: <object>, base-offset :: <raw-integer>, offset :: <raw-integer>, size :: <raw-integer>,
     value :: <object>)
 => ();
  //---*** Fill this in...
end;

define side-effecting stateless dynamic-extent &primitive-descriptor primitive-fill-bytes!
    (dst :: <object>, base-offset :: <raw-integer>, offset :: <raw-integer>, size :: <raw-integer>,
     value :: <raw-byte-character>)
 => ();
  let dst-ptr = op--byte-element-ptr(be, dst, base-offset, offset);
  ins--call-intrinsic(be, "llvm.memset",
                      vector(dst-ptr, value, size, i32(0), $llvm-false));
end;

define side-effecting stateless dynamic-extent &unimplemented-primitive-descriptor primitive-replace!
    (dst :: <object>, dst-base-offset :: <raw-integer>, dst-offset :: <raw-integer>,
     src :: <object>, src-base-offset :: <raw-integer>, src-offset :: <raw-integer>,
     size :: <raw-integer>)
 => ();
  //---*** Fill this in...
end;

define side-effecting stateless dynamic-extent &primitive-descriptor primitive-replace-bytes!
    (dst :: <object>, dst-base-offset :: <raw-integer>, dst-offset :: <raw-integer>,
     src :: <object>, src-base-offset :: <raw-integer>, src-offset :: <raw-integer>,
     size :: <raw-integer>)
 => ();
  let dst-ptr = op--byte-element-ptr(be, dst, dst-base-offset, dst-offset);
  let src-ptr = op--byte-element-ptr(be, src, src-base-offset, src-offset);
  ins--call-intrinsic(be, "llvm.memcpy",
                      vector(dst-ptr, src-ptr, size, i32(0), $llvm-false));
end;


/// Unicode Characters

// TODO: NEED UNICODE SUPPORT IN COMPILER's RUNTIME
define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-unicode-character-as-raw
    (x :: <unicode-character>) => (r :: <raw-integer>);
  let raw-integer-type
    = llvm-reference-type(be, dylan-value(#"<raw-integer>"));
  let bits = ins--ptrtoint(be, x, raw-integer-type);
  ins--lshr(be, bits, $dylan-tag-bits)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-raw-as-unicode-character
     (r :: <raw-integer>) => (x :: <unicode-character>);
  let shifted = ins--shl(be, r, $dylan-tag-bits);
  let tagged = ins--or(be, shifted, $dylan-tag-unichar);
  ins--inttoptr(be, tagged, $llvm-object-pointer-type);
end;


/// Byte Characters

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-byte-character-as-raw
    (x :: <byte-character>) => (r :: <raw-integer>);
  let raw-integer-type
    = llvm-reference-type(be, dylan-value(#"<raw-integer>"));
  let bits = ins--ptrtoint(be, x, raw-integer-type);
  ins--lshr(be, bits, $dylan-tag-bits)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-raw-as-byte-character
     (r :: <raw-integer>) => (x :: <byte-character>);
  let shifted = ins--shl(be, r, $dylan-tag-bits);
  let tagged = ins--or(be, shifted, $dylan-tag-character);
  ins--inttoptr(be, tagged, $llvm-object-pointer-type);
end;


/// String

// See Hacker's Delight, pp. 91-96
// and http://hackersdelight.org/revisions.pdf page 29
//
define method op--word-contains-zero-byte?
    (be :: <llvm-back-end>, v :: <llvm-value>)
 => (contains-zero-byte? :: <llvm-value>);
  let word-size = back-end-word-size(be);
  let iWord-type = be.%type-table["iWord"];
  for (byte from 0 below word-size,
       sub-value = 0 then generic/logior(generic/ash(sub-value,  8), #x01))
  finally
    let mask-value
      = make(<llvm-binop-constant>, operator: #"SHL",
             operands: vector(llvm-builder-value(be, sub-value),
                              llvm-builder-value(be, 7))); // #x80808080...
    let sub    = ins--sub(be, v, sub-value);
    let masked = ins--and(be, v, mask-value);
    let neg    = ins--xor(be, masked, mask-value);
    let and    = ins--and(be, sub, neg);
    ins--icmp-ne(be, and, 0)
  end for
end method;

define side-effect-free stateless dynamic-extent &runtime-primitive-descriptor primitive-strlen
    (x :: <raw-byte-string>) => (size :: <raw-integer>)
  let word-size = back-end-word-size(be);
  let iWord-type = be.%type-table["iWord"];
  let iWord*-type = llvm-pointer-to(be, iWord-type);

  // Basic blocks
  let first-word-loop-init = make(<llvm-basic-block>);
  let first-word-loop-head = make(<llvm-basic-block>);
  let first-word-loop-body = make(<llvm-basic-block>);
  let first-word-loop-inc  = make(<llvm-basic-block>);
  let main-loop-head       = make(<llvm-basic-block>);
  let main-loop            = make(<llvm-basic-block>);
  let main-loop-inc        = make(<llvm-basic-block>);
  let last-word-loop-init  = make(<llvm-basic-block>);
  let last-word-loop-body  = make(<llvm-basic-block>);
  let last-word-loop-inc   = make(<llvm-basic-block>);
  let return               = make(<llvm-basic-block>);

  // Round the start address down to a word boundary
  let as-word = ins--ptrtoint(be, x, iWord-type);
  let masked  = ins--and(be, as-word, -word-size);
  let lp0     = ins--inttoptr(be, masked, iWord*-type);
  let lp1     = ins--gep(be, lp0, 1);

  // Read the first word (which, if the string was not word-aligned,
  // may not all be part of the string) and see if it contains a zero
  // byte
  let first-word = ins--load(be, lp0, alignment: word-size);
  let first-word-contains-zero? = op--word-contains-zero-byte?(be, first-word);
  ins--br(be, first-word-contains-zero?, first-word-loop-init, main-loop-head);

  // It does; loop to search for a zero in the first word
  ins--block(be, first-word-loop-init);
  let limit = ins--bitcast(be, lp1, $llvm-i8*-type);
  ins--br(be, first-word-loop-head);

  // If the loop reaches the end of the first word, continue to the
  // main loop
  ins--block(be, first-word-loop-head);
  let p-placeholder
    = make(<llvm-symbolic-value>, type: $llvm-i8*-type, name: "p");
  let first-word-p = ins--phi(be, x, first-word-loop-init,
                                  p-placeholder, first-word-loop-inc);
  let done? = ins--icmp-eq(be, first-word-p, limit);
  ins--br(be, done?, main-loop-head, first-word-loop-body);

  // Check for zero bytes
  ins--block(be, first-word-loop-body);
  let byte = ins--load(be, first-word-p, type: $llvm-i8-type);
  let zero-byte = be.%byte-character-constants[0];
  let found? = ins--icmp-eq(be, byte, zero-byte);
  ins--br(be, found?, return, first-word-loop-inc);

  // Not found yet, increment the loop variable and continue
  ins--block(be, first-word-loop-inc);
  p-placeholder.llvm-placeholder-value-forward
    := ins--gep(be, first-word-p, 1);
  ins--br(be, first-word-loop-head);

  ins--block(be, main-loop-head);
  ins--br(be, main-loop);

  // Loop, checking one word at a time for words containing zero bytes
  ins--block(be, main-loop);
  let lp-placeholder = make(<llvm-symbolic-value>,
                            type: iWord*-type,
                            name: "lp");
  let lp = ins--phi(be, lp1, main-loop-head,
                        lp-placeholder, main-loop-inc);
  let word = ins--load(be, lp, alignment: word-size);
  let word-contains-zero? = op--word-contains-zero-byte?(be, word);
  ins--br(be, word-contains-zero?, last-word-loop-init, main-loop-inc);

  // Not found yet, increment the loop variable and continue
  ins--block(be, main-loop-inc);
  lp-placeholder.llvm-placeholder-value-forward
    := ins--gep(be, lp, 1);
  ins--br(be, main-loop);

  // Found a zero byte in a word; loop to determine which byte
  ins--block(be, last-word-loop-init);
  let last-word-p-init = ins--bitcast(be, lp, $llvm-i8*-type);
  ins--br(be, last-word-loop-body);

  // Check this byte to see if it is zero
  ins--block(be, last-word-loop-body);
  let last-word-p-placeholder
    = make(<llvm-symbolic-value>, type: $llvm-i8*-type, name: "p");
  let last-word-p = ins--phi(be, last-word-p-init, last-word-loop-init,
                                 last-word-p-placeholder, last-word-loop-inc);
  let byte = ins--load(be, last-word-p, type: $llvm-i8-type);
  let found? = ins--icmp-eq(be, byte, zero-byte);
  ins--br(be, found?, return, last-word-loop-inc);

  // Not found yet, increment the loop variable and continue
  ins--block(be, last-word-loop-inc);
  last-word-p-placeholder.llvm-placeholder-value-forward
    := ins--gep(be, last-word-p, 1);
  ins--br(be, last-word-loop-body);

  // Return the difference between the address of the found zero byte
  // and the start of the string
  ins--block(be, return);
  let p = ins--phi(be, first-word-p, first-word-loop-body,
                       last-word-p, last-word-loop-body);
  let p-as-word = ins--ptrtoint(be, p, iWord-type);
  ins--sub(be, p-as-word, as-word)
end;

define side-effect-free stateless indefinite-extent &primitive-descriptor primitive-string-as-raw
    (x :: <byte-string>) => (r :: <raw-byte-string>);
  op--getslotptr(be, x, #"<byte-string>", #"string-element", 0)
end;

define side-effect-free stateless indefinite-extent &runtime-primitive-descriptor primitive-raw-as-string
    (r :: <raw-byte-string>) => (x :: <byte-string>);
  let word-size = back-end-word-size(be);
  let module = be.llvm-builder-module;
  let class :: <&class> = dylan-value(#"<byte-string>");

  let wrapper = emit-reference(be, module, ^class-mm-wrapper(class));
  let base-size = instance-storage-bytes(be, class);
  let rep-size-slot-descriptor = ^slot-descriptor(class, dylan-value(#"size"));
  let rep-size-slot
    = dylan-value(#"$number-header-words")
    + ^slot-offset(rep-size-slot-descriptor, class);

  // Compute string length + NUL byte + (word-size - 1) to round to
  // word boundary
  let len = call-primitive(be, primitive-strlen-descriptor, r);
  let extra-len = ins--add(be, len, base-size + word-size);
  let number-bytes = ins--and(be, extra-len, -word-size);

  // Allocate memory for storing the string
  let ptr
    = call-primitive(be, primitive-alloc-leaf-r-descriptor,
                     number-bytes, wrapper,
                     len, rep-size-slot);

  // Copy the string (including the NUL)
  let byte-string = op--object-pointer-cast(be, ptr, class);
  let string-element-ptr
    = op--getslotptr(be, byte-string, class, #"string-element", 0);
  let terminated-len = ins--add(be, len, 1);
  ins--call-intrinsic(be, "llvm.memcpy",
                      vector(string-element-ptr, r, terminated-len, i32(0),
                             $llvm-false));

  byte-string
end;

/// Repeated

define side-effect-free stateless indefinite-extent &primitive-descriptor primitive-repeated-slot-as-raw
    (x :: <object>, offset :: <raw-integer>) => (r :: <raw-pointer>);
  // Cast as a pointer to slots and index
  let ptr
    = ins--bitcast(be, x, llvm-pointer-to(be, $llvm-object-pointer-type));
  let slot-ptr
    = ins--gep(be, ptr, offset);
  ins--bitcast(be, slot-ptr,
               llvm-reference-type(be, dylan-value(#"<raw-pointer>")))
end;

define side-effect-free stateless indefinite-extent &primitive-descriptor primitive-repeated-slot-offset
    (x :: <object>) => (r :: <raw-integer>)
  let word-size = back-end-word-size(be);

  // Retrieve the <mm-wrapper> object from the object header
  let wrapper-slot-ptr = ins--gep-inbounds(be, x, 0, i32(0));
  let wrapper = ins--load(be, wrapper-slot-ptr, alignment: word-size);

  // Retrieve the FP (fixed-part) slot from the wrapper
  let fixed-part-slot-ptr
    = op--getslotptr(be, wrapper, #"<mm-wrapper>", #"mm-wrapper-fixed-part");
  let fixed-part
    = ins--load(be, fixed-part-slot-ptr, alignment: word-size);

  // Extract the fixed part length
  let fixed-length = ins--ashr(be, fixed-part, 2);

  // Add the header and size slots
  ins--add(be, fixed-length, dylan-value(#"$number-header-words") + 1)
end;

/// Vector

define side-effect-free stateless indefinite-extent &unimplemented-primitive-descriptor primitive-vector
    (size :: <integer>, #rest arguments) => (value :: <simple-object-vector>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &runtime-primitive-descriptor primitive-copy-vector
    (x :: <simple-object-vector>) => (value :: <simple-object-vector>);
  let word-size = back-end-word-size(be);
  let module = be.llvm-builder-module;
  let class :: <&class> = dylan-value(#"<simple-object-vector>");

  // Basic blocks
  let entry-block = be.llvm-builder-basic-block;
  let return-copied-vector = make(<llvm-basic-block>);
  let return-common = make(<llvm-basic-block>);

  // Read the vector size
  let vector-size = call-primitive(be, primitive-vector-size-descriptor, x);

  // Is it empty?
  let cmp = ins--icmp-eq(be, vector-size, 0);
  ins--br(be, cmp, return-common, return-copied-vector);

  // Determine the vector length in bytes and copy the vector using
  // primitive-copy
  ins--block(be, return-copied-vector);
  let repeated-byte-size = ins--mul(be, vector-size, word-size);
  let number-bytes
    = ins--add(be, repeated-byte-size, instance-storage-bytes(be, class));
  let raw-x = op--raw-pointer-cast(be, x);
  let ptr
    = call-primitive(be, primitive-copy-descriptor, number-bytes, raw-x);
  let copied-vector = op--object-pointer-cast(be, x, class);
  ins--br(be, return-common);

  // Return the canonical empty vector it the input vector was empty,
  // or the copied vector if it was not
  ins--block(be, return-common);
  let empty-vector-name
    = emit-name(be, module, dylan-value(#"%empty-vector"));
  let empty-vector = llvm-builder-global(be, empty-vector-name);
  ins--phi(be, empty-vector, entry-block, copied-vector, return-copied-vector)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-vector-element
    (x :: <simple-object-vector>, index :: <raw-integer>)
 => (value :: <object>);
  let slot-ptr
    = op--getslotptr(be, x, #"<simple-object-vector>", #"vector-element",
                     index);
  ins--load(be, slot-ptr, alignment: back-end-word-size(be))
end;

define side-effecting stateless indefinite-extent &primitive-descriptor primitive-vector-element-setter
    (new-value :: <object>,
     x :: <simple-object-vector>, index :: <raw-integer>)
 => (value :: <object>);
  let slot-ptr
    = op--getslotptr(be, x, #"<simple-object-vector>", #"vector-element",
                     index);
  let v = ins--bitcast(be, new-value, $llvm-object-pointer-type);
  ins--store(be, v, slot-ptr, alignment: back-end-word-size(be));
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-vector-size
    (x :: <simple-object-vector>) => (size :: <raw-integer>);
  let size-slot-ptr
    = op--getslotptr(be, x, #"<simple-object-vector>", #"size");
  let tagged-vector-size
    = ins--load(be, size-slot-ptr, alignment: back-end-word-size(be));
  op--untag-integer(be, tagged-vector-size)
end;

define side-effect-free stateless indefinite-extent &primitive-descriptor primitive-vector-as-raw
    (x :: <simple-object-vector>) => (r :: <raw-pointer>);
  let repeated-slot-ptr
    = op--getslotptr(be, x, #"<simple-object-vector>", #"vector-element");
  op--raw-pointer-cast(be, repeated-slot-ptr)
end;

/*
define side-effect-free stateless indefinite-extent &primitive-descriptor primitive-raw-as-vector
    (n :: <raw-integer>, r :: <raw-pointer>) => (x :: <simple-object-vector>);
  //---*** Fill this in...
end;
*/


/// Instance

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-object-class
    (x :: <object>) => (c :: <class>);
  let word-size = back-end-word-size(be);

  // Retrieve the <mm-wrapper> object from the object header
  let wrapper-slot-ptr = ins--gep-inbounds(be, x, 0, i32(0));
  let wrapper = ins--load(be, wrapper-slot-ptr, alignment: word-size);

  // Retrieve the <implementation-class> object from the wrapper
  let iclass-slot-ptr
    = op--getslotptr(be, wrapper, #"<mm-wrapper>",
                     #"mm-wrapper-implementation-class");
  let iclass
    = ins--load(be, iclass-slot-ptr, alignment: word-size);
  let iclass-cast
    = op--object-pointer-cast(be, iclass, #"<implementation-class>");

  // Retrieve the <class> object from the implementation class object
  let class-slot-ptr
    = op--getslotptr(be, iclass-cast, #"<implementation-class>",
                     #"iclass-class");
  let class = ins--load(be, class-slot-ptr, alignment: word-size);
  op--object-pointer-cast(be, class, #"<class>")
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-slot-value
    (x :: <object>, position :: <raw-integer>) => (value :: <object>)
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-initialized-slot-value
    (x :: <object>, position :: <raw-integer>) => (value :: <object>);
  let word-size = back-end-word-size(be);
  let x-slots
    = ins--bitcast(be, x, llvm-pointer-to(be, $llvm-object-pointer-type));
  let x-body = ins--gep-inbounds(be, x, dylan-value(#"$number-header-words"));
  let slot-ptr = ins--gep-inbounds(be, x, position);
  ins--load(be, slot-ptr, alignment: word-size)
end;

define side-effecting stateless indefinite-extent &unimplemented-primitive-descriptor primitive-slot-value-setter
    (value :: <object>, x :: <object>, position :: <raw-integer>) => (value :: <object>)
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-repeated-slot-value
    (x :: <object>, base-position :: <raw-integer>, position :: <raw-integer>)
 => (value :: <object>);
  //---*** Fill this in...
end;

define side-effecting stateless indefinite-extent &unimplemented-primitive-descriptor primitive-repeated-slot-value-setter
    (value :: <object>, x :: <object>,
     base-position :: <raw-integer>, position :: <raw-integer>)
 => (value :: <object>);
  //---*** Fill this in...
end;


/// Type Checks

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-instance?
    (x :: <object>, t :: <type>) => (true? :: <boolean>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-type-check
    (x :: <object>, t :: <type>) => (true? :: <boolean>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-range-check
    (i :: <raw-integer>, low :: <raw-integer>, high :: <raw-integer>)
 => (true? :: <boolean>);
  let above = ins--icmp-sge(be, i, low);
  let below = ins--icmp-slt(be, i, high);
  let result = ins--and(be, above, below);
  op--boolean(be, result)
end;


/// Multiple Values

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-values
    (size :: <integer>, #rest arguments) => (#rest values)
  //---*** Fill this in...
end;
