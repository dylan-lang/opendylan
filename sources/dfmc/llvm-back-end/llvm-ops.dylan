Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2013 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Support

// Cast an integer as an i8 integer constant
define inline function i8
    (value :: <integer>) => (llvm-value :: <llvm-integer-constant>)
  current-back-end().%byte-character-constants[value]
end function;

// Cast an integer as an i32 integer constant
define inline function i32
    (value :: <abstract-integer>) => (llvm-value :: <llvm-integer-constant>)
  make(<llvm-integer-constant>, type: $llvm-i32-type, integer: value)
end function;

// Cast an integer as an i64 integer constant
define inline function i64
    (value :: <abstract-integer>) => (llvm-value :: <llvm-integer-constant>)
  make(<llvm-integer-constant>, type: $llvm-i64-type, integer: value)
end function;

// Get a pointer to the named slot of the given class
define method op--getslotptr
    (be :: <llvm-back-end>, x :: <llvm-value>,
     class :: <&class>, slot-name :: <symbol>, #rest indices)
 => (pointer :: <llvm-value>);
  let slot-descriptor :: <&slot-descriptor>
    = ^slot-descriptor(class, dylan-value(slot-name));
  let header-words = dylan-value(#"$number-header-words");
  let gep = apply(ins--gep-inbounds, be, x, 0,
                  i32(header-words + ^slot-offset(slot-descriptor, class)),
                  indices);
  llvm-value-type(gep);
  gep
end method;

// Same, but with the class name given as a symbol
define method op--getslotptr
    (be :: <llvm-back-end>, x :: <llvm-value>,
     class-name :: <symbol>, slot-name :: <symbol>, #rest indices)
 => (pointer :: <llvm-value>);
  apply(op--getslotptr, be, x, dylan-value(class-name), slot-name,
        indices)
end method;

// Cast a pointer as a pointer to the given pointer's struct type
define method op--object-pointer-cast
    (be :: <llvm-back-end>, x :: <llvm-value>, class :: <&class>)
 => (pointer :: <llvm-value>);
  let class-type = llvm-class-type(be, class);
  ins--bitcast(be, x, llvm-pointer-to(be, class-type))
end method;

// Same, but with the class name given as a symbol
define method op--object-pointer-cast
    (be :: <llvm-back-end>, x :: <llvm-value>, class-name :: <symbol>)
 => (pointer :: <llvm-value>);
  op--object-pointer-cast(be, x, dylan-value(class-name))
end method;

// Cast a pointer to <raw-pointer> type
define function op--raw-pointer-cast
    (be :: <llvm-back-end>, x :: <llvm-value>)
 => (pointer :: <llvm-value>);
  ins--bitcast(be, x, llvm-reference-type(be, dylan-value(#"<raw-pointer>")))
end function;

// Tag a raw value (known to fit) as an integer)
define method op--tag-integer
    (be :: <llvm-back-end>, integer-value :: <llvm-value>)
 => (result :: <llvm-value>);
  let shifted = ins--shl(be, integer-value, $dylan-tag-bits);
  let tagged = ins--or(be, shifted, $dylan-tag-integer);
  ins--inttoptr(be, tagged, $llvm-object-pointer-type)
end method;

// Extract an integer value from an integer-tagged object reference
define method op--untag-integer
    (be :: <llvm-back-end>, x :: <llvm-value>)
 => (integer-value :: <llvm-value>);
  let type = llvm-reference-type(be, dylan-value(#"<raw-integer>"));
  let word = ins--ptrtoint(be, x, type);
  ins--ashr(be, word, $dylan-tag-bits)
end method;

define method op--untag-character
    (be :: <llvm-back-end>, x :: <llvm-value>)
 => (integer-value :: <llvm-value>);
  let type = llvm-reference-type(be, dylan-value(#"<raw-integer>"));
  let word = ins--ptrtoint(be, x, type);
  ins--ashr(be, word, $dylan-tag-bits)
end method;

// Return #t or #f for an i1 value (such as an icmp/fcmp result)
define function op--boolean
    (be :: <llvm-back-end>, x :: <llvm-value>)
 => (boolean :: <llvm-value>);
  let module = be.llvm-builder-module;
  ins--select(be, x,
              emit-reference(be, module, &true),
              emit-reference(be, module, &false))
end function;

// Round a byte count up to the next full word
define method op--round-up-to-word
    (be :: <llvm-back-end>, val) => (aligned-val :: <llvm-value>);
  let word-size = back-end-word-size(be);
  let add = ins--add(be, val, word-size - 1);
  ins--and(be, add, -word-size)
end method;

// Stack allocate a vector
define method op--stack-allocate-vector
    (back-end :: <llvm-back-end>, count)
 => (new-vector :: <llvm-value>);
  let module = back-end.llvm-builder-module;
  let header-words = dylan-value(#"$number-header-words");

  let class :: <&class> = dylan-value(#"<simple-object-vector>");

  let instance-bytes = instance-storage-bytes(back-end, class);
  let repeated-bytes
    = ins--mul(back-end, count,
               slot-storage-bytes(back-end, dylan-value(#"<object>")));
  let byte-size = ins--add(back-end, instance-bytes, repeated-bytes);
  let new-vector = ins--alloca(back-end, $llvm-i8-type, byte-size,
                               alignment: back-end-word-size(back-end));

  // Initialize the wrapper and size slots
  let vector-cast = op--object-pointer-cast(back-end, new-vector, class);
  let wrapper-slot-ptr = ins--gep-inbounds(back-end, vector-cast, 0, i32(0));
  let wrapper-name = emit-name(back-end, module, ^class-mm-wrapper(class));
  let wrapper = llvm-builder-global(back-end, wrapper-name);
  ins--store(back-end, wrapper, wrapper-slot-ptr);

  let size-slot-ptr = op--getslotptr(back-end, vector-cast, class, #"size");
  let size-ref = op--tag-integer(back-end, count);
  ins--store(back-end, size-ref, size-slot-ptr);

  new-vector
end method;


/// Overflow trap

define method op--overflow-trap
    (be :: <llvm-back-end>) => ();
  error("op--overflow-trap not implemented for this platform");
end method;

define method op--overflow-trap
    (be :: <llvm-x86-back-end>) => ();
  ins--call-intrinsic(be, "llvm.x86.int", vector(i8(4)));
end method;
