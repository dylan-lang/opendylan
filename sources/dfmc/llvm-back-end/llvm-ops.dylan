Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Support

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
  apply(ins--gep-inbounds, be, x, 0,
        i32(header-words + ^slot-offset(slot-descriptor, class)),
        indices)
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


/// Overflow trap

define method op--overflow-trap
    (be :: <llvm-back-end>) => ();
  error("op--overflow-trap not implemented for this platform");
end method;

define method op--overflow-trap
    (be :: <llvm-x86-back-end>) => ();
  ins--call-intrinsic(be, "llvm.x86.int",
                      vector(be.%byte-character-constants[4]));
end method;
